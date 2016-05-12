#!/usr/bin/env python
from optparse import OptionParser
from disambiguation import disambiguation
from candc import tokenize, get_all, get_fol, get_drg
import simplejson as json
import logging as log
from os import listdir
from os.path import isfile, join, dirname
from itertools import product, combinations
from sys import exit
from framenet import frames
from collections import Counter
from mappings import bn2offset
import ConfigParser
from unboxer import unboxer, drg
import re

# log configuration
log.basicConfig(level=log.INFO, format='%(asctime)s.%(msecs)03d %(levelname)s %(message)s')

# read configuration
config = ConfigParser.ConfigParser()
config.read(join(dirname(__file__),'../config/namespace.conf'))

with open(join(dirname(__file__),'../resources/thematic_roles.txt')) as f:
    thematic_roles = [line.rstrip() for line in f]
# command line argument partsing
parser = OptionParser()
parser.add_option('-i',
                  '--input',
                  dest="input_file",
                  help="read text from FILE",
                  metavar="FILE")
parser.add_option('-d',
                  '--input-dir',
                  dest="input_dir",
                  help="read text files from DIRECTORY",
                  metavar="DIRECTORY")
parser.add_option('-o',
                  '--output',
                  dest="output_file",
                  help="write JSON to FILE",
                  metavar="FILE")
parser.add_option('-c',
                  '--comentions',
                  action="store_true",
                  dest="comentions",
                  help="output co-mentions")
(options, args) = parser.parse_args()

if options.input_file:
    documents = [options.input_file]
else:
    documents = [ join(options.input_dir,f) for f in listdir(options.input_dir) if isfile(join(options.input_dir,f)) ]

triples = list()
with open(options.output_file, "w") as f:
    pass

for filename in documents:
    # read file
    log.info("opening file {0}".format(filename))
    with open(filename) as f:
        text = f.read()

    # tokenization
    log.info("Tokenization")
    tokens = tokenize(text)
    if not tokens:
        log.error("error during tokenization of file '{0}', exiting".format(filename))
        continue

    tokenized = "\n".join([' '.join(sentence) for sentence in tokens[:-1]])

    log.info("Parsing")
    drs = get_all(tokenized)
    if not drs:
        log.error("error during the execution of Boxer on file '{0}', exiting".format(filename))
        continue
    log.info("Word sense disambiguation and entity linking")
    synsets, entities = disambiguation(tokenized, drs)
    if synsets==None or entities==None:
		log.error("error during the disambiguation of file '{0}', exiting".format(filename))
		continue

    # extracting co-mentions
    if options.comentions:
        dbpedia_entities = set(map(lambda x: x['entity'], entities))
        for entity1, entity2 in combinations(dbpedia_entities, 2):
            if (entity1 != 'null' and
                entity2 != 'null'):
                triples.append(('<{0}>'.format(entity1), '<{0}#comention>', '<{2}>'.format(config.get('namespace', 'relation'), entity2)))

    # build dictionary of variables
    try:
        variables = dict()
        for predicate in drs['predicates']+drs['namedentities']:
            if not predicate['variable'] in variables:
                variables[predicate['variable']] = []
            for synset in synsets:
                # baseline sysnet alignment
                # TODO: make this smarter
                if predicate['token_start'] == synset['token_start'] and predicate['token_end'] == synset['token_end']:
                    if not synset['synset'] in variables[predicate['variable']]:
                        variables[predicate['variable']].append(synset['synset'])
            for entity in entities:
                # baseline entity alignment
                # TODO: make this smarter
                if predicate['token_start'] == entity['token_start'] and predicate['token_end'] == entity['token_end'] and entity['entity'] != 'null':
                    if not entity['entity'] in variables[predicate['variable']]:
                        variables[predicate['variable']].append(entity['entity'])
    except:
        log.error("error during the alignment on file '{0}', exiting".format(filename))
        continue

    # read DRG
    tuples = get_drg(tokenized)
    drgparser = drg.DRGParser()
    d = drgparser.parse_tup_lines(tuples)
    # de-reificate variables (build a mapping)
    reificated = dict()
    for t in d.tuples:
        if t.edge_type == "referent":
            dereificated = re.sub(".*:", "", t.to_node)
            if not dereificated in reificated:
                reificated[dereificated] = set()
            reificated[dereificated].add(t.to_node)
    # scanning relations
    with open(options.output_file, "a") as f:
        for relation in drs['relations']:
            if (relation['arg1'] in variables and
                relation['arg2'] in variables):
                for entity1, entity2 in product(variables[relation['arg1']],
                                             variables[relation['arg2']]):
                    if relation['symbol'] in thematic_roles:
                        if (entity2 != '' and entity1 != ''):
                            triple = ('<{0}>'.format(entity2),
                                      '<{0}#{1}>'.format(config.get('namespace', 'relation'), relation['symbol']),
                                      '<{0}>'.format(entity1))
                            triples.append(triple)
                            f.write("{0} {1} {2} .\n".format(*triple))
                    else:
                        if (entity2 != '' and entity1 != ''):
                            triple = ('<{0}>'.format(entity1),
                                      '<{0}#{1}>'.format(config.get('namespace', 'relation'), relation['symbol']),
                                      '<{0}>'.format(entity2))
                            triples.append(triple)
                            f.write("{0} {1} {2} .\n".format(*triple))

    # get variables surface forms
    surfaceforms = dict()
    for variable in variables.keys():
        try:
            for reificated_var in reificated[variable]:
                surface = []
                unboxer.generate_from_referent(d, reificated_var, surface, generic=True)
                if len(surface) > 0 and ' '.join(surface)!='*':
                    if not variable in surfaceforms:
                        surfaceforms[variable] = []
                        surfaceforms[variable].append(' '.join(surface))
        except:
            log.error('cannot find mapping for variable {0}'.format(variable))

    with open(options.output_file, "a") as f:
        for variable, content in variables.iteritems():
            if variable in surfaceforms:
                for item in content:
                    for surfaceform in surfaceforms[variable]:
                        triple = ('<{0}>'.format(item),
                                  '<{0}#entity>'.format(config.get('namespace', 'lexicalization')),
                                  '"{0}"'.format(surfaceform))
                        triples.append(triple)
                        f.write("{0} {1} {2} .\n".format(*triple))

    # get surface forms for relations
    with open(options.output_file, "a") as f:
        for relation in drs['relations']:
            try:
                for arg1, arg2 in product(reificated[relation['arg1']], reificated[relation['arg2']]):
                    surface = unboxer.generate_from_relation(d, arg1, arg2)
                    if surface:
                        # TODO: implement schema from FrameBase

                        triple = ('<{0}>'.format(relation['symbol']),
                                  '<{0}#relation>'.format(config.get('namespace', 'lexicalization')),
                                  '"{0}"'.format(surface))
                        triples.append(triple)
                        f.write("{0} {1} {2} .\n".format(*triple))
            except:
                log.error('cannot find variable mapping for relation {0} {1} {2}'.format(relation['arg1'], relation['symbol'], relation['arg2']))
