#!/usr/bin/env python
from optparse import OptionParser
from disambiguation import disambiguation
from candc import tokenize, get_all, get_fol
import simplejson as json
import logging as log
from os import listdir
from os.path import isfile, join
from itertools import product, combinations
from sys import exit
from framenet import frames
from collections import Counter
from mappings import bn2offset
import ConfigParser

# log configuration
log.basicConfig(level=log.INFO, format='%(asctime)s.%(msecs)03d %(levelname)s %(message)s')

# read configuration
config = ConfigParser.ConfigParser()
config.read('config/namespace.conf')

with open('resources/thematic_roles.txt') as f:
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
    tokenized = " ".join(tokens)

    # process the text
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
        for predicate in drs['predicates']:
            if not predicate['variable'] in variables:
                variables[predicate['variable']] = []
            for synset in synsets:
                # baseline sysnet alignment
                # TODO: make this smarter
                if predicate['token_start'] == synset['token_start'] and predicate['token_end'] == synset['token_end']:
                    variables[predicate['variable']].append(synset['synset'])
            for entity in entities:
                # baseline entity alignment
                # TODO: make this smarter
                if predicate['token_start'] == entity['token_start'] and predicate['token_end'] == entity['token_end'] and entity['entity'] != 'null':
                    variables[predicate['variable']].append(entity['entity'])
    except:
        log.error("error during the alignment on file '{0}', exiting".format(filename))
        continue

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
                    else:
                        if (entity2 != '' and entity1 != ''):
                            triple = ('<{0}>'.format(entity1),
                                      '<{0}#{1}>'.format(config.get('namespace', 'relation'), relation['symbol']),
                                      '<{0}>'.format(entity2))
                    triples.append(triple)
                    f.write("{0} {1} {2} .\n".format(*triple))

'''
with open(options.output_file, "w") as f:
    for triple, frequency in Counter(triples).iteritems():
        # write down n-triples with frequency
        #f.write("{0} {1} {2} {3}\n".format(frequency, *triple))
        f.write("{0} {1} {2} .\n".format(*triple))
'''
