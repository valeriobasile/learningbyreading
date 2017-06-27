#!/usr/bin/env python
import simplejson as json
import logging as log
import ConfigParser
from optparse import OptionParser
from disambiguation import disambiguation
import candc
import semafor
from os import listdir
from os.path import isfile, join, dirname
from itertools import product, combinations
from collections import Counter
from frameinstance import *
from lxml import objectify

# log configuration
log.basicConfig(level=log.INFO, format='%(asctime)s.%(msecs)03d %(levelname)s %(message)s')

# read configuration
config = ConfigParser.ConfigParser()
config.read(join(dirname(__file__),'../config/namespace.conf'))
config.read(join(dirname(__file__),'../config/disambiguation.conf'))

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
                  default='out',
                  help="write output to FILE",
                  metavar="FILE")
parser.add_option('-c',
                  '--comentions',
                  action="store_true",
                  dest="comentions",
                  help="output co-mentions")
parser.add_option('-t',
                  '--tokenized',
                  action="store_true",
                  dest="tokenized",
                  help="do not tokenize input")
parser.add_option('-f',
                  '--format',
                  dest="format",
                  default='triples',
                  help="format of the output: 'triples' or 'xml'")

(options, args) = parser.parse_args()

if options.input_file:
    documents = [options.input_file]
else:
    documents = [ join(options.input_dir,f) for f in listdir(options.input_dir) if isfile(join(options.input_dir,f)) ]

if not options.output_file:
    output_file = 'out'
else:
    output_file = options.output_file

if (not options.format) or (not options.format in ['triples', 'xml']):
    output_format = 'triples'
else:
    output_format = options.format


triples = list()
frame_instance_triples = list()
root = objectify.Element("frameinstances")
for filename in documents:
    # read file
    log.info("opening file {0}".format(filename))
    with open(filename) as f:
        text = f.read()

    # semantic parsing
    if config.get('semantics', 'module') == 'boxer':
        # tokenization
        if not options.tokenized:
            log.info("Tokenization with t")
            tokens = candc.tokenize(text)
            if not tokens:
                log.error("error during tokenization of file '{0}', exiting".format(filename))
                continue
            tokenized = "\n".join([' '.join(sentence) for sentence in tokens[:-1]])
        else:
            tokenized = text


        log.info("Parsing with Boxer")
        semantics = candc.get_all(tokenized)
        if not semantics:
            log.error("error during the execution of Boxer on file '{0}', exiting".format(filename))
            continue

    elif config.get('semantics', 'module') == 'semafor':
        log.info("Parsing with Semafor")
        semantics, tokenized = semafor.parse(text)
        if not semantics:
            log.error("error during the execution of Semafor on file '{0}', exiting".format(filename))
            continue


    log.info("Word sense disambiguation and entity linking")
    synsets, entities = disambiguation(tokenized, semantics)
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
        for predicate in semantics['predicates']+semantics['namedentities']:
            if not predicate['variable'] in variables:
                variables[predicate['variable']] = []
            for synset in synsets:
                #print synset
                # baseline sysnet alignment
                # TODO: make this smarter
                if predicate['token_start'] <= synset['token_start'] and predicate['token_end'] >= synset['token_end']:
                    if not synset['synset'] in variables[predicate['variable']]:
                        variables[predicate['variable']].append(synset['synset'])
                        print synset['synset']+' is '+ predicate['variable']
            for entity in entities:
                # baseline entity alignment
                # TODO: make this smarter
                if predicate['token_start'] <= entity['token_start'] and predicate['token_end'] >= entity['token_end'] and entity['entity'] != 'null':
                    if not entity['entity'] in variables[predicate['variable']]:
                        variables[predicate['variable']].append(entity['entity'])

    except:
        log.error("error during the alignment on file '{0}', exiting".format(filename))
        continue

    # extract frame instances.
    # this is the core algorithm of KNEWS,
    # the alignment between the semantic parsing and the word sense disambiguation module
    frame_instances = get_frame_instances(variables, semantics, thematic_roles)
    frame_instance_triples.extend(get_frame_triples(frame_instances))

    # use DRG to get aligned frame instances
    if output_format == 'xml':
        aligned_frames_xml = get_aligned_frames_xml(tokenized, frame_instances, root)


log.info('writing output ({0}) on file {1}...'.format(output_format, options.output_file))
with open(output_file, "w") as f:
    if output_format == 'triples':
        for triple in frame_instance_triples:
            f.write("{0} {1} {2} .\n".format(*triple))
    elif output_format == 'xml':
        f.write(aligned_frames_xml)
