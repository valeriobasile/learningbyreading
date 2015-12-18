#!/usr/bin/env python
from optparse import OptionParser
from babelfy import babelfy
from candc import tokenize, boxer
import simplejson as json
import logging as log
from os import listdir
from os.path import isfile, join
from itertools import product, combinations
from sys import exit
from framenet import frames
from collections import Counter

# log configuration
log.basicConfig(level=log.INFO)

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
(options, args) = parser.parse_args()

if options.input_file:
    documents = [options.input_file]
else:
    documents = [ join(options.input_dir,f) for f in listdir(options.input_dir) if isfile(join(options.input_dir,f)) ]

triples = list()
for filename in documents:
    # read file
    log.info("opening file {0}".format(filename))
    with open(filename) as f:
        text = f.read()

    # tokenization
    log.info("calling tokenizer")
    tokens = tokenize(text)
    if not tokens:
        log.error("error during tokenization of file '{0}', exiting".format(filename))
        continue
    tokenized = " ".join(tokens)

    # process the text
    log.info("calling Boxer")
    drs = boxer(tokenized)
    if not drs:
        log.error("error during the execution of Boxer on file '{0}', exiting".format(filename))
        continue

    log.info("calling Babelfy")
    babel = babelfy(tokenized)
    if not babel:
        log.error("error during the execution of Babelfy on file '{0}', exiting".format(filename))
        continue

    # extracting co-mentions
    dbpedia_entities = set(map(lambda x: x['entity'], babel['entities']))
    for entity1, entity2 in combinations(dbpedia_entities, 2):
        if (entity1 != 'null' and
            entity2 != 'null'):
            triples.append(('<{0}>'.format(entity1), 'aloof_relation:comention', '<{0}>'.format(entity2)))

    # build dictionary of variables
    try:
        variables = dict()
        for predicate in drs['predicates']:
            if not predicate['variable'] in variables:
                variables[predicate['variable']] = []
            for entity in babel['entities']:
                # baseline alignment
                # TODO: make this smarter
                if predicate['token_start'] == entity['token_start'] and predicate['token_end'] == entity['token_end']:
                    variables[predicate['variable']].append((entity['entity'], entity['bn_url']))
    except:
        log.error("error during the alignment on file '{0}', exiting".format(filename))
        continue

    # scanning relations
    for relation in drs['relations']:
        if (relation['arg1'] in variables and
            relation['arg2'] in variables):
            for event, entity in product(variables[relation['arg1']],
                                         variables[relation['arg2']]):
                if (entity[0] != 'null' and event[1] != ''):
                    # fix ID format wn:00035718r ->  00594989-v
                    bn_id = event[1].split('/')[-1]
                    #wn_offset = bn2offset[bn_id]
                    if bn_id in frames:
                        framelist = frames[bn_id]
                    else:
                        framelist = ['unknown_frame']
                    for frame in framelist:
                        triples.append(('<{0}>'.format(entity[0]), relation['symbol'], 'framenet:{0}'.format(frame)))

with open(options.output_file, "w") as f:
    for triple, frequency in Counter(triples).iteritems():
        # write down n-triples with frequency
        f.write("{0} {1} {2} {3}\n".format(frequency, *triple))
