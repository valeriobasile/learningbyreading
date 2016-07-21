#!/usr/bin/env python
from optparse import OptionParser
from babelfy import babelfy
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
import re
from string import ascii_uppercase
from disambiguation import disambiguation

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
parser.add_option('-t',
                  '--tokenized',
                  action="store_true",
                  dest="tokenized",
                  help="do not tokenize input")
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

for filename in documents:
    # read file
    log.info("opening file {0}".format(filename))
    with open(filename) as f:
        text = f.read()

    # tokenization
    if not options.tokenized:
        log.info("Tokenization")
        tokens = tokenize(text)
        if not tokens:
            log.error("error during tokenization of file '{0}', exiting".format(filename))
            continue
        tokenized = "\n".join([' '.join(sentence) for sentence in tokens[:-1]])
    else:
        tokenized = text


    log.info("Parsing")
    drs = get_all(tokenized)
    fol = get_fol(tokenized)
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

    # mapping Boxer-prolog variable systems
    i = 0
    prolog_variables = dict()
    for predicate in drs['predicates']:
        if not predicate['variable'] in prolog_variables:
            prolog_variables[predicate['variable']] = ascii_uppercase[i]
            i+=1
    # build symbol-synset dictionary
    synsets = dict()
    for variable, predicates in variables.iteritems():
        for predicate in predicates:
            wn_id = predicate.split('/')[-1]
            synsets[prolog_variables[variable]] = wn_id

    # replace synsets
    fol_predicates = ['all', 'some', 'and', 'or', 'not']
    fol_synsets = fol
    pred_regex=r'(\w+)\((\w+)\)'
    pattern = re.compile(pred_regex, re.MULTILINE)
    for match in re.finditer(pattern, fol):
        predicate, variable = match.groups()
        if variable in synsets:
            synset = synsets[variable]
        else:
            log.error('cannot find synset for predicate {0}({1})'.format(predicate, variable))
            synset = predicate
        fol_synsets = fol_synsets.replace(predicate, synset)

log.info('Writing output on file {0}'.format(options.output_file))
with open(options.output_file, "w") as f:
    f.write("{0}\n".format(fol_synsets))
