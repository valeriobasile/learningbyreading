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
    fol = get_fol(tokenized)
    drs = get_all(tokenized)
    if not drs:
        log.error("error during the execution of Boxer on file '{0}', exiting".format(filename))
        continue

    log.info("calling Babelfy")
    babel = babelfy(tokenized)
    if not babel:
        log.error("error during the execution of Babelfy on file '{0}', exiting".format(filename))
        continue

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
                    variables[predicate['variable']].append({
                        'entity' : entity['entity'],
                        'bn_url' : entity['bn_url'],
                        'symbol' : predicate['symbol'],
                        'sense' : predicate['sense'],
                        'type' : predicate['type']})
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
            if predicate['sense'] == '0':
                sense = '1'
            else:
                sense = predicate['sense']
            symbol = predicate['type']+sense+predicate['symbol']
            bn_id = predicate['bn_url'].split('/')[-1]
            if bn_id in bn2offset:
                synset = bn2offset[bn_id]
                synsets[prolog_variables[variable]] = synset
            else:
                log.error('cannot find mapping for BN synset {0}'.format(bn_id))

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
