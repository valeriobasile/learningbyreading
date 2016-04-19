#!/usr/bin/env python
from optparse import OptionParser
from babelfy import babelfy
from candc import tokenize, get_all, get_fol
from ukb import wsd
import simplejson as json
import logging as log
from os import listdir
from os.path import isfile, join
from itertools import product, combinations
from sys import exit
from framenet import frames
from collections import Counter
from mappings import bn2offset

# log configuration
log.basicConfig(level=log.INFO)

# command line argument partsing
parser = OptionParser()
parser.add_option('-i',
                  '--input',
                  dest="input_file",
                  help="read text from FILE",
                  metavar="FILE")
(options, args) = parser.parse_args()

log.info("opening file {0}".format(options.input_file))
with open(options.input_file) as f:
    text = f.read()

# tokenization
log.info("calling tokenizer")
tokens = tokenize(text)
if not tokens:
    log.error("error during tokenization of file '{0}', exiting".format(options.input_file))
    tokenized = ''
tokenized = " ".join(tokens)

# process the text
babel = babelfy(tokenized, wordnet=True)
if not babel:
    log.error("error during the execution of Babelfy on file '{0}', exiting".format(options.input_file))
    babel = None
print babel

drs = get_all(tokenized)
disambiguated = wsd(drs['predicates'])
print disambiguated
