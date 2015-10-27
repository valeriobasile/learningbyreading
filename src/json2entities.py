#!/usr/bin/env python
import simplejson as json
from itertools import permutations
from optparse import OptionParser

# command line argument partsing
parser = OptionParser()
parser.add_option('-i',
                  '--input',
                  dest="input_file",
                  help="read text from FILE",
                  metavar="FILE")
(options, args) = parser.parse_args()

# read input file
with open(options.input_file) as f:
    j = json.loads(f.read())

entities = set()
synsets = set()
for i in j:
    db = i['DBpediaURL']
    if db != '':
        entities.add(db)
    bn = i['BabelNetURL']
    if bn != '':
        synsets.add(bn)

for pair in permutations(entities, 2):
    print '<{0}> aloof:cooccurs <{1}>'.format(pair[0].encode('utf-8'), pair[1].encode('utf-8'))
for pair in permutations(synsets, 2):
    print '<{0}> aloof:cooccurs <{1}>'.format(pair[0].encode('utf-8'), pair[1].encode('utf-8'))
