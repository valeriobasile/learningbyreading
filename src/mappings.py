import logging as log
import os
import re

# builds a dictionary of frame names indexed by wordnet synset id
offset2bn = dict()
bn2offset = dict()
offset2wn = dict()
wn2offset = dict()
wn2bn = dict()
bn2wn = dict()
wn30wn31 = dict()
wn31wn30 = dict()
bn2dbpedia = dict()
dbpedia2bn = dict()

# the mapping is in a tabular file, e.g.:
# s00069798n Scout-n#2-n 110582611-n
with open(os.path.join(os.path.dirname(__file__), '../resources/bn35-wn31.map')) as f:
    for line in f:
        bn_id, wn_id, wn_offset = line.rstrip().split(' ')
        if wn_offset.endswith("-s"): wn_offset = wn_offset.replace("-s", "-a")# To use only the tag "a" for adjetives
        if wn_id.endswith("-s"): wn_id = re.sub("(-s)(#\d+)(-s)", "-a\\2-a", wn_id)# To use only the tag "a" for adjetives
        offset2bn[wn_offset[1:]] = bn_id
        bn2offset[bn_id] = wn_offset[1:]
        offset2wn[wn_offset[1:]] = wn_id
        wn2offset[wn_id] = wn_offset[1:]
        wn2bn[wn_id] = bn_id
        bn2wn[bn_id] = wn_id

# Mapping different WN versions
# 00013662-a 00013681-a
with open(os.path.join(os.path.dirname(__file__), '../resources/wn30-31')) as f:
    for line in f:
        wn30, wn31 = line.rstrip().split(' ')
        wn30wn31[wn30] = wn31
        wn31wn30[wn31] = wn30

# Mapping BabelNet-DBpedia
# s00000006n Dodecanol
for i in range(4):
    filename = os.path.join(os.path.dirname(__file__), '../resources/bn-dbpedia{0}'.format(i+1))
    with open(filename) as f:
        for line in f:
            bn_id, dbpedia_id = line.rstrip().split(' ')
            dbpedia2bn[dbpedia_id] = bn_id
            bn2dbpedia[bn_id] = dbpedia_id
