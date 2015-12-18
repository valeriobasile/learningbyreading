import logging as log

# builds a dictionary of frame names indexed by wordnet synset id
offset2bn = dict()
bn2offset = dict()
offset2wn = dict()
wn2offset = dict()
wn2bn = dict()
bn2wn = dict()

# the mapping is in a tabular file, e.g.:
# s00069798n Scout-n#2-n 110582611-n
with open('resources/bn35-wn31.map') as f:
    for line in f:
        bn_id, wn_id, wn_offset = line.rstrip().split(' ')
        offset2bn[wn_offset] = bn_id
        bn2offset[bn_id] = wn_offset
        offset2wn[wn_offset] = wn_id
        wn2offset[wn_id] = wn_offset
        wn2bn[wn_id] = bn_id
        bn2wn[bn_id] = wn_id
