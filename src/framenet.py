import logging as log
import sys

# reads the mapping between version 1.6 and 3.0 of wordnet
wn16tobn = dict()

with open('resources/wnid-bn-16') as f:
    lines = f.readlines()

for line in lines:
    fields = line.rstrip().split(' ')
    wn16tobn[fields[2]] = fields[1]

# builds a dictionary of frame names indexed by wordnet synset id
frames = dict()

with open('resources/eXtendedWFN') as f:
    lines = f.readlines()

for line in lines:
    # fixes bugged lines with pipeline symbol in them
    line = line.rstrip().replace('|', ' ')

    if line.startswith('Frame:'):
        current_frame = line.replace('Frame: ', '')
    elif line != '':
        fields = line.split(' ')
        lemma = fields[0]
        pos = fields[1]
        try:
            synset = wn16tobn[fields[2]]
        except:
            continue
        if synset in frames:
            if not current_frame in frames[synset]:
                frames[synset].append(current_frame)
        else:
            frames[synset] = [current_frame]
