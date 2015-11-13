import logging as log

# reads the mapping between version 1.6 and 3.0 of wordnet
newer = dict()

with open('resources/wn16-30') as f:
    lines = f.readlines()

for line in lines:
    fields = line.rstrip().split(' ')
    newer[fields[0]] = fields[1]

# builds a dictionary of frame names indexded by wordnet synset id
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
            offset = newer[fields[2]]
        except:
            continue
        if offset in frames:
            if not current_frame in frames[offset]:
                frames[offset].append(current_frame)
        else:
            frames[offset] = [current_frame]
