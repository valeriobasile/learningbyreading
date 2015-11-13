import logging as log

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
        offset = fields[2]
        if offset in frames:
            if not current_frame in frames[offset]:
                frames[offset].append(current_frame)
        else:
            frames[offset] = [current_frame]
