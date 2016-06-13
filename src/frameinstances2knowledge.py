#!/usr/bin/env python

import sys
from mappings import offset2bn, bn2dbpedia
#print offset2bn

frame_type = dict()
frequencies = dict()
with open(sys.argv[1]) as f:
    for line in f:
        s, p, o, _ = line.rstrip().split(' ')
        if p == '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>':
            frame_type[s] = o
        else:
            frame = frame_type[s][1:-1].split('/')[-1].split('-')[-2]
            role = p[1:-1].split('/')[-1].split('-')[-1]
            if 'wordnet-rdf.princeton.edu' in o:
                try:
                    filler = bn2dbpedia[offset2bn[o[1:-1].split('/')[-1]]]
                except:
                    #sys.stderr.write('cannot find mapping for {0}\n'.format(o[1:-1].split('/')[-1]))
                    continue
            elif 'dbpedia' in o:
                filler = o[1:-1].split('/')[-1]
            if filler == '-NA-' or filler == 'Male' or filler == 'Female':
                continue
            triple = (frame, filler)
            if not triple in frequencies:
                frequencies[triple] = 0
            frequencies[triple] += 1

for triple, frequency in frequencies.iteritems():
    print frequency, ' '.join(triple)
