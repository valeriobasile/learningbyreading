#-*- coding: UTF-8 -*-
import re
import rdflib
from os.path import join, dirname
from mappings import wn30wn31
from prettytable import PrettyTable

def load_fnbn_mapping(file_path):
    fnbn_mapping = dict()
    graph = rdflib.Graph()
    triple_list = graph.parse(join(dirname(__file__), file_path), format='turtle')

    for subj, pred, obje  in triple_list:
        subj = re.match(".+/([^/]+)", subj.toPython()).group(1)
        obje = re.match(".+/([^/]+)", obje.toPython()).group(1)
        if subj not in fnbn_mapping: fnbn_mapping[subj] = []
        fnbn_mapping[subj].append(obje)

    return fnbn_mapping

def load_fnwn_mapping(file_path):
    fnwn_mapping = dict()

    with open(join(dirname(__file__), file_path)) as f:
        #   0 ## Killing - suffocation.n - n#225593
        for line in f:
            try:
                frame, synset_id, posoffset = line.rstrip().split(' - ')
            except:
                print "error reading line\n{0}".format(line)
                continue
            frame = frame.split(' ')[-1]
            lemma, pos = synset_id.split('.')
            pos, offset = posoffset.split('#')
            offset30 = '{0:8d}-{1}'.format(eval(offset), pos).replace(' ', '0')

            if offset30 in wn30wn31:
                offset31 = wn30wn31[offset30]

                if frame not in fnwn_mapping: fnwn_mapping[frame] = []
                fnwn_mapping[frame].append(offset31)

            else:
                print "{0} not in wn30 mapping".format(offset30)

    return fnwn_mapping

def count_synsets(synsets):
    counter = 0

    for frame in synsets.keys():
        counter += len(set(synsets[frame]))

    return counter

if __name__ == '__main__':
    wordnet = load_fnwn_mapping("../resources/framenet-wordnet-map.txt")
    babelnet = load_fnbn_mapping("../resources/fn2bnFrameBase.ttl")
    table = PrettyTable()
    table.field_names = ["Item", "WordNet", "BabelNet"]
    table.add_row(["Frames", len(wordnet), len(babelnet)])
    table.add_row(["Synsets", count_synsets(wordnet), count_synsets(babelnet)])
    table.align = "r"
    print table
    



