import sys
import rdflib
import re
import ConfigParser
import logging as log
from os.path import join, dirname
from mappings import wn30wn31
from lxml import etree, objectify

# read configuration
config_mapping = ConfigParser.ConfigParser()
config_mapping.read(join(dirname(__file__),'../config/mapping.conf'))

frames = {}

# builds a dictionary of frame names indexed by wordnet3.1 synset id
def read_wnfn_mapping():
    with open(join(dirname(__file__), '../resources/framenet-wordnet-map.txt')) as f:
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

                if offset31 in frames:
                    if not frame in frames[offset31]:
                        frames[offset31].append(frame)
                else:
                    frames[offset31] = [frame]
            else:
                print "{0} not in wn30 mapping".format(offset30)

#builds a dictionary of frame names indexed by babelnet synset id
def read_bnfn_mapping():
    graph = rdflib.Graph()
    triple_list = graph.parse(join(dirname(__file__), '../resources/fn2bnFrameBase.ttl'), format='turtle')

    for subj, pred, obje  in triple_list:
        subj = re.match(".+/([^/]+)", subj.toPython()).group(1)
        obje = re.match(".+/([^/]+)", obje.toPython()).group(1)
        if obje not in frames: frames[obje] = []
        frames[obje].append(subj)

if config_mapping.get('net', 'module') == 'wordnet':
    read_wnfn_mapping()
elif config_mapping.get('net', 'module') == 'babelnet':
    read_bnfn_mapping()

# read SemLinks for the mapping between Verbnet and FrameNet roles
vn2fn_roles = {}
with open(join(dirname(__file__), '../resources/VN-FNRoleMapping.txt')) as f:
    #   0 ## Killing - suffocation.n - n#225593
    semlinks = objectify.fromstring(f.read())
    for verbclass in semlinks.findall('.//vncls'):
        frame = verbclass.attrib['fnframe']
        if not frame in vn2fn_roles:
            vn2fn_roles[frame] = dict()
        try:
            for role in verbclass['roles']['role']:
                vn2fn_roles[frame][role.attrib['vnrole']] = role.attrib['fnrole']
        except:
            # there is one empty frame: Weathcer
            pass