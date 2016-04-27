#!/usr/bin/env python
from optparse import OptionParser
import logging as log
from os import listdir
from os.path import isfile, join, dirname
from framenet import frames
import ConfigParser

# log configuration
log.basicConfig(level=log.INFO, format='%(asctime)s.%(msecs)03d %(levelname)s %(message)s')

# read configuration
config = ConfigParser.ConfigParser()
config.read(join(dirname(__file__),'../config/namespace.conf'))

with open(join(dirname(__file__),'../resources/thematic_roles.txt')) as f:
    thematic_roles = [line.rstrip() for line in f]

# command line argument partsing
parser = OptionParser()
parser.add_option('-i',
                  '--input',
                  dest="input_file",
                  help="read text from FILE",
                  metavar="FILE")
parser.add_option('-d',
                  '--input-dir',
                  dest="input_dir",
                  help="read text files from DIRECTORY",
                  metavar="DIRECTORY")
parser.add_option('-o',
                  '--output',
                  dest="output_file",
                  help="write JSON to FILE",
                  metavar="FILE")
(options, args) = parser.parse_args()

if options.input_file:
    documents = [options.input_file]
else:
    documents = [ join(options.input_dir,f) for f in listdir(options.input_dir) if isfile(join(options.input_dir,f)) ]

triples = list()
with open(options.output_file, "w") as f:
    pass

for filename in documents:
    # read file
    log.info("opening file {0}".format(filename))
    with open(filename) as f:
        lines = f.readlines()

    with open(options.output_file, "a") as f:
        for line in lines:
            subj, pred, obj, _ = line.rstrip().split(' ')
            relation = pred[:-1].split('#')[-1]
            if 'wordnet-rdf.princeton.edu' in obj and relation in thematic_roles:
                synset = obj[:-1].split('/')[-1]
                try:
                    framelist = frames[synset]
                except:
                    log.info('No frame found for synset {0}'.format(synset))
                    continue

                for frame in framelist:
                    f.write("{0} {1} <{2}#{3}> .\n".format(subj, pred, config.get('namespace', 'frame'), frame))
