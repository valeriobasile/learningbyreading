#!/usr/bin/env python
from optparse import OptionParser
from disambiguation import disambiguation
from candc import tokenize, get_all, get_fol, get_drg
import simplejson as json
import logging as log
from os import listdir
from os.path import isfile, join, dirname
from itertools import product, combinations
from sys import exit
from framenet import frames, vn2fn_roles
from collections import Counter
from mappings import bn2offset
import ConfigParser
from unboxer import unboxer, drg
import re

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
parser.add_option('-c',
                  '--comentions',
                  action="store_true",
                  dest="comentions",
                  help="output co-mentions")
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
        text = f.read()

    # tokenization
    log.info("Tokenization")
    tokens = tokenize(text)
    if not tokens:
        log.error("error during tokenization of file '{0}', exiting".format(filename))
        continue

    tokenized = "\n".join([' '.join(sentence) for sentence in tokens[:-1]])

    log.info("Parsing")
    drs = get_all(tokenized)
    if not drs:
        log.error("error during the execution of Boxer on file '{0}', exiting".format(filename))
        continue
    log.info("Word sense disambiguation and entity linking")
    synsets, entities = disambiguation(tokenized, drs)
    if synsets==None or entities==None:
		log.error("error during the disambiguation of file '{0}', exiting".format(filename))
		continue

    # extracting co-mentions
    if options.comentions:
        dbpedia_entities = set(map(lambda x: x['entity'], entities))
        for entity1, entity2 in combinations(dbpedia_entities, 2):
            if (entity1 != 'null' and
                entity2 != 'null'):
                triples.append(('<{0}>'.format(entity1), '<{0}#comention>', '<{2}>'.format(config.get('namespace', 'relation'), entity2)))

    # build dictionary of variables
    try:
        variables = dict()
        for predicate in drs['predicates']+drs['namedentities']:
            if not predicate['variable'] in variables:
                variables[predicate['variable']] = []
            for synset in synsets:
                # baseline sysnet alignment
                # TODO: make this smarter
                if predicate['token_start'] == synset['token_start'] and predicate['token_end'] == synset['token_end']:
                    if not synset['synset'] in variables[predicate['variable']]:
                        variables[predicate['variable']].append(synset['synset'])
            for entity in entities:
                # baseline entity alignment
                # TODO: make this smarter
                if predicate['token_start'] == entity['token_start'] and predicate['token_end'] == entity['token_end'] and entity['entity'] != 'null':
                    if not entity['entity'] in variables[predicate['variable']]:
                        variables[predicate['variable']].append(entity['entity'])
    except:
        log.error("error during the alignment on file '{0}', exiting".format(filename))
        continue


    # outputs frame instances
    instance_counter = dict()
    frame_instances = dict()
    for variable, senses in variables.iteritems():
        for sense in senses:
            synset = sense.split('/')[-1]
            if synset in frames:
                for frame in frames[synset]:
                    # create new frame instance
                    if not frame in instance_counter:
                        instance_counter[frame] = 0
                    instance_id = "{0}_{1}".format(frame, instance_counter[frame])
                    frame_instances[instance_id] = dict()
                    frame_instances[instance_id]['frame'] = frame
                    frame_instances[instance_id]['synset'] = synset
                    frame_instances[instance_id]['variable'] = variable
                    frame_instances[instance_id]['roles'] = dict()

                    for relation in drs['relations']:
                        if relation['arg1'] == variable and relation['arg2'] in variables and relation['symbol'] in thematic_roles:
                            for filler in variables[relation['arg2']]:
                                if frame in vn2fn_roles:
                                    if relation['symbol'] in vn2fn_roles[frame]:
                                        role = vn2fn_roles[frame][relation['symbol']]
                                    else:
                                        role = "vn:{0}".format(relation['symbol'])
                                    frame_instances[instance_id]['roles'][role] = (relation['arg2'], filler)
                    instance_counter[frame] += 1

    # read DRG
    # TODO: put this into the Unboxer module
    tuples = get_drg(tokenized)
    drgparser = drg.DRGParser()
    d = drgparser.parse_tup_lines(tuples)

    for instance_id, frame_instance in frame_instances.iteritems():
        if len(frame_instance['roles']) > 0:
            # very brutal XML output
            print "<frameinstance id='{0}' type='{1}-{2}', internalvariable='{3}'>".format(instance_id, frame_instance['frame'], frame_instance['synset'], frame_instance['variable'])
            for reificated_frame_var in d.reificated[frame_instance['variable']]:
                print "<framelexicalization>"
                surface = []
                unboxer.generate_from_referent(d, reificated_frame_var, surface, complete=True)
                print ' '.join(surface)
                print "</framelexicalization>"
                print "<frameelements>"
                for role, (variable, filler) in frame_instance['roles'].iteritems():
                    print "<frameelement role='{0}' internalvariable='{1}'>".format(role, variable)
                    print "<concept>"
                    print filler
                    print "</concept>"
                    for reificated_role_var in d.reificated[variable]:
                        print "<conceptexicalization>"
                        surface = []
                        unboxer.generate_from_referent(d, reificated_role_var, surface, complete=True)
                        print ' '.join(surface)
                        print "</conceptlexicalization>"
                        print "<roleexicalization>"
                        surface = unboxer.generate_from_relation(d, reificated_frame_var, reificated_role_var)
                        print surface
                        print "</rolelexicalization>"
                    print "</frameelement>"
                print "</frameelements>"
            print "</frameinstance>"

    # scanning relations
    with open(options.output_file, "a") as f:
        for relation in drs['relations']:
            if (relation['arg1'] in variables and
                relation['arg2'] in variables):
                for entity1, entity2 in product(variables[relation['arg1']],
                                             variables[relation['arg2']]):
                    if relation['symbol'] in thematic_roles:
                        # thematic roles
                        synset = entity1.split('/')[-1]
                        try:
                            framelist = frames[synset]
                        except:
                            log.info('No frame found for synset {0}'.format(synset))
                            continue

                        for frame in framelist:
                            if (entity2 != '' and frame != ''):
                                vnrole = relation['symbol']
                                if frame in vn2fn_roles:
                                    if vnrole in vn2fn_roles[frame]:
                                        role = vn2fn_roles[frame][vnrole]
                                    #else:
                                    #    role = "verbnet:{0}".format(vnrole)
                                        triple = ('<{0}>'.format(entity2),
                                                  '<{0}#{1}>'.format(config.get('namespace', 'relation'), role),
                                                  '<{0}#{1}>'.format(config.get('namespace', 'frame'), frame))
                                        triples.append(triple)
                                        f.write("{0} {1} {2} .\n".format(*triple))
                    '''
                    else:
                        # other types of relations
                        if (entity2 != '' and entity1 != ''):
                            triple = ('<{0}>'.format(entity1),
                                      '<{0}#{1}>'.format(config.get('namespace', 'relation'), relation['symbol']),
                                      '<{0}>'.format(entity2))
                            triples.append(triple)
                            f.write("{0} {1} {2} .\n".format(*triple))
                    '''
