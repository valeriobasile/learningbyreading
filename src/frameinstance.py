from lxml import etree, objectify
from unboxer import unboxer, drg
import re
from framenet import frames, vn2fn_roles
from candc import get_drg
import logging as log
from mappings import offset2wn, bn2wn
import ConfigParser
from os.path import dirname, join
from uuid import uuid4

# read configuration
config = ConfigParser.ConfigParser()
config.read(join(dirname(__file__),'../config/namespace.conf'))
config.read(join(dirname(__file__),'../config/disambiguation.conf'))
config_mapping = ConfigParser.ConfigParser()
config_mapping.read(join(dirname(__file__),'../config/mapping.conf'))

if config_mapping.get('net', 'module') == 'wordnet':
    mapping_net = offset2wn
elif config_mapping.get('net', 'module') == 'babelnet':
    mapping_net = bn2wn

def get_frame_instances(variables, semantics, thematic_roles):
    frame_instances = dict()
    for variable, senses in variables.iteritems():
        for sense in senses:
            synset = sense.split('/')[-1]
            if synset in frames:
                framelist = frames[synset]
            else:
                framelist = ['Unmapped']
            for frame in framelist:
                # create new frame instance
                instance_id = "{0}_{1}".format(frame, uuid4())
                frame_instances[instance_id] = dict()
                frame_instances[instance_id]['frame'] = frame
                frame_instances[instance_id]['synset'] = synset
                frame_instances[instance_id]['variable'] = variable
                frame_instances[instance_id]['roles'] = dict()

                for relation in semantics['relations']:
                    if config.get('semantics', 'module') == 'boxer':
                        if relation['arg1'] == variable and relation['arg2'] in variables and relation['symbol'] in thematic_roles:
                            for filler in variables[relation['arg2']]:
                                if frame in vn2fn_roles and relation['symbol'] in vn2fn_roles[frame]:
                                    role = vn2fn_roles[frame][relation['symbol']]
                                else:
                                    role = "vn-{0}".format(relation['symbol'])
                                frame_instances[instance_id]['roles'][role] = (relation['arg2'], filler)
                    elif config.get('semantics', 'module') == 'semafor':
                        for filler in variables[relation['arg2']]:
                            frame_instances[instance_id]['roles'][relation['symbol']] = (relation['arg2'], filler)

    return frame_instances

def get_frame_triples(frame_instances):
    triples = []
    for frame_instance_id, frame_instance in frame_instances.iteritems():
        if len(frame_instance['roles']) > 0:
            if frame_instance['frame'] != "Unmapped":
                framebase_id = "{0}-{1}".format(frame_instance['frame'], mapping_net[frame_instance['synset']].split("#")[0].replace('-', '.'))
            else:
                log.info('No mapping found for synset {0}'.format(frame_instance['synset']))
                framebase_id = "{0}-{1}".format(frame_instance['frame'], frame_instance['synset'])

            triple = ('<{0}/fi-{1}>'.format(config.get('namespace', 'frame'), frame_instance_id),
                      '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>',
                      '<{0}/frame-{1}>'.format(config.get('namespace', 'frame'), framebase_id))
            triples.append(triple)
            for role, (variable, filler) in frame_instance['roles'].iteritems():
                filler = unicode(filler, "utf-8")
                triple = ('<{0}/fi-{1}>'.format(config.get('namespace', 'frame'), frame_instance_id),
                          '<{0}/fe-{1}>'.format(config.get('namespace', 'frame'), role),
                          '<{0}>'.format(filler.encode('utf-8')))
                triples.append(triple)
    return triples

def get_aligned_frames_xml(tokenized, frame_instances, root):
    # read DRG
    tuples = get_drg(tokenized)
    drgparser = drg.DRGParser()
    d = drgparser.parse_tup_lines(tuples)

    for instance_id, frame_instance in frame_instances.iteritems():
        if len(frame_instance['roles']) > 0:
            if frame_instance['frame'] != "Unmapped":
                framebase_id = "{0}-{1}".format(frame_instance['frame'], mapping_net[frame_instance['synset']].split("#")[0].replace('-', '.'))
            else:
                log.info('No mapping found for synset {0}'.format(frame_instance['synset']))
                continue
            tag_frameinstance = objectify.SubElement(root, "frameinstance")
            tag_frameinstance.attrib['id']=instance_id
            tag_frameinstance.attrib['type']=framebase_id
            tag_frameinstance.attrib['internalvariable']=frame_instance['variable']

            for reificated_frame_var in d.reificated[frame_instance['variable']]:
                tag_framelexicalization = objectify.SubElement(tag_frameinstance, "framelexicalization")
                surface = []
                unboxer.generate_from_referent(d, reificated_frame_var, surface, complete=False)
                tag_framelexicalization[0] = ' '.join(surface)
                tag_instancelexicalization = objectify.SubElement(tag_frameinstance, "instancelexicalization")
                surface = []
                unboxer.generate_from_referent(d, reificated_frame_var, surface, complete=True)
                tag_instancelexicalization[0] = ' '.join(surface)
                tag_frameelements = objectify.SubElement(tag_frameinstance, "frameelements")
                for role, (variable, filler) in frame_instance['roles'].iteritems():
                    tag_frameelement = objectify.SubElement(tag_frameelements, "frameelement")
                    tag_frameelement.attrib['role'] = role
                    tag_frameelement.attrib['internalvariable'] = variable
                    tag_concept = objectify.SubElement(tag_frameelement, "concept")
                    tag_concept[0] = filler
                    try:
                        for reificated_role_var in d.reificated[variable]:
                            # composed lexicalization
                            surface = unboxer.generate_from_relation(d, reificated_frame_var, reificated_role_var)
                            if surface != None:
                                tag_rolelexicalization = objectify.SubElement(tag_frameelement, "rolelexicalization")
                                tag_rolelexicalization[0] = surface

                                # complete surface forms
                                surface = []
                                unboxer.generate_from_referent(d, reificated_role_var, surface, complete=True)
                                tag_conceptlexicalization = objectify.SubElement(tag_frameelement, "conceptlexicalization")
                                tag_conceptlexicalization[0] = ' '.join(surface)
                    except:
                        log.error("error with DRG reification: {0}".format(variable))

    objectify.deannotate(root, xsi_nil=True)
    etree.cleanup_namespaces(root)
    return etree.tostring(root, pretty_print=True)
