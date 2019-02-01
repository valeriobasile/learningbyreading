from babelfy import babelfy
from ukb import wsd
from candc import postag
from spotlight import spotlight
import ConfigParser
import logging as log
from mappings import bn2dbpedia, offset2bn, bn2offset
from os.path import join, dirname

# read configuration
config = ConfigParser.ConfigParser()
config.read(join(dirname(__file__),'../config/disambiguation.conf'))
config_mapping = ConfigParser.ConfigParser()
config_mapping.read(join(dirname(__file__),'../config/mapping.conf'))

def disambiguation(tokenized, drs):
    # Word Sense Disambiguation
    entities = []
    if config.get('wsd', 'module') == 'babelfy':
        log.info("Calling Babelfy")
        disambiguated = babelfy(tokenized, config.get('babelfy', 'key'))
        synsets = disambiguated['synsets']
        if config_mapping.get('net', 'module') == 'wordnet':
            synsets = babelfy_to_wordnet(synsets)
        if config.get('el', 'module') == 'babelfy':
            log.info("Using Babelfy also for entities")
            if(disambiguated != None):
                entities = disambiguated['entities']
    elif config.get('wsd', 'module') == 'ukb':
        log.info("Calling POS-tagger")
        postags = postag(tokenized)
        log.info("Calling UKB")
        disambiguated = wsd(postags)
        synsets = disambiguated['synsets']
        if config_mapping.get('net', 'module') == 'babelnet':
            synsets = ubk_to_babelnet(synsets)

    # Entity Linking
    if config.get('el', 'module') == 'babelfy' and config.get('wsd', 'module') != 'babelfy':
        log.info("Calling Babelfy")
        disambiguated = babelfy(tokenized)
        if(disambiguated != None):
            entities = disambiguated['entities']
    elif config.get('el', 'module') == 'spotlight':
        log.info("Calling Spotlight")
        disambiguated = spotlight(tokenized)
        if not disambiguated:
            return None, None
        if(disambiguated != None):
            entities = disambiguated['entities']
    elif config.get('el', 'module') == 'none':
        log.info("No module selected for entity linking")
        entities = []

    # enriching the entity list with WordNet mapping
    '''
    for synset in synsets:
        offset = synset['synset'].split('/')[-1]
        if offset in offset2bn:
            bn = offset2bn[offset]
            if bn in bn2dbpedia:
                entity = bn2dbpedia[bn]
                if entity != '-NA-':
                    uri = u'http://dbpedia.org/resource/{0}'.format(entity)
                    if not uri in [e['entity'] for e in entities]:
                        entities.append({'token_start': synset['token_start'],
                                          'token_end': synset['token_end'],
                                         'entity': uri})
    '''

    return synsets, entities

def babelfy_to_wordnet(synsets):
    try:
        for synset in synsets:
            bn_id = synset['synset'].split('/')[-1]
            if bn_id in bn2offset:
                synset['synset'] = 'http://wordnet-rdf.princeton.edu/wn31/{0}'.format(bn2offset[bn_id])
            else:
                synset['synset'] = ''
    except:
        log.error("babelfy(): error linking to WordNet output")
        return None

    return synsets

def ubk_to_babelnet(synsets):
    try:
        for synset in synsets:
            wn_id = synset['synset'].split('/')[-1]
            if wn_id in offset2bn:
                synset['synset'] = 'http://babelnet.org/rdf/{0}'.format(offset2bn[wn_id])
            else:
                synset['synset'] = ''
    except:
        log.error("UBK(): error linking to BabelNet output")
        return None

    return synsets
