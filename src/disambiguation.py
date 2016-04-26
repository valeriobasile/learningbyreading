from babelfy import babelfy
from ukb import wsd
from spotlight import spotlight
import ConfigParser
import logging as log
from mappings import bn2dbpedia, offset2bn

# read configuration
config = ConfigParser.ConfigParser()
config.read('config/disambiguation.conf')

def disambiguation(tokenized, drs):
	# Word Sense Disambiguation
	if config.get('wsd', 'module') == 'babelfy':
		log.info("Calling Babelfy")
		disambiguated = babelfy(tokenized)
		synsets = disambiguated['synsets']
		if config.get('el', 'module') == 'babelfy':
			log.info("Using Babelfy also for entities")
			entities = disambiguated['entities']
	elif config.get('wsd', 'module') == 'ukb':
			log.info("Calling UKB")
			disambiguated = wsd(drs['predicates'])
			synsets = disambiguated['synsets']

	# Entity Linking
	if config.get('el', 'module') == 'babelfy' and config.get('wsd', 'module') != 'babelfy':
		log.info("Calling Babelfy")
		disambiguated = babelfy(tokenized)
		entities = disambiguated['entities']
	elif config.get('el', 'module') == 'spotlight':
		log.info("Calling Spotlight")
		disambiguated = spotlight(tokenized)
		entities = disambiguated['entities']
	elif config.get('el', 'module') == 'none':
		log.info("No module selected for entity linking")
		entities = []

	# enriching the entity list with WordNet mapping
	for synset in synsets:
		offset = synset['synset'].split('/')[-1]
		if offset in offset2bn:
			bn = offset2bn[offset]
			if bn in bn2dbpedia:
				entity = bn2dbpedia[bn]
				if entity != '-NA-':
					uri = u'https://dbpeia.org/resource/{0}'.format(entity)
					entities.append({'token_start': synset['token_start'],
	                                 'token_end': synset['token_end'],
	                                 'entity': uri})
	return synsets, entities
