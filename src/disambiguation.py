from babelfy import babelfy
from ukb import wsd
import ConfigParser
import logging as log

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
	elif config.get('el', 'module') == 'none':
		log.info("No module selected for entity linking")
		entities = []
	return synsets, entities
