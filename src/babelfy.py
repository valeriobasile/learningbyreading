import subprocess
import json
from requests import get
import logging as log
import os
from os.path import join, dirname
from nltk.stem import WordNetLemmatizer
from nltk import pos_tag
from babelpy.babelfy import BabelfyClient
from nltk.corpus import wordnet

def get_wordnet_pos(treebank_tag):

    if treebank_tag.startswith('J'):
        return wordnet.ADJ
    elif treebank_tag.startswith('V'):
        return wordnet.VERB
    elif treebank_tag.startswith('N'):
        return wordnet.NOUN
    elif treebank_tag.startswith('R'):
        return wordnet.ADV
    else:
        return None

def babelfy(text, key):
    params = {"lang": "en"}
    babel_client = BabelfyClient(key, params)
    babel_client.babelfy(text)

    synsets = []
    entities = []

    for entity in babel_client.merged_entities:
        token_start = entity["tokenFragment"]["start"]
        token_end = entity["tokenFragment"]["end"]
        synset = entity["babelSynsetID"]
        lemma = entity["text"]
        synsets.append({'token_start' : token_start,
                        'token_end' : token_end,
                        'synset' : synset,
                        'lemma' : lemma})
        entities.append({'token_start' : token_start,
                        'token_end' : token_end,
                        'entity' : synset,
                        'lemma' : lemma})
    return {'synsets' : synsets, 'entities' : entities}
