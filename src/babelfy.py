import subprocess
import simplejson as json
from requests import get
import logging as log
import os
from os.path import join, dirname
from nltk.stem import WordNetLemmatizer
from nltk import pos_tag

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

with open(join(dirname(__file__),'../config/babelfy.var.properties')) as f:
    babelnet_key = f.readlines()[0][:-1].split('=')[1]

def babelfy(text):
    postagged = pos_tag(text.split(' '))
    wordnet_lemmatizer = WordNetLemmatizer()
    lemmatized = []
    lemmas = []
    for token, pos in postagged:
        wnpos = get_wordnet_pos(pos)
        if wnpos != None:
            lemma = wordnet_lemmatizer.lemmatize(token, wnpos)
        else:
            lemma = wordnet_lemmatizer.lemmatize(token)
        lemmatized.append("{0}|{1}".format(token, lemma))
        lemmas.append(lemma)
    try:
        # we have to use an external POS tagger and lemmatizer
        text = ' '.join(lemmatized)
        libs = [join(dirname(__file__),'../libs/babelfy-aloof/babelfy-aloof.jar'),join(dirname(__file__),'../libs/babelfy-aloof/libs/*'), join(dirname(__file__),'../config')]
        local_libs = map(lambda x: join(dirname(__file__),'../'+x), libs)
        local_libs = libs
        process = subprocess.Popen(["java", "-cp", ":".join(local_libs), "BabelfyAloof", text],
                               shell=False,
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
        out, err = process.communicate(text)
    except:
        log.error("babelfy(): error executing Babelfy Java API")
        return None

    if process.returncode:
        log.error("babelfy(): error executing Babelfy Java API (return code:{0}):\n{1}".format(process.returncode, err))
        return None

    try:
        lines = out.split("\n")[:-1]
        synsets = []
        entities = []
        for line in lines:
            token_start, token_end, synset, entity = line.split('\t')
            token_start = eval(token_start)
            token_end = eval(token_end)
            lemma = '_'.join([lemmas[i] for i in range(token_start, token_end+1)])
            synsets.append({'token_start' : token_start,
                            'token_end' : token_end,
                            'synset' : synset,
                            'lemma' : lemma})
            entities.append({'token_start' : token_start,
                            'token_end' : token_end,
                            'entity' : synset,
                            'lemma' : lemma})
    except:
        log.error("babelfy(): error processing Babelfy output")
        return None

    return {'synsets' : synsets, 'entities' : entities}
