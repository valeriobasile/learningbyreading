import subprocess
import simplejson as json
from requests import get
import logging as log
from mappings import bn2offset
import os

with open(os.path.join(os.path.dirname(__file__), '../config/babelfy.var.properties')) as f:
    babelnet_key = f.readlines()[0][:-1].split('=')[1]

def babelfy(text, wordnet=False):
    try:
        libs = ["libs/babelfy-aloof/babelfy-aloof.jar","libs/babelfy-aloof/libs/*", "config"]
        local_libs = map(lambda x: os.path.join(os.path.dirname(__file__), "../"+x), libs)
        process = subprocess.Popen(["java", "-cp", ":".join(local_libs), "BabelfyAloof", text],
                               shell=False,
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
        out, err = process.communicate(text)
    except:
        log.error("babelfy(): error executing Babelfy Java API:\n{0}".format(err))
        return None

    if process.returncode:
        log.error("babelfy(): error executing Babelfy Java API (return code:{0}):\n{1}".format(process.returncode, err))
        return None

    try:
        lines = out.split("\n")[:-1]
        entities = map(lambda x: {'token_start':eval(x.split('\t')[0]),
                                  'token_end':eval(x.split('\t')[1]),
                                  'bn_url':x.split('\t')[2],
                                  'entity':'{0}'.format(x.split('\t')[3])},
                                  lines)
    except:
        log.error("babelfy(): error processing Babelfy output")
        return None

    try:
        if wordnet:
            log.info("linking to WordNet only")
            for entity in entities:
                bn_id = entity['bn_url'].split('/')[-1]
                if bn_id in bn2offset:
                    entity['entity'] = 'http://wordnet-rdf.princeton.edu/wn31/{0}'.format(bn2offset[bn_id])
                else:
                    entity['entity'] = 'null'
    except:
        log.error("babelfy(): error linking to WordNet output")
        return None

    return {'entities' : entities}
