import subprocess
import simplejson as json
from requests import get
import logging as log

with open('config/babelfy.var.properties') as f:
    babelnet_key = f.readlines()[0][:-1].split('=')[1]

def babelfy(text):
    try:
        process = subprocess.Popen(["java", "-jar", "libs/babelfy.jar", text],
                               shell=False,
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
        out, err = process.communicate(text)
    except:
        log.error("babelfy(): error executing Babelfy Java API")
        return None

    try:
        lines = out.split("\n")[:-1]
        entities = map(lambda x: {'token_start':eval(x.split('\t')[0]),
                              'token_end':eval(x.split('\t')[1]),
                              'synset':x.split('\t')[2],
                              'entity':x.split('\t')[3]},
                              lines)
    except:
        log.error("babelfy(): error processing Babelfy output")
        return None

    return {'entities' : entities}
