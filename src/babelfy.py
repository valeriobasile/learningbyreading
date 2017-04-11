import subprocess
import simplejson as json
from requests import get
import logging as log
import os
from os.path import join, dirname

with open(join(dirname(__file__),'../config/babelfy.var.properties')) as f:
    babelnet_key = f.readlines()[0][:-1].split('=')[1]

def babelfy(text):
    try:
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
        log.error("babelfy(): error executing Babelfy Java API:\n{0}".format(err))
        return None

    if process.returncode:
        log.error("babelfy(): error executing Babelfy Java API (return code:{0}):\n{1}".format(process.returncode, err))
        return None

    try:
        lines = out.split("\n")[:-1]
        synsets = map(lambda x: {'token_start':eval(x.split('\t')[0]),
                                  'token_end':eval(x.split('\t')[1]),
                                  'synset':x.split('\t')[2]},
                                  lines)
        entities = map(lambda x: {'token_start':eval(x.split('\t')[0]),
                                  'token_end':eval(x.split('\t')[1]),
                                  'entity':'{0}'.format(x.split('\t')[3])},
                                  lines)
    except:
        log.error("babelfy(): error processing Babelfy output")
        return None

    return {'synsets' : synsets, 'entities' : entities}