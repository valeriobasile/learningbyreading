import subprocess
import simplejson as json
from requests import get
import logging as log
from mappings import bn2offset
import os
import ConfigParser
from tempfile import NamedTemporaryFile
import sys
import shlex
from subprocess import CalledProcessError

def wsd(predicates):
    context = ['{0}#{1}#{2}-{3}#1'.format(predicate['symbol'], predicate['type'], predicate['token_start'], predicate['token_end']) for predicate in predicates]
    #f = NamedTemporaryFile()
    f = open('test.txt', 'w')
    f.write('sentence\n{0}\n'.format(' '.join(context)))
    f.close()
    basedir = os.path.abspath('ext/ukb')
    ukb = '{0}/bin/ukb_wsd'.format(basedir)
    relation_file = '{0}/wn30.bin'.format(basedir)
    dict_file = '{0}/lkb_sources/30/wnet30_dict.txt'.format(basedir)
    cmdline = "{0} --ppr -K {1} -D {2} {3}".format(ukb, relation_file, dict_file, os.path.abspath(f.name))

    try:
        process = subprocess.Popen(shlex.split(cmdline), universal_newlines=True,
                            shell=False,
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    except CalledProcessError, e:
        print e.args
        print e.cmd
        print e.message
        print e.output
        print e.returncode
        print '--'
    out, err = process.communicate()
    entities = []
    for line in out.split('\n'):
        if not line.startswith('!!') and len(line) > 1:
            ctxid, tokens, _, wn30id, _, lemma = line.rstrip().split(' ')
            token_start, token_end = map(eval, tokens.split('-'))
            entities.append({'token_start':token_start,
                             'token_end':token_end,
                             'synset':wn30id})
    return {'entities':entities}
