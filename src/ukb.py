import subprocess
import simplejson as json
from requests import get
import logging as log
from mappings import bn2offset, wn30wn31, offset2bn, bn2dbpedia
import os
from tempfile import NamedTemporaryFile
import sys
import shlex
from subprocess import CalledProcessError

def wsd(predicates, wordnet=False):
    context = ['{0}#{1}#{2}-{3}#1'.format(predicate['symbol'], predicate['type'], predicate['token_start'], predicate['token_end']) for predicate in predicates]
    f = NamedTemporaryFile('w', delete=False)
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
    os.remove(f.name)

    entities = []
    for line in out.split('\n'):
        if not line.startswith('!!') and len(line) > 1:
            ctxid, tokens, _, wn30id, _, lemma = line.rstrip().split(' ')
            try:
                wn31id = wn30wn31[wn30id]
            except:
                log.info('cannot find Wordnet 3.1 synset for WN3.0 synset {0}'.format(wn30id))
                continue
            try:
                bn_id = offset2bn[wn31id]
            except:
                log.info('cannot find BabelNet synset for WN3.1 synset {0}'.format(wn31id))
                dbpedia_id = ''
            try:
                dbpedia_id = bn2dbpedia[bn_id]
                if dbpedia_id == "-NA-":
                    dbpedia_id=''
            except:
                log.info('cannot find DBpedia synset for BabelNet synset {0}'.format(bn_id))
                dbpedia_id = ''

            token_start, token_end = map(eval, tokens.split('-'))
            synset = 'http://wordnet-rdf.princeton.edu/wn31/{0}'.format(wn31id)
            if wordnet:
                entity = 'http://wordnet-rdf.princeton.edu/wn31/{0}'.format(wn31id)
            else:
                if dbpedia_id != '':
                    entity = 'http://dbpedia.org/resource/{0}'.format(dbpedia_id)
                else:
                    entity = ''

            entities.append({'token_start':token_start,
                             'token_end':token_end,
                             'synset': synset,
                             'entity': entity})
    return {'entities':entities}
