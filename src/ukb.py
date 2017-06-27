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
from os.path import join, dirname

def wsd(postags):
    context = []
    for index, item in enumerate(postags.split(' ')):
        word, postag = item.split('|')
        wnpostag = postag.lower()[0]
        context.append(u'{0}#{1}#{2}#1'.format(word, wnpostag, index))
    #context = [u'{0}#{1}#{2}:{3}#1'.format(predicate['symbol'], predicate['type'], predicate['token_start'], predicate['token_end']) for predicate in predicates]
    f = NamedTemporaryFile('w', delete=False)
    f.write(u'sentence\n{0}\n'.format(' '.join(context)).encode('utf-8'))
    f.close()
    basedir = os.path.abspath(join(dirname(__file__),'../ext/ukb'))
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

    synsets = []
    for line in out.split('\n'):
        if not line.startswith('!!') and len(line) > 1:
            try:
                fields = line.rstrip().split(' ')
                ctxid = fields[0]
                tokenid = eval(fields[1])
                lemma = fields[-1]
                wn30ids = fields[3:-2]
            except:
                log.error('cannot parse line:\n{0}'.format(line))
                sys.exit(1)
            for wn30id in wn30ids:
                try:
                    wn31id = wn30wn31[wn30id]
                except:
                    log.info('cannot find Wordnet 3.1 synset for WN3.0 synset {0}'.format(wn30id))
                    continue

                synset = 'http://wordnet-rdf.princeton.edu/wn31/{0}'.format(wn31id)

                synsets.append({'token_start':tokenid,
                                 'token_end':tokenid,
                                 'synset': synset})
    return {'synsets':synsets}
