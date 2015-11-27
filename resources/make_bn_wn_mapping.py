#!/usr/bin/env python
from SPARQLWrapper import SPARQLWrapper, JSON
from sys import stderr
from time import sleep
import logging as log

limit = 2500
timeout = 30
timeout_long = 300
log.basicConfig(level=log.INFO)

out_file = "resources/bn35-wn31.map"
f = open(out_file, 'w')
f.close()

wn_offsets = dict()
with open('resources/wn31.map') as f:
    for line in f:
        wn_id, wn_offset = line.rstrip().split(' ')
        wn_offsets[wn_id] = wn_offset

i = 0
while True:
    offset = limit * i
    query = """
    SELECT *
    WHERE {{
     ?bn <http://www.w3.org/2002/07/owl#sameAs> ?wn .
    }} limit {0} offset {1}""".format(limit, offset)

    log.info('querying BabelNet sparql endpoint ({0}-{1})'.format(limit*i, limit*(i+1)-1))
    sparql = SPARQLWrapper("http://babelnet.org/sparql/")
    try:
        sparql.setQuery(query)
        results = sparql.query().convert()
    except:
        log.error('sparql endpoint returned an error: waiting and retrying')
        sleep(timeout_long)
        continue
    result_tags = results.getElementsByTagName('result')

    if len(result_tags) == 0:
        log.info('end of data, exiting.')
        break

    for result in result_tags:
        binding_bn, binding_wn = result.getElementsByTagName('binding')
        bn_uri = binding_bn.firstChild.firstChild.nodeValue
        bn_id = bn_uri.split('/')[-1]
        wn_uri = binding_wn.firstChild.firstChild.nodeValue
        wn_id = wn_uri.split('/')[-1]

        if wn_id in wn_offsets:
            with open(out_file, 'a') as f:
                f.write("{0} {1} {2}\n".format(bn_id, wn_id, wn_offsets[wn_id]))

    i = i + 1
    sleep(timeout)
