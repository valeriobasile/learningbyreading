#!/usr/bin/env python
from requests import post
from lxml import etree, objectify

def boxer(text):
    # HTTP request
    r = post('http://gingerbeard.alwaysdata.net/candcapi/proxy.php/raw/pipeline?resolve=true&instantiate=true&roles=verbnet&format=xml', data=text)
    xml = r.text.encode("utf-8")
    drs = objectify.fromstring(xml)
    tagtokens = drs['xdrs']['taggedtokens']['tagtoken']
    # get the predicates
    predicates = []
    preds = drs.findall('.//pred')
    for pred in preds:
        poslist = map(lambda x: x.attrib['pos'], pred['indexlist']['index'])
        predicate = {'token_start':eval(poslist[0])-1,
                     'token_end':eval(poslist[-1])-1,
                     'variable':pred.attrib['arg']}
        predicates.append(predicate)

    # get the relations
    relations = []
    rels = drs.findall('.//rel')
    for rel in rels:
        relation = {'arg1':rel.attrib['arg1'],
                     'arg2':rel.attrib['arg2'],
                     'symbol':rel.attrib['symbol']}
        relations.append(relation)

    # get the identities
    identities = []
    eqs = drs.findall('.//eq')
    for eq in eqs:
        identity = {'arg1':eq.attrib['arg1'],
                    'arg2':eq.attrib['arg2']}
        identities.append(identity)

    return {"predicates" : predicates,
            "relations" : relations,
            "identities" : identities}

'''
    <cond label="l">
     <rel arg1="e1" arg2="x1" symbol="agent" sense="0">
     <indexlist>
     </indexlist>
     </rel>
    </cond>
'''
