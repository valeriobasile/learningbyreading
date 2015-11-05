from requests import post
from lxml import etree, objectify
import logging as log

def tokenize(text):
    # HTTP request
    r = post('http://gingerbeard.alwaysdata.net/candcapi/proxy.php/raw/t', data=text)
    tokenized = r.text.encode("utf-8")
    return tokenized.split(" ")

def boxer(tokenized):
    # HTTP request
    # takes the input text already tokenized
    r = post('http://gingerbeard.alwaysdata.net/candcapi/proxy.php/raw/candcboxer?resolve=true&instantiate=true&roles=proto&integrate=true&format=xml', data=tokenized)
    xml = r.text.encode("utf-8")
    drs = objectify.fromstring(xml)

    # get the tokens and their IDs
    try:
        tagtokens = drs['xdrs']['taggedtokens']['tagtoken']
        token_ids = map(lambda x: x.attrib['{http://www.w3.org/XML/1998/namespace}id'], tagtokens)
    except:
        log.error("boxer(): error getting token IDs")
        return None

    # get the predicates
    try:
        predicates = []
        preds = drs.findall('.//pred')
        for pred in preds:
            try:
                poslist = map(lambda x: token_ids.index(x.text), pred['indexlist']['index'])
            except:
                poslist = [-1]
            predicate = {'token_start' : poslist[0],
                         'token_end' : poslist[-1],
                         'variable' : pred.attrib['arg']}
            predicates.append(predicate)
    except:
        log.error("boxer(): error getting predicates")
        return None

    # get the relations
    try:
        relations = []
        rels = drs.findall('.//rel')
        for rel in rels:
            # transform Boxer relations into URIs
            rel_url = 'http://ns.inria.fr/aloof/boxer/relation#{0}'.format(rel.attrib['symbol'])

            relation = {'arg1':rel.attrib['arg1'],
                         'arg2':rel.attrib['arg2'],
                         'symbol':rel_url}
            relations.append(relation)
    except:
        log.error("boxer(): error getting relations")
        return None

    # get the identities
    try:
        identities = []
        eqs = drs.findall('.//eq')
        for eq in eqs:
            identity = {'arg1':eq.attrib['arg1'],
                        'arg2':eq.attrib['arg2']}
            identities.append(identity)
    except:
        log.error("boxer(): error getting identities")
        return None

    return {"predicates" : predicates,
            "relations" : relations,
            "identities" : identities}
