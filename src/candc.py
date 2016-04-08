from requests import post
from lxml import etree, objectify
import logging as log

# TODO: read this from a config file
BASE_URL = 'http://gingerbeard.alwaysdata.net/candcapi/proxy.php'

def tokenize(text):
    # HTTP request
    r = post('{0}/raw/t'.format(BASE_URL), data=text)
    tokenized = r.text.encode("utf-8")
    return tokenized.split(" ")

def boxer(tokenized, fol=False):
    # HTTP request
    # takes the input text already tokenized
    try:
        if fol:
            r = post('{0}/raw/candcboxer?resolve=true&instantiate=true&roles=verbnet&integrate=true&semantics=fol'.format(BASE_URL), data=tokenized)
            ret = r.text.encode("utf-8")
        else:
            r = post('{0}/raw/candcboxer?resolve=true&instantiate=true&roles=verbnet&integrate=true&format=xml'.format(BASE_URL), data=tokenized)
            xml = r.text.encode("utf-8")
            ret = objectify.fromstring(xml)
    except:
        log.error("boxer(): contacting API")
        return None
    return ret

def get_predicates(drs, token_ids):
    predicates = []
    try:
        preds = drs.findall('.//pred')
        for pred in preds:
            try:
                poslist = map(lambda x: token_ids.index(x.text), pred['indexlist']['index'])
            except:
                poslist = [-1]
            predicate = {'token_start' : poslist[0],
                         'token_end' : poslist[-1],
                         'symbol' : pred.attrib['symbol'],
                         'type' : pred.attrib['type'],
                         'sense' : pred.attrib['sense'],
                         'variable' : pred.attrib['arg']}
            predicates.append(predicate)
    except:
        log.error("boxer(): error getting predicates")
        return None
    return predicates

def get_relations(drs):
    relations = []
    try:
        rels = drs.findall('.//rel')
        for rel in rels:
            # transform Boxer relations into URIs
            rel_url = '<http://ns.inria.fr/aloof/boxer/relation#{0}>'.format(rel.attrib['symbol'])

            relation = {'arg1':rel.attrib['arg1'],
                         'arg2':rel.attrib['arg2'],
                         'symbol':rel_url}
            relations.append(relation)
    except:
        log.error("boxer(): error getting relations")
        return None
    return relations

def get_identities(drs):
    # get the identities
    identities = []
    try:
        eqs = drs.findall('.//eq')
        for eq in eqs:
            identity = {'arg1':eq.attrib['arg1'],
                        'arg2':eq.attrib['arg2']}
            identities.append(identity)
    except:
        log.error("boxer(): error getting identities")
        return None
    return identities

def get_tokens(drs):
    try:
        tagtokens = drs['xdrs']['taggedtokens']['tagtoken']
        token_ids = map(lambda x: x.attrib['{http://www.w3.org/XML/1998/namespace}id'], tagtokens)
    except:
        log.error("boxer(): error getting token IDs")
        return None
    return token_ids

def predicate2folsymbol(predicate):
    if predicate['sense']=='0':
        sense = '1'
    else:
        sense = predicate['sense']
    return "".join((predicate['type'], sense, predicate['symbol']))

def get_fol(tokenized):
    drs = boxer(tokenized)
    fol = boxer(tokenized, fol=True)
    token_ids = get_tokens(drs)
    predicates = get_predicates(drs, token_ids)
    #for predicate in predicates:
    #    print predicate2folsymbol(predicate)
    return fol

def get_all(tokenized):
    # get the tokens and their IDs
    drs = boxer(tokenized)
    token_ids = get_tokens(drs)
    predicates = get_predicates(drs, token_ids)
    relations = get_relations(drs)
    identities = get_identities(drs)

    return {"predicates" : predicates,
            "relations" : relations,
            "identities" : identities}
