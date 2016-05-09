from requests import post
from lxml import etree, objectify
import logging as log
import ConfigParser
import subprocess
from os.path import join, dirname

config = ConfigParser.ConfigParser()
config.read(join(dirname(__file__),'../config/boxer.conf'))

def tokenize(text):
    if config.get('boxer', 'mode') == 'local' or config.get('boxer', 'mode') == 'soap':
        return tokenize_local(text)
    elif config.get('boxer', 'mode') == 'online':
        return tokenize_online(text)

def boxer(tokenized, fol=False, drg=False):
    if config.get('boxer', 'mode') == 'local' or config.get('boxer', 'mode') == 'soap':
        return boxer_local(tokenized, fol, drg)
    elif config.get('boxer', 'mode') == 'online':
        return boxer_online(tokenized, fol, drg)

def tokenize_local(text):
    tokenizer = join(dirname(__file__),'../{0}/bin/t'.format(config.get('local', 'base_dir')))
    process = subprocess.Popen([tokenizer, '--stdin'],
                           shell=False,
                           stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
    out, err = process.communicate(text)
    if err:
        log.error('Tokenizer error: {0}'.format(err))
    tokenized = out.decode('utf-8').encode("utf-8")
    return tokenized.split(" ")

def parse_local(tokenized):
    parser_options = ['--models', join(dirname(__file__),'../{0}/models/boxer'.format(config.get('local', 'base_dir'))),
                      '--candc-printer', 'boxer']
    parser = '{0}/bin/candc'.format(config.get('local', 'base_dir'))
    process = subprocess.Popen([parser] + parser_options,
                           shell=False,
                           stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
    out, err = process.communicate(tokenized)
    if err:
        # C&C writes info on the stderr, we want to ignore it
        if not err.startswith('#'):
            log.error('Parser error: {0}'.format(err))
    parsed = out.decode('utf-8').encode("utf-8")
    return parsed

def parse_soap(tokenized):
    parser_options = ['--url', '{0}:{1}'.format(config.get('soap', 'soap_url'), config.get('soap', 'soap_port'))]
    parser = join(dirname(__file__),'../{0}/bin/soap_client'.format(config.get('local', 'base_dir')))
    process = subprocess.Popen([parser] + parser_options,
                           shell=False,
                           stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
    out, err = process.communicate(tokenized)
    if err:
        # C&C writes info on the stderr, we want to ignore it
        if not err.startswith('#'):
            log.error('Parser error: {0}'.format(err))
    parsed = out.decode('utf-8').encode("utf-8")
    return parsed

def get_boxer_options():
    parameters = config.options('options')
    options = {parameter: config.get('options', parameter) for parameter in parameters}
    if config.get('boxer', 'mode') == 'local' or config.get('boxer', 'mode') == 'soap':
        option_list = ['--{0} {1}'.format(parameter, value) for parameter, value in options.iteritems()]
        return ' '.join(option_list)
    elif config.get('boxer', 'mode') == 'online':
        option_list = ['{0}={1}'.format(parameter, value) for parameter, value in options.iteritems()]
        return '&'.join(option_list)

def boxer_local(tokenized, fol=False, drg=False):
    if config.get('boxer', 'mode') == 'local':
        parsed = parse_local(tokenized)
    elif config.get('boxer', 'mode') == 'soap':
        parsed = parse_soap(tokenized)

    if fol:
        boxer_options = ['--stdin',
                         '--semantics', 'fol']
    elif drg:
        boxer_options = ['--stdin',
                         '--resolve', 'true',
                         '--semantics', 'drg']
    else:
        boxer_options = ['--stdin',
                         '--instantiate', 'true',
                         '--format', 'xml']
        boxer_options.extend(get_boxer_options().split(' '))

    boxer = join(dirname(__file__),'../{0}/bin/boxer'.format(config.get('local', 'base_dir')))

    process = subprocess.Popen([boxer] + boxer_options,
                           shell=False,
                           stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
    out, err = process.communicate(parsed)

    if err:
        # Boxer throws a silly error every time (a bug), we want to ignore it
        if not "No source location" in err:
            log.error('Boxer error: {0}'.format(err))
    boxed = out.decode('utf-8').encode("utf-8")
    return boxed

def tokenize_online(text):
    # HTTP request
    r = post('{0}/raw/t'.format(config.get('online', 'http_url')), data=text)
    tokenized = r.text.decode('utf-8').encode("utf-8")
    return tokenized.split(" ")

def boxer_online(tokenized, fol=False, drg=False):
    # HTTP request
    # takes the input text already tokenized
    boxer_options = get_boxer_options()
    try:
        if fol:
            r = post('{0}/raw/candcboxer?instantiate=true&semantics=fol&{1}'.format(config.get('online', 'http_url'), boxer_options), data=tokenized)
        elif drg:
            r = post('{0}/raw/candcboxer?instantiate=true&semantics=drg&{1}'.format(config.get('online', 'http_url'), boxer_options), data=tokenized)
        else:
            r = post('{0}/raw/candcboxer?instantiate=true&format=xml&{1}'.format(config.get('online', 'http_url'), boxer_options), data=tokenized)
    except:
        log.error("boxer(): contacting API")
        return None
    return r.text.decode('utf-8').encode("utf-8")

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

def get_named(drs, token_ids):
    namedentities = []
    try:
        nameds = drs.findall('.//named')
        for named in nameds:
            try:
                poslist = map(lambda x: token_ids.index(x.text), named['indexlist']['index'])
            except:
                poslist = [-1]
            namedentity = {'token_start' : poslist[0],
                         'token_end' : poslist[-1],
                         'symbol' : named.attrib['symbol'],
                         'type' : named.attrib['type'],
                         'class' : named.attrib['class'],
                         'variable' : named.attrib['arg']}
            namedentities.append(namedentity)
    except:
        log.error("boxer(): error getting named entities")
        return None
    return namedentities

def get_relations(drs):
    relations = []
    try:
        rels = drs.findall('.//rel')
        for rel in rels:
            # transform Boxer relations into URIs
            rel_url = rel.attrib['symbol']

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
    fol = boxer(tokenized, fol=True)
    return fol.split('\n')[-2]

def get_drg(tokenized):
    drg = boxer(tokenized, drg=True)
    lines = [line+'\n' for line in drg.split('\n') if not (line.startswith('%') or len(line)==0)]
    return lines

def get_all(tokenized):
    # get the tokens and their IDs
    try:
        drs = objectify.fromstring(boxer(tokenized))
    except:
        log.error("cannot read Boxer XML")
        return None
    token_ids = get_tokens(drs)
    predicates = get_predicates(drs, token_ids)
    namedentities = get_named(drs, token_ids)
    relations = get_relations(drs)
    identities = get_identities(drs)

    return {"predicates" : predicates,
            "namedentities" : namedentities,
            "relations" : relations,
            "identities" : identities}
