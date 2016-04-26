import simplejson as json
from requests import post
import logging as log
import ConfigParser

# read configuration
config = ConfigParser.ConfigParser()
config.read('config/disambiguation.conf')

def spotlight(tokenized):
    # mapping character offset to token offset
    t = 0
    tokenindex = [-1 for i in tokenized]
    for index, char in enumerate(tokenized):
        if char == ' ':
            t += 1
        if char != ' ':
            tokenindex[index] = t

    # making the request to Spotlight
    try:
        params = {'text': tokenized}
        headers = {'Accept': 'application/json'}
        data = {'confidence': config.get('spotlight', 'confidence'), 'support': 20, 'text': tokenized}
        r = post(config.get('spotlight', 'url'), params=params, headers=headers, data=data)
        out = r.json()
    except:
        log.error("spotlight(): error executing Spotlight API.")
        return None

    try:
        entities = []
        if 'Resources' in out:
            entities_spotlight = out['Resources']
            for entity in entities_spotlight:
                token_start = tokenindex[eval(entity['@offset'])]
                token_end = tokenindex[eval(entity['@offset'])+len(entity['@surfaceForm'])-1]
                entities.append({'token_start': token_start,
                                 'token_end': token_end,
                                 'entity': entity['@URI']})
    except:
        log.error("spotlight(): error processing Spotlight output")
        return None

    return {'entities' : entities}
