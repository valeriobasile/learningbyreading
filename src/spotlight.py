import simplejson as json
from requests import post
import logging as log

def spotlight(text):
    try:
        url = 'http://massivity:2222/rest/annotate'
        params = {'text': text}
        headers = {'Accept': 'application/json'}
        data = {'confidence': 0.2, 'support': 20, 'text': text}
        r = post(url, params=params, headers=headers, data=data)
        out = r.json()
    except:
        log.error("spotlight(): error executing Spotlight API.")
        return None

    try:
        entities = []
        entities_spotlight = out['Resources']
        for entity in entities_spotlight:
            entities.append({'token_start': entity['@offset'],
                             'token_end': eval(entity['@offset'])+len(entity['@surfaceForm']),
                             'entity': entity['@URI']})
    except:
        log.error("spotlight(): error processing Spotlight output")
        return None

    return {'entities' : entities}


