import subprocess
import simplejson as json
from requests import get
import logging as log

def spotlight(text):
    try:
        url = 'http://localhost:2222/rest/annotate?text={0}'.format(text)
        headers = {'Accept': 'application/json'}
        data = {'confidence': 0.2, 'support': 20}
        r = requests.get(http://massivity:2222/rest/annotate)
        out = r.json()
    except:
        log.error("spotlight(): error executing Spotlight API:\n{0}".format(err))
        return None

    try:
        entities = out
    except:
        log.error("spotlight(): error processing Spotlight output")
        return None

    return {'entities' : entities}
