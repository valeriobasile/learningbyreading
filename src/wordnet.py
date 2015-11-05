from nltk.corpus import wordnet
import logging as log

def get_lemma(synset_id):
    try:
        wn_id = synset_id.split(':')[1]
        offset = int(wn_id[:-1])
        pos = wn_id[-1]
        print offset
        print pos
        return wordnet._synset_from_pos_and_offset(pos, offset)
    except:
        log.error("get_lemma(): error looking up synset id {0} in NLTK WordNet".format(synset_id))
        return None
