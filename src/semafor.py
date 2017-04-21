import logging as log
import subprocess
from os.path import join, dirname, isfile
from os import remove
import tempfile
import ConfigParser
import sys
import simplejson as json

config = ConfigParser.ConfigParser()
config.read(join(dirname(__file__),'../config/semanticparsing.conf'))

# bin/runSemafor.sh /home/vbasile/dev/semafor/in.txt /home/vbasile/dev/semafor/out.txt 1

def parse(text):
    semafor = join(dirname(__file__),'../{0}/bin/runSemafor.sh'.format(config.get('semafor', 'base_dir')))
    input_file = join(dirname(__file__),'../{0}/bin/in.txt'.format(config.get('semafor', 'base_dir')))
    with open(input_file, 'w') as f:
        # TODO: call Punkt (NLTK) and write one sentence per line
        f.write(text)
    output_file = join(dirname(__file__),'../{0}/bin/out.txt'.format(config.get('semafor', 'base_dir')))
    if isfile(output_file):
        remove(output_file)
    process = subprocess.Popen([semafor, input_file, output_file, '1'],
                           shell=False)
    out, err = process.communicate(text)
    if err:
        log.debug('Semafor output: {0}'.format(err))
    with open(output_file) as f:
        semafor_output = json.load(f)

    # process the output from Semafor
    predicates = dict()
    relations = []
    for frame in semafor_output['frames']:
        # predicate from frame type
        for span in frame['target']['spans']:
            variable_frame = 'x{0}-{1}'.format(span['start'], span['end'])
            predicate_frame = {'token_end': span['end']-1,
                         'token_start': span['start'],
                         'symbol': span['text'],
                         'sense': '0',
                         'variable': variable_frame,
                         'type': 'v'}
            if not variable_frame in predicates:
                predicates[variable_frame] = predicate_frame

        # predicates from frame elements
        for frame_element in frame['annotationSets'][0]['frameElements']:
            for span in frame_element['spans']:
                variable = 'x{0}-{1}'.format(span['start'], span['end'])
                predicate = {'token_end': span['end']-1,
                             'token_start': span['start'],
                             'symbol': span['text'],
                             'sense': '0',
                             'variable': variable,
                             'type': 'n'}
                if not variable in predicates:
                    predicates[variable] = predicate

            relation = {'arg1': variable_frame,
                        'arg2': variable,
                        'symbol': frame_element['name']}
            relations.append(relation)

    semantics = {'predicates': predicates.values(),
     'namedentities': [],
     'identities': [],
     'relations': relations}

    return semantics, ' '.join(semafor_output['tokens'])

# now we need to map this:
'''
{'frames': [
   {'target': {'name': 'Inclusion', 'spans': [{'start': 1, 'end': 2, 'text': 'contains'}]},
      'annotationSets': [{'frameElements': [
        {'name': 'Part', 'spans': [{'start': 2, 'end': 3, 'text': 'water'}]},
        {'name': 'Total', 'spans': [{'start': 0, 'end': 1, 'text': 'Ajoblanco'}]}],
      'score': 30.12078744665738,
      'rank': 0}]},
   {'target': {'name': 'Natural_features', 'spans': [{'start': 2, 'end': 3, 'text': 'water'}]},
      'annotationSets': [{'frameElements': [
        {'name': 'Locale', 'spans': [{'start': 2, 'end': 3, 'text': 'water'}]}],
        'score': 36.96106426962915, 'rank': 0}]}],
 'tokens': ['Ajoblanco', 'contains', 'water', 'and', 'is', 'from', 'Spain', '.']}


'''
# into this

'''
{'predicates': [
   {'token_end': 1, 'token_start': 1, 'symbol': 'contain', 'sense': '0', 'variable': 'e1', 'type': 'v'},
   {'token_end': 2, 'token_start': 2, 'symbol': 'water', 'sense': '0', 'variable': 'x2', 'type': 'n'}],
 'namedentities': [
   {'token_end': 6, 'token_start': 6, 'symbol': 'spain', 'variable': 'x3', 'type': 'nam', 'class': 'geo'},
   {'token_end': 0, 'token_start': 0, 'symbol': 'ajoblanco', 'variable': 'x1', 'type': 'nam', 'class': 'org'}],
 'identities': [],
 'relations': [
   {'arg1': 'x1', 'arg2': 'x3', 'symbol': 'from'},
   {'arg1': 'e1', 'arg2': 'x2', 'symbol': 'Co-Theme'},
   {'arg1': 'e1', 'arg2': 'x1', 'symbol': 'Theme'}]}
'''
