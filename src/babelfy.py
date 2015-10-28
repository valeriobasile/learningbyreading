#!/usr/bin/env python
import subprocess

def babelfy(text):
    process = subprocess.Popen(["java", "-jar", "src/babelfy/babelfy.jar", text],
                               shell=False,
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    out, err = process.communicate(text)
    lines = out.split("\n")[:-1]
    return map(lambda x: {'token_start':eval(x.split('\t')[0]),
                          'token_end':eval(x.split('\t')[1]),
                          'entity':x.split('\t')[2]},
                          #'synset':x.split('\t')[3]},
                          lines)
