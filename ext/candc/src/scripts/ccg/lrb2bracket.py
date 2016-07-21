#!/usr/bin/env python

import sys



brackets = {
  '-LRB-': '(',
  '-RRB-': ')',
  '-LCB-': '{',
  '-RCB-': '}',
  '-LSB-': '[',
  '-RSB-': ']',
  }

def trans(x):
  fields = x.split('|')
  if fields[0] in brackets:
    fields[0] = brackets[fields[0]]
    if len(fields) > 1:
      fields[1] = fields[1].strip('-')
  return '|'.join(fields)

for line in sys.stdin:
  print ' '.join(map(trans, line.split()))

