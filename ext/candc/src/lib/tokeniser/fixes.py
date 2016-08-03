#!/usr/bin/env python

import sys

def fix_terminator(tokens):
  if not tokens:
    return
  last = tokens[-1]
  if last not in ('.', '?', '!') and last.endswith('.'):
    tokens[-1] = last[:-1]
    tokens.append('.')

def balance_quotes(tokens):
  count = tokens.count("'")
  if not count:
    return
  processed = 0
  for i, token in enumerate(tokens):
    if token == "'":
      if processed % 2 == 0 and (i == 0 or processed != count - 1):
        tokens[i] = "`"
      processed += 1

def output(tokens):
  if not tokens:
    return

#  fix_terminator(tokens)
  balance_quotes(tokens)
  print ' '.join(tokens)

prev = None
for line in sys.stdin:
  tokens = line.split()
  if len(tokens) == 1 and tokens[0] in ('"', "'", ')', ']'):
    prev.append(tokens[0])
  else:
    output(prev)
    prev = tokens

output(prev)
