#!/usr/bin/env python2.5

import sys
import os
import re
from collections import defaultdict

CCGbank = sys.argv[1]
MARKEDUP = sys.argv[2]

LEAF = re.compile(r'<L[^>]* ([^ >]*) ([^>]*)>')
VAR = re.compile(r'_\d+')
MISSING = re.compile(r'([^}])([/\\)])')
DEPS = re.compile(r'<[0-9]>')
BARE_S = re.compile(r'S([^\[])')
MARKEDUP_VARS = re.compile(r'\{([_A-Z])\*?\}')

ORDER = "YZWVUTRQABCDEFGHIJKLMNOP"

def markup(ccgbank):
  try:
    vars = defaultdict(iter(ORDER).next)
    map(lambda match: vars[match.group(0)], VAR.finditer(ccgbank))
    markedup = VAR.sub(lambda match: '{%s}' % vars[match.group(0)], ccgbank)
    markedup = markedup.replace('}:B', '*}').replace('}:U', '*}')
    markedup = MISSING.sub(r'\g<1>{_}\g<2>', markedup)
    markedup = BARE_S.sub(r'S[X]\g<1>', markedup)
    if '/' in ccgbank or '\\' in ccgbank:
      markedup = '(%s){_}' % markedup
    else:
      markedup += '{_}' 
    raw = VAR.sub('', ccgbank).replace(':B', '').replace(':U', '')
    return raw, markedup 
  except StopIteration:
    return None, None

lexicon = {}
cats = {}
counts = defaultdict(int)
def extract_markedup(f, cats, counts):
  for line in open(f, 'rU'):
    for match in LEAF.finditer(line):
      word = match.group(1)
      raw, markedup = markup(match.group(2))
      if raw in cats:
        if cats[raw] != markedup:
          print raw, cats[raw], markedup
      else:
        cats[raw] = markedup
      counts[raw] += 1

AUTO = os.path.join(CCGbank, 'data/AUTO')
for section in sorted(os.listdir(AUTO)):
  if not section.isdigit():
    continue
  section = os.path.join(AUTO, section)
  for f in sorted(os.listdir(section)):
    if not f.endswith('.auto'):
      continue
    extract_markedup(os.path.join(section, f), cats, counts)
    
#for freq, raw, markedup in sorted(map(lambda x: (counts[x[0]], x[0], x[1]), cats.iteritems())):
#  print 'corpus', freq, raw, markedup

def check_markedup(markedup):
  vars = MARKEDUP_VARS.findall(markedup)
  seen = {}
  vars = [seen.setdefault(i, i) for i in vars if i != '_' and i not in seen]
  if not ORDER.startswith(''.join(vars)):
    print 'order', markedup

raw = None
for line in open(MARKEDUP, 'rU'):
  line = line.rstrip()
  if line.startswith(('#', '=')) or not line:
    continue

  if line.startswith(' '):
    if raw:
      ndeps, entry = line.split()
      markedup = DEPS.sub('', entry)
      check_markedup(entry)
      if raw not in cats:
        print 'miss', raw
      elif cats[raw] != markedup:
        print 'diff', raw
        print '  ', cats[raw]
        print '  ', markedup
      raw = None
  else:
    raw = line

