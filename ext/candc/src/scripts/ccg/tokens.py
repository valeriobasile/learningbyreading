#!/usr/bin/env python

import sys, re

TOKENS = re.compile(r'\([^ ()]+ [^ ()]+\)')

for line in sys.stdin:
  print ' '.join(TOKENS.findall(line))

