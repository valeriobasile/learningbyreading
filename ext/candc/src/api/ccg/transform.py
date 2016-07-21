#!/usr/bin/env python

import sys
import os

from ccg import *

TRANSFORM = sys.argv[1]
transform = trans.__dict__[TRANSFORM]

FILTER = ''
if len(sys.argv) == 3:
  FILTER = sys.argv[2]
  if FILTER in ['dev', 'train', 'test']:
    FILTER = bank.__dict__[FILTER]

for deriv in bank.visit(transform, bank.iter('../data/CCGbank1.2', FILTER)):
  print deriv.stags()
