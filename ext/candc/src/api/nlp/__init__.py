# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

from base import *

import config
import io
import model
import tagger
import ccg

def load(super, parser, load_model = True):
  int_cfg = ccg.IntegrationConfig()
  super_cfg = tagger.SuperConfig()
  super_cfg.path.value = super
  parser_cfg = ccg.ParserConfig()
  parser_cfg.path.value = parser
  return ccg.Integration(int_cfg, super_cfg, parser_cfg, Sentence())

def read(sent, s):
  tokens = [tuple(x.split('|')) for x in s.split()]
  sent.words = [t[0] for t in tokens]
  sent.pos = [t[1] for t in tokens]
  sent.msuper = [[t[2]] for t in tokens]
