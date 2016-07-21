#!/usr/bin/env python

import sys
import os

from ccg import *
from ccg.deriv import Leaf as L, Rule as R, Pattern

import optparse

parser = optparse.OptionParser()
parser.add_option('-c', '--colour', dest='colour', help='force match highlighting', action='store_true', default=False)
parser.add_option('-m', '--matcher', dest='matcher', help='the location of matches [parents|children|all]', default='parents')
parser.add_option('-f', '--format', dest='format', help='output format [ccgbank|tree|stags|context]', default='tree')
parser.add_option('-d', '--dataset', dest='dataset', help='dataset [all|dev|train|test]', default='all')

parser.add_option('-v', '--verbose', help='print the section being scanned', action='store_true', default=False)

(opts, args) = parser.parse_args()

pattern = Pattern(eval(args[0]))

MATCHERS = dict(
  parents=pattern.findparents,
  children=pattern.findchildren,
  all=pattern.findall,
  lca=pattern.findlca,
  span=pattern.findspan,
  nospan=pattern.findnospan,
)
matcher = MATCHERS[opts.matcher]

FORMATS = dict(
  bank=lambda deriv, node: str(deriv),
  bank_frag=lambda deriv, node: str(node),
  tree=lambda deriv, node: repr(deriv),
  tree_frag=lambda deriv, node: repr(node),
  stags=lambda _, y: y.stags(),
  context=lambda x, y: x.context(y.start, y.start + y.span)
)
format = FORMATS[opts.format]

DATASETS = dict(
  all=None,
  dev=bank.dev,
  train=bank.train,
  test=bank.test
)
dataset = DATASETS.get(opts.dataset, opts.dataset)

cfg.SHOW_COLOUR = opts.colour or os.isatty(1)

for (deriv, nodes) in iterators.match(matcher, bank.iter('../data/CCGbank1.2', dataset, opts.verbose)):
  for node in nodes:
    if cfg.SHOW_COLOUR:
      node.colour = colours.GREEN
    print format(deriv, node)
