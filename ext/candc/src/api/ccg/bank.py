
import sys
import types

import os

import cfg
import deriv

import re
NODES_RE = re.compile(r'\(<T ([^ ]+) ([0-9]) ([0-9])>|\(<L ([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+)>\)')
ID_RE = re.compile(r'ID=([^ ]+) PARSER=GOLD NUMPARSE=1')
del re

def parse(line, leaf, unary, binary):
  def _parse(nodes, leaf, unary, binary):
    cat, head, nchildren, cat2, token, ptb, pos, markedup = nodes.next().groups()
    if cat2:
      _parse.nleaves += 1
      return leaf(cat2, token, ptb, pos, markedup, _parse.nleaves - 1)

    head, nchildren = int(head), int(nchildren)

    if cat:
      left = _parse(nodes, leaf, unary, binary)
      if nchildren == 1:
        return unary(cat, head, left)

      right = _parse(nodes, leaf, unary, binary)
      return binary(cat, head, left, right)

    raise SyntaxError, "unexpected node %s" % str(next)

  _parse.nleaves = 0

  nodes = NODES_RE.finditer(line)
  try:
    tree = _parse(nodes, leaf, unary, binary)
  except StopIteration:
    raise SyntaxError, "unexpected finish"

  try:
    next = nodes.next()
    raise SyntaxError, "extra nodes %s" % str(next)
  except StopIteration:
    pass

  return tree

def tuples(line, id, filename):
  def _tuple(*args):
    return tuple(args)
  return parse(line, _tuple, _tuple, _tuple)

def nodes(line, id, filename):
  return parse(line, deriv.Leaf, deriv.Rule, deriv.Rule)

def derivs(line, id, filename):
  return deriv.Deriv(parse(line, deriv.Leaf, deriv.Rule, deriv.Rule), id, filename)

def load(filename, method=derivs):
  id = None
  for line in open(filename, 'rU'):
    if line.startswith('(<'):
      if id is None:
        raise SyntaxError, "parse is not preceded by an ID line"
      yield method(line.strip(), id, filename)
      id = None
    elif line.startswith('ID='):
      match = ID_RE.match(line)
      if not match:
        raise SyntaxError, "could not parse ID line in %s" % filename
      id = match.group(1)

def dev(filename):
  return filename.startswith('wsj_00')

import re
TRAIN_RE = re.compile(r'wsj_(?:0[2-9]|1[0-9]|2[01])')
del re

def train(filename):
  return TRAIN_RE.match(filename)

def test(filename):
  return filename.startswith('wsj_23')

def iter(base, dataset=None, verbose=False, method=derivs):
  if dataset is None:
    keep = lambda x: True
  elif type(dataset) is types.FunctionType:
    keep = dataset
  else:
    keep = lambda x: dataset in x

  for section in xrange(25):
    path = os.path.join(base, 'data/AUTO/%02d' % section)
    if verbose:
      print >> sys.stderr, path

    filenames = [x for x in os.listdir(path) if x.endswith('.auto') and keep(x)]
    for filename in sorted(filenames):
      for t in load(os.path.join(path, filename), method):
        yield t
