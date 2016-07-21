#!/usr/bin/env python
# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

import sys
import re
import colours

class MissingCat(Exception):
  pass

BRACKETS = {
  '(': '-LRB-',
  ')': '-RRB-',
  '{': '-LCB-',
  '}': '-RCB-',
  '[': '-LSB-',
  ']': '-RSB-',
  }

class PTB:
  def __init__(self, ccg, label, *kids):
    self.ccg = ccg
    self.label = label
    self.word = None
    self.kids = []
    for k in kids:
      if k.label:
        self.kids.append(k)
      else:
        self.kids.extend(k.kids)

  def clone(self):
    new = PTB(self.ccg, self.label, *self.kids)
    new.word = self.word
    return new

  @staticmethod
  def dup_left(ccg, *kids):
    label = kids[0].label
    return PTB(ccg, label, *kids)

  @staticmethod
  def LEAF(label, ccg, word):
    if label == 'RRB':
      label = '-RRB-'
    elif label == 'LRB':
      label = '-LRB-'
    elif (label == 'IN' and word in [':', '--', '-']) or label == ';':
      label = ':'
    elif label == 'SO':
      label = 'RB'
    res = PTB(ccg, label)
    res.word = BRACKETS.get(word, word)
    return res
    
  def addleft(self, ccg, node):
    self.ccg = ccg
    if node.label:
      self.kids.insert(0, node)
    else:
      self.kids[0:0] = node.kids
    return self

  def addright(self, ccg, node):
    self.ccg = ccg
    if node.label:
      self.kids.append(node)
    else:
      self.kids.extend(node.kids)
    return self

  def kids_labels(self):
    return ' '.join(['%s|%s' % (k.label, k.word) for k in self.kids])

  insert_QP_RE = re.compile('\$|RB.*CD|IN[^ ]* \$|IN[^ ]* CD[^ ]* NNS')
  ADJP_to_QP_RE = re.compile('CD[^ ]* CD|IN[^ ]* CD')
  def add_qp(self):
    for k in self.kids:
      k.add_qp()
    
    if self.label in ['NP', 'ADJP']:
      kids = self.kids_labels()
      if len(self.kids) > 2:
        if self.insert_QP_RE.match(kids):
          self.kids = [PTB(self.ccg, 'QP', *self.kids)]
      elif self.ADJP_to_QP_RE.match(kids):
        if self.label == 'NP':
          self.kids = [PTB(self.ccg, 'QP', *self.kids)]
        else:
          self.label = 'QP'

  # add ADVP nodes above RBs inside PP
  def add_rb_advp(self, parent = None):
    for k in self.kids:
      k.add_rb_advp(self.label)

    if self.label == 'RB' and parent == 'PP':
      self.kids = [self.clone()]
      self.label = 'ADVP'
      self.word = None
      
  def add_ucp(self):
    for k in self.kids:
      k.add_ucp()
    
    if len(self.kids) == 3 and self.kids[1].label == 'CC':
      if self.kids[0].label != self.kids[2].label and self.label != 'PRN':
        self.label = 'UCP'

  # remove PP nested inside PP nodes
  def flatten(self):
    for k in self.kids:
      k.flatten()

    kids = self.kids
 
    if self.label == 'PP' and len(kids) >= 2 and kids[1].label == 'PP' \
       and kids[0].word and kids[0].word.lower() == 'because':
      self.kids[1].label = None

  def __str__(self):
    if self.label is None:
      return ' '.join(map(str, self.kids))
    if self.word:
      return '(%s %s)' % (self.label, self.word)
    else:
      return '(%s %s)' % (self.label, ' '.join(map(str, self.kids)))

  def __repr__(self):
    if self.word:
      return '(%s %s)' % (self.label, self.word)
    else:
      return '(%s %s)' % (self.label, ' '.join(map(repr, self.kids)))

  def evalb(self):
    label = self.label

    if not label:
      return ' '.join(map(lambda x: x.evalb(), self.kids))
    
    if label == 'IN' and self.word in ['--', ':']:
      label = ':'

    if MODE == '-t':
      if label.startswith('S['):
        if 'S[frg]' in label:
          label = 'FRAG'
        elif 'S[q]' in label or 'S[wq]' in label:
          label = 'SQ'
        else:
          label = 'S'
      if '/' in label or '\\' in label or '[' in label:
        label = 'MISS'
    if self.word:
      return '(%s %s)' % (label, self.word)
    else:
      return '(%s %s)' % (label, ' '.join(map(lambda x: x.evalb(), self.kids)))

class Node:
  def __init__(self, orig):
    self.orig = orig
    self.cat = None
  def __str__(self):
    return self.orig
  def paren(self):
    if '/' in self.cat or '\\' in self.cat:
      return '(' + self.cat + ')'
    else:
      return self.cat
  def find(self, cat):
    return self.cat == cat

class TNode(Node):
  # (<T *** NP\NP * 0 1>
  def __init__(self, orig):
    Node.__init__(self, orig)
    fields = orig[4:-1].split(' ')
    try:
      self.cat, self.head, self.nchildren = fields
    except ValueError:
      print >> sys.stderr, fields
      sys.exit(1)
    if self.cat.endswith('[conj]'):
      self.cat = self.cat[:-6]
      self.cat = "%s\\%s" % (self.paren(), self.paren())
    self.cat = self.cat.replace('[nb]', '')

    self.leaf = False
    self.head = int(self.head)
    self.nchildren = int(self.nchildren)
    self.l = None
    self.r = None

  def sentence(self):
    if self.r:
      return self.l.sentence() + self.r.sentence()
    else:
      return self.l.sentence()

  def ccgtree(self):
    if self.r:
      return '(%s %s %s)' % (self.cat, self.l.ccgtree(), self.r.ccgtree())
    else:
      return '(%s %s)' % (self.cat, self.l.ccgtree())

  def ptb_bracket(self, label):
    if self.r:
      return PTB(self, label, self.l.ptb(), self.r.ptb())
    else:
      return PTB(self, label, self.l.ptb())

  def ptb(self, label = None):
    if self.r:
      return RULES.get((self.l.cat, self.r.cat), missing)(self)
    else:
      return UNARY.get((self.l.cat, self.cat), missing_unary)(self)

  def find(self, cat):
    if Node.find(self, cat) or self.l.find(cat):
      return True
    return self.r and self.r.find(cat)

class LNode(Node):
  # (<L N/N JJ JJ nonexecutive N_28/N_28>)
  def __init__(self, orig):
    Node.__init__(self, orig)
    fields = orig[4:-1].split(' ')
    try:
      self.cat, self.ccg_pos, self.ptb_pos, self.word, self.vars = fields
    except ValueError:
      print >> sys.stderr, fields, orig
      sys.exit(1)
    self.leaf = True

  def sentence(self):
    return [ self.word ]

  def count_rules(self, rules):
    return

  def ccgtree(self):
    return '(%s %s)' % (self.cat, self.word)

  def ptb_leaf(self):
    return PTB(self, None, PTB.LEAF(self.ptb_pos, self, self.word))

  def ptb_bracket(self, label):
    return PTB(self, label, self.ptb_leaf())

  def ptb(self):
    if self.ccg_pos in POS:
      return self.ptb_bracket(POS[self.ccg_pos])
    elif self.cat in LEAVES:
      leaf = LEAVES[self.cat]
      if leaf.startswith('? '):
        fields = leaf[2:].split()
        pos = fields[0]
        if self.ccg_pos == pos:
          leaf = fields[1]
        elif len(fields) == 3:
          leaf = fields[2]
        else:
          return self.ptb_leaf()
      elif leaf.startswith('! '):
        fields = leaf[2:].split()
        leaf = fields[0]
        words = fields[1:]
        if self.word in words or self.ccg_pos in words:
          return self.ptb_leaf()
      elif leaf == 'sfs':
        if self.ccg_pos == 'RB':
          return self.ptb_bracket('ADVP')
        elif self.ccg_pos.startswith('NN'):
          return self.ptb_bracket('NP')
        else:
          return self.ptb_leaf()
      elif leaf == 'special_pp_prn':
        if self.ccg_pos in ['LRB', ':'] or self.word in ['--']:
          return self.ptb_bracket('PRN')
        elif self.word == ':':
          return self.ptb_leaf()
        elif self.ccg_pos == 'RB':
          return self.ptb_bracket('ADVP')
        else:
          return self.ptb_bracket('PP')
      elif leaf == 'special_SbNPbSbNP':
        if self.word in ["n't", 'not']:
          return self.ptb_leaf()
        elif self.word == '%':
          leaf = 'NP'
        else:
          leaf = 'ADVP'
      elif leaf == 'special_fPP':
        leaf = 'PP'
        if self.ccg_pos == 'RB':
          leaf = 'ADVP'
        elif self.ccg_pos == 'JJ':
          leaf = 'ADJP'
      return self.ptb_bracket(leaf)
    else:
      return self.ptb_leaf()

CCGBANK_NODES = re.compile(r'\(<T[^>]+>|\(<L[^>]+>|\)')
class Tree:
  def __init__(self, line):
    if line:
      tokens = CCGBANK_NODES.findall(line)
      self.root = self.parse(tokens)
    else:
      self.root = None

  def parse(self, tokens):
    top = tokens.pop(0)
    if top.startswith('(<T'):
      node = TNode(top)
      node.l = self.parse(tokens)
      if node.nchildren == 2:
        node.r = self.parse(tokens)      
    elif top.startswith('(<L'):
      node = LNode(top)
    else:
      raise 'unexpected line "%s"' % top

    top = tokens.pop(0)
    if top != ')':
      raise 'expected closing parenthesis'

    return node

  def sentence(self):
    return self.root.sentence()

  def count_rules(self, rules):
    self.root.count_rules(rules)

  def ccgtree(self):
    if self.root:
      return self.root.ccgtree()
    else:
      return ''

  def ptbtree(self):
    if not self.root:
      return ''
    root = self.root.ptb()
    if not root.label:
      root.label = 'MISS'
    root.add_qp()
    root.add_ucp()
    root.add_rb_advp()
    root.flatten()
    return root

rules = {}
lines = []
count = 0

def ignore_preface(lines):
  in_preface = True
  for line in lines:
    if in_preface:
      in_preface = line != '\n'
      continue
    yield line

MODE = sys.argv[1]
CCGBANK = sys.argv[2]
PTBANK = sys.argv[3]
RULES_FILE = sys.argv[4]
UNARY_FILE = sys.argv[5]
LEAVES_FILE = sys.argv[6]

DECL = re.compile(r'^\(+S\[[^a\]]+\]\\NP(?:\[[^\]]+\])?\)')

def default_leaf_cases(LEAVES, leaf):
  if DECL.match(leaf):
    LEAVES[leaf] = 'VP'
  pass

LEAVES = {}
for line in ignore_preface(open(LEAVES_FILE)):
  if line.startswith('#') or not line.strip():
    continue
  fields = line.split()
  leaf = fields[0]
  if len(fields) >= 2:
    LEAVES[leaf] = ' '.join(fields[1:])
  else:
    default_leaf_cases(LEAVES, leaf)

POS = {'RP': 'PRT'}

# temporarily removed the rule NP NP\NP ! NP S[ng]\NP

TOKENS = re.compile('([A-Za-z_]+|[^ ])')
SIMPLE = {
  'l': '_n.l.ptb()',
  'r': '_n.r.ptb()',
  'll': '_n.l.l.ptb()',
  'lr': '_n.l.r.ptb()',
  'rl': '_n.r.l.ptb()',
  'rr': '_n.r.r.ptb()',
  '<': '_n.l.ptb().addright(_n, _n.r.ptb())',
  '>': '_n.r.ptb().addleft(_n, _n.l.ptb())',
  '-': 'PTB(_n, None, _n.l.ptb(), _n.r.ptb())',
}

def special_sinv(n):
  label = 'S'
  if n.r.find('(S[dcl]\S[dcl])/NP'):
    label = 'SINV'
  return PTB(n, label, n.l.ptb(), n.r.ptb())

def special_NPbNP(n):
  l = n.l.ptb()
  r = n.r.ptb()
  if l.kids[-1].label == 'PP' and r.label in ['PP', 'SBAR']:
    return l.addright(n, r)
  return PTB(n, 'NP', l, r)

def special_ng_adv(n):
  if n.l.leaf:
    return n.l.ptb().addright(n, PTB(n, 'S', n.r.ptb()))
  
  ll = n.l.l.ptb()
  lr = n.l.r.ptb()
  r = n.r.ptb()
  return ll.addright(n, PTB(n, 'S', lr, r))

def special_control_structure(n):
  if n.l.leaf:
    return n.l.ptb().addright(n, n.r.ptb())
  
  ll = n.l.l.ptb()
  lr = n.l.r.ptb()
  r = n.r.ptb()
  return ll.addright(n, r.addleft(n, lr))

def special_conj(n):
  if not n.r.leaf and n.r.l.cat == ',' and n.r.r.cat != n.r.cat:
    return n.l.ptb().addright(n, strip(n.r.ptb()))

  return n.l.ptb().addright(n, n.r.ptb())

def special_nfn(n):
  if not n.r.leaf and n.r.l.cat == r'((N/N)\(N/N))/(N/N)':
    return n.r.ptb().addleft(n, n.l.ptb())
  else:
    return PTB(n, None, n.l.ptb(), n.r.ptb())

def special_rel(n):
  if n.l.leaf and n.l.ccg_pos in ['LRB', ':']:
      return PTB(n, 'PRN', n.l.ptb(), n.r.ptb())
  return PTB(n, 'SBAR', n.l.ptb(), PTB(n, 'S', n.r.ptb()))

SPECIAL = set(['special_sinv', 'special_NPbNP', 'special_ng_adv',
               'special_control_structure', 'special_conj',
               'special_nfn', 'special_rel'])

def strip(node):
  node.label = None
  return node

def missing(node):
  print >> missing_log, 'RULE:', node.l.cat, node.r.cat
  MISSED.add((node.l.cat, node.r.cat))
  return node.ptb_bracket(node.cat)

def missing_unary(node):
  print >> missing_log, 'UNARY:', node.l.cat, node.cat
  MISSED.add(('unary', node.l.cat, node.cat))
  return node.ptb_bracket(node.cat)

def used(ccg, ptb):
  if ccg.l and ccg.r:
    USED.add(('B', ccg.l.cat, ccg.r.cat))
  else:
    USED.add(('U', ccg.l.cat, ccg.cat))
  return ptb

def compile(template):
  template = template.strip()
  if template == '':
    return missing
  try:
    python = parse(TOKENS.findall(template))
  except SyntaxError, e:
    raise SyntaxError('%s in template %s' % (e.message, template))
  return eval("lambda _n: used(_n, %s)" % python)

def parse(tokens):
  t = tokens.pop(0)
  if t in '([{':
    if t == '(':
      t = tokens.pop(0)
      args = parse_args(tokens, ')')
      if t == 'dl':
        return 'PTB.dup_left(_n, %s)' % ', '.join(args)
      label = "'%s'" % t
    elif t == '[':
      label = 'None'
      args = parse_args(tokens, ']')
    elif t == '{':
      args = parse_args(tokens, '}')
      if len(args) != 1:
        raise SyntaxError, "can only strip a single child"
      return 'strip(%s)' % args[0]
    return 'PTB(_n, %s, %s)' % (label, ', '.join(args))
  elif t in SIMPLE:
    return SIMPLE[t]
  elif t in SPECIAL:
    return t + '(_n)'
  else:
    raise SyntaxError, "unknown token '%s'" % t

def parse_args(tokens, delimiter):
  args = []
  while tokens:
    if tokens[0] == delimiter:
      tokens.pop(0)
      return args
    args.append(parse(tokens))
  raise SyntaxError, "missing closing '%s'" % delimiter

FWD_COMP = re.compile(r'.*/([^ ]+) \1/')
BWD_COMP = re.compile(r'([^ ]+)\\[^ ]+ [^ ]+\\\1$')
BWD_CROSS = re.compile(r'([^ ]+)/[^ ]+ [^ ]+\\\1$')

def default_rule_cases(l, r):
  if l == 'conj' and 'conj' not in r:
    return compile('-')
  if DECL.match(l):
    if l.endswith('/' + r) or l.endswith('/(' + r + ')'):
      return compile('<')
    if r == '(S\NP)\(S\NP)':
      return compile('<')
  if l == 'S/(S\NP)' and DECL.match(r):
    return compile('<')
  if l == '(' + r + ')/(' + r + ')':
    return compile('>')
  if r == '(' + l + r')\(' + l + ')':
    return compile('<')

  # extra cases break some tokens somehow for non-complex results
  if l.endswith('/' + r):
    return compile('<')
  if r.endswith(')\\' + l):
    return compile('>')

  pair = '%s %s' % (l, r)

  if BWD_COMP.match(pair):
    return compile('<')

  if BWD_CROSS.match(pair):
    return compile('<')

  if FWD_COMP.match(pair):
    return compile('>')

  return missing  

RULES = {}
for line in ignore_preface(open(RULES_FILE)):
  if line.startswith('# ') or not line.strip():
    continue
  fields = line.split()
  (l, r) = fields[:2]
  if len(fields) >= 3:
    template = ' '.join(fields[2:])
    try:
      RULES[(l, r)] = compile(template)
    except SyntaxError, e:
      raise SyntaxError, '%s for rule %s %s: %s' % (template, l, r, e.message)
  else:
    RULES[(l, r)] = default_rule_cases(l, r)

UNARY = {}
for line in ignore_preface(open(UNARY_FILE)):
  if line.startswith('#') or not line.strip():
    continue
  fields = line.split()
  (child, parent) = fields[:2]
  if len(fields) >= 3:
    template = ' '.join(fields[2:])
    try:
      UNARY[(child, parent)] = compile(template)
    except SyntaxError, e:
      raise SyntaxError, '%s for rule %s %s: %s' % (template, child, parent, e.message)

ptb = open(PTBANK)
missing_log = open('missing.log', 'w')
MISSED = set()
USED = set()

PTB_IGNORE = re.compile(r"\(-NONE- [^)]+\)|-[0-9]|\(`` [^)]+\) | \('' [^)]+\)")
PTB_MINOR = "SBJ PRD TMP TTL LGS HLN CLR MNR DIR ADV TPC NOM PRP EXT LOC"
PTB_MINOR = '(?:-' + ' |-'.join(PTB_MINOR.split()) + ' )[0-9]*'
PTB_MINOR = re.compile(PTB_MINOR)
PTB_CLEANUP = re.compile(r"\([^ ()]+ +\)")
if MODE == '-d':
  print "# this file was generated by the following command(s):"
  print "# " + ' '.join(sys.argv)
  print

  total = 0
  errors = 0

  for line in open(CCGBANK):
    if line.startswith('ID='):
      continue

    tree = Tree(line)
    ccgtree = tree.ccgtree()
    MISSED.clear()
    USED.clear()
    converted = '(TOP %s)' % str(tree.ptbtree())

    parsed = True

    correct = PTB_IGNORE.sub('', ptb.readline().strip())
    correct = PTB_MINOR.sub(' ', correct)
    while PTB_CLEANUP.search(correct):
      correct = PTB_CLEANUP.sub('', correct)
    correct = ' '.join(correct.split())

    print total
    print 'CCG:', ccgtree
    print '-----------------------'

    if MISSED:
      print '\n'.join([colours.YELLOW + (' '.join(x)) + colours.OFF for x in sorted(MISSED)])
      print '-----------------------'

    if USED:
      print '\n'.join([colours.LBLUE + (' '.join(x)) + colours.OFF for x in sorted(USED)])
      print '-----------------------'

    if correct != converted:
      errors += 1
    total += 1

    correct, converted = colours.diff(correct, str(converted))

    print 'TEST:', converted
    print '-----------------------'
    print 'GOLD:', correct
    print '======================='

  print 'Number of sentences', total
  print 'Number of correct sentences', total - errors
elif MODE in ['-t', '-T']:
  parsed = True
  for line in open(CCGBANK):
    if line.startswith('ID='):
      if not parsed:
        print '(TOP)'
      parsed = False
      continue
    
    tree = Tree(line)
    MISSED.clear()
    print '(TOP %s)' % tree.ptbtree().evalb()
    parsed = True
