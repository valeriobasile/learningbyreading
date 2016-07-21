
import colours
import cfg
from cat import Cat

class Deriv(object):
  __slots__ = ('root', 'id', 'filename', 'leaves')

  def __init__(self, root=None, id=None, filename=None):
    self.id, self.root, self.filename = id, root, filename
    self.leaves = None
    if self.root:
      self.leaves = tuple(leaves(self.root))

  def _repr(self):
    return 'ID=%s PARSER=GOLD NUMPARSE=1' % self.id

  def __repr__(self):
    return '%s\n%s' % (self._repr(), repr(self.root))

  def __str__(self):
    return '%s\n%s' % (self._repr(), self.root)

  def stags(self, begin=0, end=None):
    return ' '.join(['%s|%s|%s' % (l.t, l.pos, l.cat) for l in self.leaves[begin:end]])

  def context(self, begin=0, end=None, edge=1):
    left = type(edge) is int and begin - edge or None

    left = ' '.join(['%s|%s|%s' % (l.t, l.pos, l.cat) for l in self.leaves[left:begin]])
    if left == '':
      left = '__STAART__'
    left += ' '

    mid = ' '.join(['%s|%s|%s' % (l.t, l.pos, l.cat) for l in self.leaves[begin:end]])
    if cfg.SHOW_COLOUR:
      mid = cfg.COLOUR + mid + colours.OFF

    right = type(edge) is int and end + edge or None
    right = ' '.join(['%s|%s|%s' % (l.t, l.pos, l.cat) for l in self.leaves[end:right]])
    if right == '':
      right = '__EEND__'
    right = ' ' + right

    return  left + mid + right

  def pipe(self):
    return '###\n%s' % self.root.pipe()

class Pattern(Deriv):
  def __init__(self, pattern):
    if type(pattern) is list:
      Deriv.__init__(self, None)
      self.leaves = tuple(pattern)
    else:
      Deriv.__init__(self, pattern)

  def _repr(self):
    return 'Pattern'

  def _gettree(self, tree):
    if isinstance(tree, Deriv):
      return tree.root
    else:
      return tree

  def _getleaves(self, tree):
    if isinstance(tree, Deriv):
      return tree.leaves
    else:
      return tuple(leaves(tree))

  def match(self, tree):
    root = self._gettree(tree)
    return self.root.match(root) and root

  def _find(self, node):
    return self.root.match(node) and node or \
        (node.l and self._find(node.l)) or \
        (node.r and self._find(node.r))

  def find(self, tree):
    root = self._gettree(tree)
    return self._find(root)

  def _findtopdown(self, node, matches, stop_at_first):
    match = False
    if self.root.match(node):
      matches.append(node)
      match = True

    if match and stop_at_first:
      return True

    match_l, match_r = False, False
    if node.l:
      match_l = self._findtopdown(node.l, matches, stop_at_first)
    if node.r:
      match_r = self._findtopdown(node.r, matches, stop_at_first)

    return match or match_l or match_r

  def _findbottomup(self, node, matches, stop_at_first):
    match_l, match_r = False, False
    if node.l:
      match_l = self._findbottomup(node.l, matches, stop_at_first)
    if node.r:
      match_r = self._findbottomup(node.r, matches, stop_at_first)
    match = match_l or match_r

    if match and stop_at_first:
      return True

    if self.root.match(node):
      matches.append(node)
      match = True

    return match

  def findall(self, tree):
    root = self._gettree(tree)
    nodes = []
    self._findtopdown(root, nodes, False)
    return nodes

  def findparents(self, tree):
    root = self._gettree(tree)
    nodes = []
    self._findtopdown(root, nodes, True)
    return nodes

  def findchildren(self, tree):
    root = self._gettree(tree)
    nodes = []
    self._findbottomup(root, nodes, True)
    return nodes

  def findlca(self, tree):
    leaves = self._getleaves(tree)
    nleaves = len(leaves)

    seq = self.leaves
    nseq = len(seq)
    nodes = []
    for i in xrange(nleaves - nseq):
      leaf = leaves[i]
      j = i
      for s in seq:
        if not s.match(leaves[j]):
          break
        j += 1
      else:
        node = leaves[i]
        end = i + nseq
        while node.start + node.span < end:
          node = node.parent
        nodes.append(node)
    return nodes

  def findnospan(self, tree):
    nseq = len(self.leaves)
    nodes = self.findlca(tree)
    return [node for node in nodes if node.span != nseq]

  def findspan(self, tree):
    nseq = len(self.leaves)
    nodes = self.findlca(tree)
    return [node for node in nodes if node.span == nseq]

def combinator(result, left, right):
  if not left:
    return 'lf'
  if not right:
    return 'u'

  if left.fwd:
    if left.arg.unify(right) and left.res.unify(result):
      return 'fa'
    if right.fwd and left.arg.unify(right.res):
      return 'fc'
  if right.bwd:
    if right.arg.unify(left):
      return 'ba'
    if left.bwd and left.res.unify(right.arg):
      return 'bc'

class Node(object):
  __slots__ = ('cat', 'l', 'r', 'parent', 'colour', 'start', 'span', '_val', '_comb')

  def __init__(self, cat=None, left=None, right=None, start=None, span=None):
    self.cat, self.l, self.r = cat, left, right
    self.parent = None
    self.colour = None
    self._val, self._comb = None, None
    self.start, self.span = start, span

  def fval(self):
    if self._val is None:
      self._val = Cat.parse(self.cat)
    return self._val

  def sval(self, v):
    self._val = v
    self._comb = None
    self.cat = str(v)

  val = property(fval, sval)

  @property
  def comb(self):
    if self._comb is None:
      self._comb = combinator(self.val, self.l and self.l.val, self.r and self.r.val)
    return self._comb

  def _repr(self, colour, depth):
    c = self.colour or colour
    cat = c and '%s%s%s' % (c, self.cat, colours.OFF) or self.cat
    depth += 2
    indent = '\n' + depth*' '
    if self.r:
      return "(%s%s%s%s%s)" % (cat, indent, self.l._repr(colour, depth), indent, self.r._repr(colour, depth))
    elif self.l:
      return "(%s %s)" % (cat, self.l._repr(colour, depth + len(self.cat)))

  def __str__(self):
    raise NotImplemented

  def pipe(self):
    raise NotImplemented

  def stags(self):
    return ' '.join(['%s|%s|%s' % (l.t, l.pos, l.cat) for l in leaves(self)]) 

  def __repr__(self):
    return self._repr(cfg.SHOW_COLOUR and cfg.COLOUR, 0)

  def match(self, other):
    return self.__class__ is other.__class__ and \
           (self.cat is None or self.cat == other.cat)

class Leaf(Node):
  __slots__ = ('t', 'pos', 'cat', 'ptb', 'coindex')

  def __init__(self, cat=None, pos=None, ptb=None, t=None, coindex=None, start=None):
    Node.__init__(self, cat, start=start, span=(start is not None and 1 or None))
    self.t, self.pos, self.ptb, self.coindex = t, pos, ptb, coindex

  def _repr(self, colour, _):
    c = self.colour or colour
    cat = colour and '%s%s%s' % (c, self.cat, colours.OFF) or self.cat
    if cfg.SHOW_POS:
      return "(%s %s %s)" % (cat, self.t, self.pos)
    else:
      return "(%s %s)" % (cat, self.t)

  def __str__(self):
    return "(<L %s %s %s %s %s>)" % (self.cat, self.pos, self.ptb, self.t, self.coindex)

  def pipe(self):
    return "(<L *** %s %s %s>\n)\n" % (self.cat, self.pos, self.t)

  def match(self, other):
    return Node.match(self, other) and \
           (self.t is None or self.t == other.t) and \
           (self.pos is None or self.pos == other.pos)

class Rule(Node):
  __slots__ = ('comb', 'head')

  def __init__(self, cat=None, head=None, l=None, r=None):
    start, span = None, None
    if l:
      l.parent = self
      start = l.start
    if r:
      r.parent = self

    span = l and (l.span + (r and r.span or 0)) or None
    Node.__init__(self, cat, l, r, start, span)
    self.head = head

  def __str__(self):
    if self.r:
      return "(<T %s %d 2> %s %s )" % (self.cat, self.head, str(self.l), str(self.r))
    else:
      return "(<T %s %d 1> %s )" % (self.cat, self.head, str(self.l))

  def pipe(self):
    # (<T *** S[dcl] * 0 2>
    if self.r:
      return "(<T *** %s * %d 2>\n%s%s)\n" % (self.cat, self.head, self.l.pipe(), self.r.pipe())
    else:
      return "(<T *** %s * %d 1>\n%s)\n" % (self.cat, self.head, self.l.pipe())

  def match(self, other):
    return Node.match(self, other) and \
           (self.l is None or self.l.match(other.l)) and \
           (self.r is None or self.r.match(other.r))

def leaves(root):
  stack = [root]
  while stack:
    current = stack.pop()
    if isinstance(current, Leaf):
      yield current
    else:
      if current.r:
        stack.append(current.r)
      if current.l:
        stack.append(current.l)


