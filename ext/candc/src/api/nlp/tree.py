
import colours

class Node:
  POS = True
  COLOUR = colours.YELLOW
  def __init__(self, cat, left = None, right = None):
    self.cat, self.l, self.r = cat, left, right

  def _repr(self, colour, depth):
    cat = colour and '%s%s%s' % (colour, self.cat, colours.OFF) or self.cat
    depth += 2
    indent = '\n' + depth*' '
    if self.r:
      return "(%s%s%s%s%s)" % (cat, indent, self.l._repr(colour, depth), indent, self.r._repr(colour, depth))
    elif self.l:
      return "(%s %s)" % (cat, self.l._repr(colour, depth + len(self.cat)))

  def __repr__(self):
    return self._repr(self.COLOUR, 0)

class Leaf(Node):
  def __init__(self, cat, *args):
    Node.__init__(self, cat)
    self.token, self.pos, self.ptb, self.index = args

  def _repr(self, colour, _):
    cat = colour and '%s%s%s' % (colour, self.cat, colours.OFF) or self.cat
    if self.POS:
      return "(%s %s %s)" % (cat, self.token, self.pos)
    else:
      return "(%s %s)" % (cat, self.token)

def leaves(root):
  stack = [root]
  while stack:
    current = stack.pop()
    if isinstance(current, Leaf):
      yield current
    else:
      if current.r:
        stack.append(current.r)
      stack.append(current.l)

def preorder(root):
  stack = [root]
  while stack:
    current = stack.pop()
    if isinstance(current, Leaf):
      yield current
    else:
      if current.r:
        stack.append(current.r)
      stack.append(current.l)
