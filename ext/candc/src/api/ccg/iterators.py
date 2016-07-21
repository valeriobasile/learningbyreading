
def pre_order(visitor, node):
  visitor(node)
  if node.l:
    pre_order(visitor, node.l)
  if node.r:
    pre_order(visitor, node.r)

def in_order(visitor, node):
  if node.l:
    in_order(visitor, node.l)
  visitor(node)
  if node.r:
    in_order(visitor, node.r)

def post_order(visitor, node):
  if node.l:
    post_order(visitor, node.l)
  if node.r:
    post_order(visitor, node.r)
  visitor(node)

def visit(visitor, iter, order=pre_order):
  for deriv in iter:
    order(visitor, deriv.root)
    yield deriv

def match(pattern, iter):
  for deriv in iter:
    res = pattern(deriv)
    if res:
      yield deriv, res
