
import colours

def explicit_punct_conj(node):
  if node.cat.endswith('[conj]') and \
      node.l and node.l.cat in [',', ';'] and \
      node.r and not node.r.cat.endswith('[conj]'):
    node.l.cat = 'conj'
    node.l.coindex = 'conj'
    node.l.colour = colours.GREEN

def explicit_absorption(node):
  if node.l and node.l.cat in ['.', ',', ':', ';', 'LRB', 'RRB'] and \
        node.r and node.r.cat == node.cat:
    pass

def identity(node):
  pass
