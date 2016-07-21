
EG = r"""(<T S[dcl] 0 2> (<T S[dcl] 1 2> (<T NP 0 2> (<T NP 0 2> (<T NP 0 2> (<T NP 0 1> (<T N 1 2> (<L N/N NNP NNP Pierre N_73/N_73>) (<L N NNP NNP Vinken N>) ) ) (<L , , , , ,>) ) (<T NP\NP 0 1> (<T S[adj]\NP 1 2> (<T NP 0 1> (<T N 1 2> (<L N/N CD CD 61 N_93/N_93>) (<L N NNS NNS years N>) ) ) (<L (S[adj]\NP)\NP JJ JJ old (S[adj]\NP_83)\NP_84>) ) ) ) (<L , , , , ,>) ) (<T S[dcl]\NP 0 2> (<L (S[dcl]\NP)/(S[b]\NP) MD MD will (S[dcl]\NP_10)/(S[b]_11\NP_10:B)_11>) (<T S[b]\NP 0 2> (<T S[b]\NP 0 2> (<T (S[b]\NP)/PP 0 2> (<L ((S[b]\NP)/PP)/NP VB VB join ((S[b]\NP_20)/PP_21)/NP_22>) (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_29/N_29>) (<L N NN NN board N>) ) ) (<T PP 0 2> (<L PP/NP IN IN as PP/NP_34>) (<T NP 1 2> (<L NP[nb]/N DT DT a NP[nb]_48/N_48>) (<T N 1 2> (<L N/N JJ JJ nonexecutive N_43/N_43>) (<L N NN NN director N>) ) ) ) ) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/N[num] NNP NNP Nov. ((S_61\NP_56)_61\(S_61\NP_56)_61)/N[num]_62>) (<L N[num] CD CD 29 N[num]>) ) ) ) ) (<L . . . . .>) ) 
"""

def _tuple(*args):
  return tuple(args)

def tuples(s):
  derivation, residue = _parse(s, _tuple, _tuple, _tuple)
  return derivation

def nodes(s):
  from tree import Node, Leaf
  derivation, residue = _parse(s, Leaf, Node, Node)
  return derivation

def _parse(s, leaf, unary, binary):
  if s.startswith('(<T '):
    _, cat, _, nchildren, rest = s.split(' ', 4)
    nchildren = nchildren[0] 

    left, rest = _parse(rest, leaf, unary, binary)
    if nchildren == '1':
      node = unary(cat, left)
    elif nchildren == '2':
      right, rest = _parse(rest, leaf, unary, binary)
      node = binary(cat, left, right)
    return node, rest[2:]
  elif s.startswith('(<L '):
    _, cat, pos_ptb, pos_ccg, token, indexation, rest = s.split(' ', 6)
    return (leaf(cat, token, pos_ccg, pos_ptb, indexation), rest)

def load(data, method=nodes):
  for line in data:
    if line.startswith('(<'):
      yield method(line.strip())

