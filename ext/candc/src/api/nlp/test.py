
import ccgbank, tree

for t in ccgbank.load(open('/u1/repos/candc/data/CCGbank1.2/data/AUTO/00/wsj_0001.auto')):
  pass

print list(tree.leaves(t))
