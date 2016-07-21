#!/usr/bin/env python

import sys
from itertools import islice
from collections import deque, defaultdict

NGRAM = int(sys.argv[1])
TEXT = sys.argv[2]

def ngrams(words, n):
  words = iter(words)
  history = deque(islice(words, n))
  if len(history) < n:
    return

  yield ' '.join(history)
  for word in words:
    history.popleft()
    history.append(word)
    yield ' '.join(history)

counts = defaultdict(int)
for line in open(TEXT, 'rU'):
  for ngram in ngrams(line.split(), NGRAM):
    counts[ngram] += 1

counts = [(v, k) for k, v in counts.iteritems()]
counts.sort(reverse=True)

for v, k in counts:
  print v, k
