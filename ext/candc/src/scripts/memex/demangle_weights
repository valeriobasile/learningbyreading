#!/usr/bin/env python

from sys import argv, exit
from string import join

if len(argv) != 2:
  print >> stderr, "usage: %s <project>" % argv[0]
  exit(1)

model = argv[1]
if model[-1] == '/':
  model = model[:-1]

def load(filename, lookup):
  line_no = 0
  for line in open(filename):
    if line_no < 3:
      line_no += 1
      continue
    line = line[:-1]
    fields = line.split()
    freq = int(fields.pop())
    lookup.append(join(fields))

klasses = []
load(model + "/classes", klasses)

attributes = []
load(model + "/attributes", attributes)
print "%d Attributes" % len(attributes)
print "%d klasses" % len(klasses)

line_no = 0
for line in open(model + "/weights"):
  line = line[:-1]
  if line_no < 3:
    line_no += 1
    continue
  (klass, attrib, weight) = line.split()
  if weight[0] != '-':
    weight = ' ' + weight
  print "%s %s" % (klass, attrib),
  # check this -2...
  print "%s\t%s\t%s" % (klasses[int(klass) - 2], attributes[int(attrib)], weight)
