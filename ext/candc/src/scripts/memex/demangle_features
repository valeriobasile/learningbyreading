#!/usr/bin/python2.2

from sys import stderr, argv, exit
from string import join

if len(argv) != 2:
  print >> stderr, "usage: %s <project>" % argv[0]
  exit(1)

model = argv[1]
if model[-1] == '/':
  model = model[:-1]

def load(filename, lookup):
  for line in open(filename):
    line = line[:-1]
    fields = line.split()
    freq = int(fields.pop())
    lookup.append(join(fields))

klasses = []
load(model + "/classes", klasses)

attributes = []
load(model + "/attributes", attributes)

for line in open(model + "/features"):
  line = line[:-1]
  (klass, attrib, freq) = line.split()
  print "%s\t%s\t%s" % (klasses[int(klass)], attributes[int(attrib)], freq)
