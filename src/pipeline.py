#!/usr/bin/env python
from optparse import OptionParser
from babelfy import babelfy
from boxer import boxer

# command line argument partsing
parser = OptionParser()
parser.add_option('-i',
                  '--input',
                  dest="input_file",
                  help="read text from FILE",
                  metavar="FILE")
'''
parser.add_option('-o',
                  '--output',
                  dest="output_file",
                  help="write JSON to FILE",
                  metavar="FILE")
'''
(options, args) = parser.parse_args()

# read input file
with open(options.input_file) as f:
    text = f.read()

babel = babelfy(text)
print babel
drs = boxer(text)
print drs

# write output
#with open(options.output_file, 'w') as f:
#    f.write(r.text.encode('utf-8'))
