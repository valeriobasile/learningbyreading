#!/usr/bin/python
from lxml import etree
from optparse import OptionParser
#import libxml2
#import libxslt

"""Transform an XML document using a stylesheet, optionally with parameters. The
XML document is given on STDIN, the path to the stylesheet as the first command
line argument, and the rest of the command line arguments are parameter names
and values, alternatingly."""

# command line argument partsing
parser = OptionParser()
parser.add_option('-i',
                  '--input',
                  dest="input_file",
                  help="read text from FILE",
                  metavar="FILE")
parser.add_option('-o',
                  '--output',
                  dest="output_file",
                  help="write JSON to FILE",
                  metavar="FILE")
parser.add_option('-x',
                  '--xsl',
                  dest="xsl_file",
                  help="XSL file to apply",
                  metavar="XSL")

(options, args) = parser.parse_args()

def kvlist_to_dict(list):
    result = {}
    while len(list) >= 2:
        k = list.pop(0)
        v = list.pop(0)
        result[k] = v
    return result

def parameters():
	return kvlist_to_dict(sys.argv[2:])

def apply(xml, xsl):
    dom = etree.fromstring(xml)
    xslt = etree.fromstring(xsl)
    transform = etree.XSLT(xslt)
    newdom = transform(dom)
    return etree.tostring(newdom)

if __name__=="__main__":
    with open(options.input_file) as f_in, open(options.xsl_file) as f_xsl:
        str_result = apply(f_in.read(), f_xsl.read())
    with open(options.output_file, 'w') as f:
        f.write(str_result)
