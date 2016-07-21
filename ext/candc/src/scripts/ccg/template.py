#!/usr/bin/env python

import re

class Templates:
  TOKENS = re.compile('([A-Za-z]+|[^ ])')
  SIMPLE = {
    'l': '_n.l.ptb()',
    'r': '_n.r.ptb()',
    '<': 'addr(_n)',
    '>': 'addl(_n)',
  }

  def compile(self, template):
    python = self.parse(self.TOKENS.findall(template))
    return eval("lambda _n: %s" % python)

  def parse(self, tokens):
    t = tokens.pop(0)
    if t in '([':
      if t == '(':
        label = "'%s'" % tokens.pop(0)
        args = self.parse_args(tokens, ')')
      elif s[0] == '[':
        label = 'None'
        args = self.parse_args(tokens, ']')
      return 'PTB(_n, %s, %s)' % (label, ', '.join(args))
    elif t in self.SIMPLE:
      return self.SIMPLE[t]
    else:
      raise SyntaxError, "unknown token '%s'" % t

  def parse_args(self, tokens, delimiter):
    args = []
    while tokens:
      if tokens[0] == delimiter:
        tokens.pop(0)
        return args
      args.append(self.parse(tokens))
    raise SyntaxError, "missing closing '%s'" % delimiter

templates = Templates()

t = templates.compile("<")
