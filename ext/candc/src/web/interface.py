#!/usr/bin/env python
# vim: set st=2 sts=2 sw=2 et ai:
# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

import ast
import cgi
from operator import attrgetter
import StringIO
import sys
import traceback

import nlp
from nlp.config import SPACE

# Setup and parse config
cfg = nlp.config.Main('parser.py')
base = nlp.config.OpPath(cfg, "base", "base directory for all models", '')
pos_cfg = nlp.tagger.POSConfig(base)
super_cfg = nlp.tagger.SuperConfig(base)
parser_cfg = nlp.ccg.ParserConfig(base)
int_cfg = nlp.ccg.IntegrationConfig()

dir = nlp.config.Alias(cfg, SPACE, parser_cfg, "model", "parser")

decoder_name = nlp.config.OpString(cfg, "decoder", "the parser decoder [deps, derivs, random]", "derivs")

start_alias = nlp.config.Alias(cfg, SPACE, int_cfg.start, "start_level", "int-start_level")
betas_alias = nlp.config.Alias(cfg, int_cfg.betas, "betas", "int-betas")
dict_cutoff_alias = nlp.config.Alias(cfg, int_cfg.dict_cutoffs, "dict_cutoffs", "int-dict_cutoffs")

cfg.reg(int_cfg, SPACE)
cfg.reg(parser_cfg, SPACE)
cfg.reg(super_cfg, SPACE)
cfg.reg(pos_cfg, SPACE)

cfg.parse(sys.argv)
cfg.check()


# Setup C&C
sent = nlp.Sentence()

integration = nlp.ccg.Integration(int_cfg, super_cfg, parser_cfg, sent)
pos_tagger = nlp.tagger.POS(pos_cfg)

printer = nlp.ccg.PythonPrinter(integration.cats)
decoder = nlp.ccg.DecoderFactory(decoder_name.value)

FORMAT = 1 + 4 + 32 + 256

input_filename = "interface.in"
output = nlp.io.Output("interface.out");
log = nlp.io.Log("interface.log");
#printer = nlp.ccg.PrinterFactory("deps", output, log, integration.cats, FORMAT)

words = []
chart = integration.parser.chart
relations = integration.cats.relations

shell_context = dict(sent=sent, integration=integration, chart=chart)

class Stats:
  def __init__(self):
    self.nwords = 0
    self.nused = 0
    self.nunused = 0

stats = Stats()

import tornado.web
import tornado.httpserver
import tornado.ioloop
import tornado.options
import tornado.escape

import logging
logging.getLogger().setLevel(logging.INFO)
tornado.options.enable_pretty_logging()

extra_cached_cats = []
cats_cache = [None]
cat2id = {}
def cache_cat(cat):
  if cat is None:
    return 0

  s = cat.str()
  if s not in cat2id:
    id = cat2id[s] = len(cats_cache)
    cat = eval(cat.str_js())
    cats_cache.append(cat)
    extra_cached_cats.append(cat)
  else:
    id = cat2id[s]
  return id

sc2id = {}
def cache_sc(sc):
  if sc is None:
    return 0
  return sc2id.setdefault(sc.id, len(sc2id) + 1)

def var2id(i):
  return '+XYZWVUTRQABCDEF'[i]

def get_unfilled(sc):
  res = []
  for d in sc.unfilled:
    rel = relations[d.rel]
    res.append(dict(pos=d.head, word=words[d.head - 1],
                    cat=cache_cat(rel.cat), slot=rel.slot,
                    varid=var2id(int(d.var))))
  return res

def get_vars(sc):
  vars = []
  for j, var in enumerate(sc.vars):
    if j >= sc.nactive:
      break
    vars.append(dict(varid=var2id(j), fillers=list(var.fillers)))
  return vars

def get_alts(sc, i):
  alts = []
  for alt in sorted(sc.equiv, key=attrgetter('score')):
    alts.append(dict(cat=cache_cat(alt.cat), id=cache_sc(alt),
                     l=cache_sc(alt.l), r=cache_sc(alt.r),
                     m=sc.marker, i=i, c=alt.comb, s=alt.score))
  return alts

def dict_equiv(sc, i):
  max = sc.max or sc
  return dict(cat=cache_cat(sc.cat), id=cache_sc(sc),
              l=cache_sc(max.l), r=cache_sc(max.r),
              pos=max.pos, span=max.span,
              m=sc.marker, i=i, c=max.comb, s=max.score)

def dict_detail(sc, i):
  return dict(id=cache_sc(sc), vars=get_vars(sc), unfilled=get_unfilled(sc),
              ehash=sc.ehash, alts=get_alts(sc, i))

def dict_cell(cell, stats):
  nequivs = len(cell)
  nunused = 0
  nused = 0

  equivs = []
  for i in xrange(nequivs):
    sc = cell[i]
    if sc.marker == 0:
      nunused += 1
      continue
    nused += 1
    equivs.append(dict_equiv(sc, i))

  stats.nused += nused
  stats.nunused += nunused
  return dict(eqs=equivs, neqs=nequivs, nused=nused)

def dict_unused(cell):
  unused = []
  for i in xrange(len(cell)):
    sc = cell[i]
    if sc.marker != 0:
      continue
    unused.append(dict_equiv(sc, i))
  return unused

def dict_tree(sc):
  max = sc.max or sc
  left = right = None
  if sc.l:
    left = dict_tree(sc.l)
    if sc.r:
      right = dict_tree(sc.r)

  res = dict_equiv(sc, 0)
  res['l'] = left
  res['r'] = right

  return res

class IndexHandler(tornado.web.RequestHandler):
  def get(self):
    self.write(open('index.html').read().replace('{{ value }}', 'This is a test sentence .'))


WSJ = list(open('wsj00.raw', 'rU'))[3:]
class WSJHandler(tornado.web.RequestHandler):
  def get(self, num=None):
    if num is None:
      self.write("""<html><head><title>WSJ 00</title></head><body><ul>\n""")
      for i, sent in enumerate(WSJ):
        url = tornado.escape.url_escape(sent)
        self.write("""<li><a href="/wsj/%d/">%s</a></li>\n""" % (i, sent))
      self.write("</ul></body></html>")
    else:
      num = int(num)
      sent = WSJ[num]
      self.write(open('index.html').read().replace('{{ value }}', sent))

class ChartHandler(tornado.web.RequestHandler):
  def get(self):
    return self.post()

  def post(self):
    global words
    ifmt = str(self.get_argument('ifmt'))
    oracle = self.get_argument('oracle', False)
    has_super = r'%s' in ifmt
    has_pos = r'%p' in ifmt
    with open(input_filename, 'w') as f:
      f.write(str(self.get_argument('sentence')))
      f.write('\n')
    reader = nlp.io.ReaderFactory(input_filename, nlp.io.Format(ifmt))
    reader.next(sent, False, False)
    if not has_pos:
      pos_tagger.tag(sent, nlp.tagger.VITERBI, 5)
    if oracle and has_super:
      sent.copy_multi('s', 'S')
    integration.parse(sent, decoder, printer, not (oracle and has_super))
    words = list(sent.words)

    stats.nwords = nwords = len(chart)
    stats.nused = 0
    stats.nunused = 0
    cells = []
    for span in xrange(1, nwords + 1):
      for pos in xrange(0, nwords - span + 1):
        cell = dict_cell(chart(pos, span), stats)
        cell['pos'], cell['span'] = pos, span
        cell['tokens'] = map(str, sent.words[pos:pos+span])
        cells.append(cell)
    result = dict(chart=dict(stats=stats.__dict__, cells=cells, words=words, cats=cats_cache))
    self.set_header('Content-Type', 'application/json; charset=UTF-8')
    self.write(tornado.escape.json_encode(result))


class DetailHandler(tornado.web.RequestHandler):
  def post(self, pos, span, index):
    global extra_cached_cats

    extra_cached_cats = []
    pos = int(pos)
    span = int(span)
    index = int(index)
    result = dict(detail=dict_detail(chart(pos, span)[index], index), extra_cats=extra_cached_cats)
    self.set_header('Content-Type', 'application/json; charset=UTF-8')
    self.write(tornado.escape.json_encode(result))


class UnusedHandler(tornado.web.RequestHandler):
  def post(self, pos, span):
    global extra_cached_cats

    extra_cached_cats = []
    pos = int(pos)
    span = int(span)
    result = dict(unused=dict_unused(chart(pos, span)), extra_cats=extra_cached_cats)
    self.set_header('Content-Type', 'application/json; charset=UTF-8')
    self.write(tornado.escape.json_encode(result))


class EquivHandler(tornado.web.RequestHandler):
  def get(self, *args):
    self.post(*args)

  def post(self, pos, span, index1, index2):
    global extra_cached_cats

    extra_cached_cats = []
    pos = int(pos)
    span = int(span)
    index1 = int(index1)
    index2 = int(index2)
    cell = chart(pos, span)
    sc1 = cell[index1]
    sc2 = cell[index2]
    result = dict(equiv=nlp.ccg.explain(sc1, sc2))
    self.set_header('Content-Type', 'application/json; charset=UTF-8')
    self.write(tornado.escape.json_encode(result))

class ParseHandler(tornado.web.RequestHandler):
  def get(self):
    return self.post()

  def post(self):
    global words
    words = str(self.get_argument('sentence'))
    words = words.split()

    sent.clear()
    sent.words = words

    pos_tagger.tag(sent, nlp.tagger.VITERBI, 5)
    integration.parse(sent, decoder, printer, True)

    print printer.deriv

    stats.nwords = nwords = len(chart)
    stats.nused = 0
    stats.nunused = 0

    root_cell = chart(0, nwords)
    max_score = None
    max_root = None
    for i in xrange(len(root_cell)):
      root = root_cell[i]
      if max_score < root.max.score:
        max_score = root.max.score
        max_root = root.max

    tree = dict_tree(max_root)

    cells = []
    for span in xrange(1, nwords + 1):
      for pos in xrange(0, nwords - span + 1):
        cell = dict_cell(chart(pos, span), stats)
        cell['pos'], cell['span'] = pos, span
        cells.append(cell)
    result = dict(chart=dict(stats=stats.__dict__, cells=cells, words=words, cats=cats_cache, tree=tree))
    self.set_header('Content-Type', 'application/json; charset=UTF-8')
    self.write(tornado.escape.json_encode(result))

class ShellWindowHandler(tornado.web.RequestHandler):
  def get(self):
    self.write(open('shell.html').read())

class ShellPostHandler(tornado.web.RequestHandler):
  def post(self):
    cmd = self.get_argument('cmd', None)
    success = True
    stdin, stdout, stderr = sys.stdin, sys.stdout, sys.stderr
    out = StringIO.StringIO()
    try:
      sys.stdin = open('/dev/null', 'r')
      sys.stdout = out
      sys.stderr = out
      node = None
      try:
        node = ast.parse(cmd, mode='exec')
        if node.body:
          if isinstance(node.body[0], ast.Expr):
            ret = eval(cmd, {}, shell_context)
            print repr(ret)
          else:
            exec(cmd, {}, shell_context)
      except:
        success = False
        traceback.print_exc(file=out)
      finally:
        del node
    finally:
      sys.stdin = stdin
      sys.stdout = stdout
      sys.stderr = stderr
    
    response = dict(cmd=cmd, data=cgi.escape(out.getvalue()), success=success)
    self.set_header('Content-Type', 'application/json; charset=UTF-8')
    self.write(tornado.escape.json_encode(response))

handlers = [
    (r"/", IndexHandler),
    (r"/wsj/", WSJHandler),
    (r"/wsj/([0-9]+)/", WSJHandler),
    (r"/chart/", ChartHandler),
    (r"/detail/([0-9]+)/([0-9]+)/([0-9]+)/", DetailHandler),
    (r"/equiv/([0-9]+)/([0-9]+)/([0-9]+)/([0-9]+)/", EquivHandler),
    (r"/unused/([0-9]+)/([0-9]+)/", UnusedHandler),
    (r"/parse/", ParseHandler),
    (r"/shell/window/", ShellWindowHandler),
    (r"/shell/post/", ShellPostHandler),
]

application = tornado.web.Application(handlers, static_path='static', default_host='127.0.0.1')

http_server = tornado.httpserver.HTTPServer(application)
http_server.listen(8888)
tornado.ioloop.IOLoop.instance().start()
