#!/usr/bin/env python
# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

import sys
import nlp

from nlp.config import SPACE

try:
  cfg = nlp.config.Main('parser.py')
  base = nlp.config.OpPath(cfg, "base", "base directory for all models", '')
  pos_cfg = nlp.tagger.POSConfig(base)
  super_cfg = nlp.tagger.SuperConfig(base)
  parser_cfg = nlp.ccg.ParserConfig(base)
  int_cfg = nlp.ccg.IntegrationConfig()

  dir = nlp.config.Alias(cfg, SPACE, parser_cfg, "model", "parser")

  input = nlp.config.OpString(cfg, "input", "the input file to read from", "<stdin>")
  ifmt = nlp.config.OpFormat(cfg, "ifmt", "the input file format", nlp.io.Format("%w \n"));
  nopos = nlp.config.OpBool(cfg, "nopos", "use existing POS tags", False)

  output = nlp.config.OpString(cfg, SPACE, "output", "the output file to write to", "<stdout>")
  log = nlp.config.OpString(cfg, "log", "the log file to write to", "<stderr>")
  prefix = nlp.config.OpString(cfg, "prefix", "the prefix of the output and log files to write to", "")

  decoder_name = nlp.config.OpString(cfg, "decoder", "the parser decoder [deps, derivs, random]", "derivs")
  printer_name = nlp.config.OpString(cfg, "printer", "the parser output printer [prolog, deps, grs]", "grs")

  start_alias = nlp.config.Alias(cfg, SPACE, int_cfg.start, "start_level", "int-start_level")
  betas_alias = nlp.config.Alias(cfg, int_cfg.betas, "betas", "int-betas")
  dict_cutoff_alias = nlp.config.Alias(cfg, int_cfg.dict_cutoffs, "dict_cutoffs", "int-dict_cutoffs")

  cfg.reg(int_cfg, SPACE)
  cfg.reg(parser_cfg, SPACE)
  cfg.reg(super_cfg, SPACE)
  cfg.reg(pos_cfg, SPACE)

  cfg.parse(sys.argv)
  cfg.check()

  if prefix.value:
    output.value = prefix.value + ".out"
    log.value = prefix.value + ".log"

  if printer_name.value == "grs" and not parser_cfg.alt_markedup.has_changed():
    parser_cfg.alt_markedup.value = True

  sent = nlp.Sentence()

  reader = nlp.io.ReaderFactory(input.value, ifmt.value)

  if not nopos.value:
    pos = nlp.tagger.POS(pos_cfg)

  integration = nlp.ccg.Integration(int_cfg, super_cfg, parser_cfg, sent)

#  FORMAT = 1 + 4 + 32 + 256
#  printer = nlp.ccg.PrinterFactory(printer_name.value, output.value, log.value, integration.cats, FORMAT)
#  printer.preface("# this is the temporary preface")

  printer = nlp.ccg.PythonPrinter(integration.cats)

  decoder = nlp.ccg.DecoderFactory(decoder_name.value)

  i = 0
  while reader.next(sent, False, False):
    if len(sent.words) == 0:
      print >> sys.stderr, "end of input"
      break

    if not nopos.value:
      pos.tag(sent, nlp.tagger.VITERBI, 5)

    if integration.parse(sent, decoder, printer, True):
      for dep in printer.deps:
        print '(' + ' '.join(map(str, dep)) + ')'

      print '<c>', ' '.join(['|'.join(w) for w in zip(sent.words, sent.pos)])

    print

except nlp.IOError, e:
  print >> sys.stderr, "parser:ioerror:" + ':'.join(e.args)
