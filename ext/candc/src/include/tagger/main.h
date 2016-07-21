/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.


#include "base.h"

#include "config/config.h"
#include "io/format.h"
#include "config/format.h"

#include "io/reader.h"
#include "io/reader_factory.h"
#include "io/writer.h"
#include "io/writer_factory.h"

#include "model/model.h"

#include "tagger/tagdict.h"
#include "tagger/tagger.h"
#include "timer.h"

using namespace std;
using namespace NLP;

template <class TAGGER>
int
run_tag(const bool MULTI, ulong DICT_CUTOFF, double BETA,
	const char *IFMT, const char *OFMT,
	int argc, char **argv){
  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);
  typename TAGGER::Config tagger_cfg(0, TAGGER::Config::DECODE);

  Config::Alias dir(cfg, SPACE, tagger_cfg, "model", tagger_cfg.NAME);

  Config::Op<std::string> input(cfg, SPACE, "input", "the input file to read from", IO::STDIN);
  Config::Op<IO::Format> ifmt(cfg, "ifmt", "the input file format", Format(IFMT));

  Config::Op<std::string> output(cfg, SPACE, "output", "the output file to write to", IO::STDOUT);
  Config::Op<IO::Format> ofmt(cfg, "ofmt", "the output file format", Format(OFMT));

  Config::Op<bool> output_preface(cfg, SPACE, "opref", "start the output with a preface", false);
  Config::Op<bool> output_verbose_preface(cfg, "overbose", "start the output with a verbose preface", false);
  Config::Op<std::string> output_config(cfg, OPTIONAL, "oconfig", "save the current configuration into a file");

  Config::Restricted<std::string> algorithm(cfg, SPACE, "algorithm",
					    MULTI ? "the decoding algorithm to use [noseq, greedy, fwdbwd]"
					    : "the decoding algorithm to use [viterbi]",
					    &Taggers::check_alg,
					    MULTI ? "fwdbwd" : "viterbi");

  Config::Op<ulong> dict_cutoff(cfg, SPACE, "dict_cutoff", "the frequency at which the tag dictionary is used", DICT_CUTOFF);
  Config::Op<double> *beta = 0;
  if(MULTI)
    beta = new Config::Op<double>(cfg, "beta", "beta variable beam cutoff ratio", BETA);

  Config::Alias maxwords(cfg, SPACE, tagger_cfg.maxwords, "maxwords", tagger_cfg.NAME + "-maxwords");

  cfg.reg(tagger_cfg, SPACE);

  cfg.parse(argc, argv);
  cfg.check();

  if(output_config.is_defined()){
    std::ofstream out(output_config().c_str());
    if(!out)
      NLP::IOException("could not open configuration file for writing", output_config());
    cfg.write_config(out);
  }

  ReaderFactory reader(input(), ifmt());
  WriterFactory writer(output(), ofmt());

  Timer timer("total  ");

  TAGGER tagger(tagger_cfg);

  Timer tagging("tagging");

  if(output_preface() || output_verbose_preface()){
    PREFACE << reader.PREFACE;
    if(output_verbose_preface())
      cfg.write_preface(PREFACE);
    writer.write_preface(PREFACE.str());
  }

  if(MULTI)
    tagger.mtag(reader, writer, Taggers::str2alg(algorithm()), dict_cutoff(), (*beta)());
  else
    tagger.tag(reader, writer, Taggers::str2alg(algorithm()), dict_cutoff());

  return 0;
}

