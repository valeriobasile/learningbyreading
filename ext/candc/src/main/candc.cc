// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

const char *PROGRAM_NAME = "candc";

#include "candc.h"

using namespace NLP;
using namespace std;

int
run(int argc, char **argv){
  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);
  CandC::Config candc_cfg;

  Config::Alias alias_candc(cfg, SPACE, candc_cfg, "models", "candc");

  Config::Op<std::string> input_file(cfg, SPACE, "input", "the input file to read from", STDIN);
  Config::Op<std::string> output_file(cfg, SPACE, "output", "the output file to write to", STDOUT);
  Config::Op<std::string> log_file(cfg, "log", "the log file to write to", STDERR);
  Config::Op<std::string> prefix(cfg, OPTIONAL, "prefix", "the prefix of the output and log files to write to");

  cfg.reg(candc_cfg, SPACE);

  cfg.parse(argc, argv);
  cfg.check();

  if(prefix() != ""){
    output_file.set_value(prefix() + ".out");
    log_file.set_value(prefix() + ".log");
  }

  CandC candc(candc_cfg, PREFACE.str());

  IO::Input in(input_file());
  IO::Output out(output_file());
  IO::Log log(log_file());

  candc.parse(in, out, log);

  return 0;
}

#include "main.h"
