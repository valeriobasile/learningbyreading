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

#include "pool.h"

#include "model/model.h"

#include "io/format.h"
#include "config/format.h"

#include "io/reader.h"
#include "io/writer.h"

#include "io/reader_factory.h"

#include "tagger/tagdict.h"
#include "tagger/tagsetdict.h"
#include "tagger/tagger.h"
#include "tagger/super.h"

#include "parser/parser.h"
#include "parser/decoder_factory.h"

#include "parser/printer.h"
#include "parser/print_stream.h"
#include "parser/print_factory.h"

#include "parser/integration.h"

#include "timer.h"

const char *PROGRAM_NAME = "parser";

using namespace std;
using namespace NLP;
using namespace NLP::IO;
using namespace NLP::Taggers;
using namespace NLP::CCG;

int
run(int argc, char** argv){
  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);
  Super::Config super_cfg;
  Parser::Config parser_cfg;
  Integration::Config int_cfg("int");

  Config::Alias parser_alias(cfg, SPACE, parser_cfg, "model", "parser");
  Config::Alias super_alias(cfg, super_cfg, "super", "super");

  Config::Op<std::string> input_file(cfg, SPACE, "input", "the input file to read from", STDIN);
  Config::Op<IO::Format> ifmt(cfg, "ifmt", "the input file format", Format("%w|%p \n"));

  Config::Op<std::string> output_file(cfg, SPACE, "output", "the output file to write to", STDOUT);
  Config::Op<std::string> log_file(cfg, "log", "the log file to write to", STDERR);
  Config::Op<std::string> prefix(cfg, "prefix", "the prefix of the output and log files to write to", "");

  Config::Restricted<std::string> decoder_name(cfg, SPACE, "decoder", "the parser decoder [deps, derivs, random]",
					       &DecoderFactory::check, "derivs");
  Config::Restricted<std::string> printer_name(cfg, "printer", "the parser output printer [deps, prolog, boxer, ccgbank, grs, xml, debug, js]", &PrinterFactory::check, "grs");
  Config::Op<bool> force_words(cfg, "force_words", "force the parser to print words on fails", true);

  Config::Op<bool> oracle(cfg, SPACE, "oracle", "use gold standard supertags from input", false);
  Config::Op<bool> ext_super(cfg, "ext_super", "use an external supertags from input", false);

  Config::Alias start_alias(cfg, SPACE, int_cfg.start, "start_level", "int-start_level");
  Config::Alias betas_alias(cfg, int_cfg.betas, "betas", "int-betas");
  Config::Alias dict_cutoff_alias(cfg, int_cfg.dict_cutoffs, "dict_cutoffs", "int-dict_cutoffs");

  cfg.reg(int_cfg, SPACE);
  cfg.reg(parser_cfg, SPACE);
  cfg.reg(super_cfg, SPACE);

  cfg.parse(argc, argv);
  cfg.check();

  if(prefix() != ""){
    output_file.set_value(prefix() + ".out");
    log_file.set_value(prefix() + ".log");
  }

  if(printer_name() == "grs" && !parser_cfg.alt_markedup.has_changed())
    parser_cfg.alt_markedup.set_value(true);

  Sentence sent;

  ReaderFactory reader(input_file(), ifmt());

  Integration integration(int_cfg, super_cfg, parser_cfg, sent);

  StreamPrinter::Format FMT = StreamPrinter::FMT_DEV |
    StreamPrinter::FMT_PRINT_UNARY;

  if(force_words())
    FMT |= StreamPrinter::FMT_FORCE_WORDS;

  IO::Output out(output_file());
  IO::Log log(log_file());
  PrinterFactory printer(printer_name(), out, log, integration.cats, FMT);
  printer.header(PREFACE.str());

  DecoderFactory decoder(decoder_name());

  Stopwatch watch;

  bool USE_SUPER = not (oracle() or ext_super());
  while(reader.next(sent)){
    if(oracle())
      sent.copy_multi('s', 'S');

    if(sent.words.size() == 0){
      log.stream << "end of input" << endl;
      break;
    }

    integration.parse(sent, decoder, printer, USE_SUPER);
  }
  printer.footer();

  double seconds = watch.stop();
  double sentence_speed = integration.nsentences / seconds;
  double word_speed = integration.nwords / seconds;

  log.stream << endl;

  log.stream << "use super = " << USE_SUPER << endl;
  log.stream << "beta levels = " << integration.BETAS << endl;
  log.stream << "dict cutoffs = " << integration.DICT_CUTOFFS << endl;
  log.stream << "start level = " << integration.START << endl;

  log.stream << "nwords = " << integration.nwords << endl;
  log.stream << "nsentences = " << integration.nsentences << endl;

  log.stream << "nexceptions = " << integration.nexceptions << endl;

  log.stream << "ncombines = " << integration.global_stats.ncombines << endl;
  log.stream << "ncombines_zeros = " << integration.global_stats.ncombines_zeros << endl;
  log.stream << "ncombines_reduced = " << integration.global_stats.ncombines_reduced << endl;
  log.stream << "ncombines_rejected = " << integration.global_stats.ncombines_rejected << endl;

  ulong nfail_bound = integration.nfail_nospan + integration.nfail_explode;
  ulong nfail_back = integration.nfail_nospan_explode + integration.nfail_explode_nospan;
  ulong nfail = nfail_bound + nfail_back;

  log.stream << "nfailures = " << nfail << endl;
  log.stream << "  run out of levels = " << nfail_bound << endl;
  log.stream << "    nospan = " << integration.nfail_nospan << endl;
  log.stream << "    explode = " << integration.nfail_explode << endl;
  log.stream << "  backtrack on levels = " << nfail_back << endl;
  log.stream << "    nospan/explode = " << integration.nfail_nospan_explode << endl;
  log.stream << "    explode/nospan = " << integration.nfail_explode_nospan << endl;

  for(int i = 0; i < static_cast<int>(integration.BETAS.size()); ++i){
    log.stream << "nsuccess " << i << ' ' << setw(6) << integration.BETAS[i];
    log.stream << ' ' << setw(6) << integration.nsuccesses[i];
    if(i == integration.START)
      log.stream << " <--";
    log.stream << endl;
  }

  log.stream << "total parsing time = " << seconds << " seconds\n";
  log.stream << "sentence speed = " << sentence_speed << " sentences/second\n";
  log.stream << "word speed = " << word_speed << " words/second\n";
 
  return 0;
}

#include "main.h"
