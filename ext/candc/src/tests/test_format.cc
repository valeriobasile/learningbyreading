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

const char *PROGRAM_NAME = "test_format";

using namespace std;
using namespace NLP;

int
run(int argc, char **argv){
  Config::Main cfg(PROGRAM_NAME);
  Config::Op<std::string> fmt_string(cfg, "fmt", "formatting");

  cfg.parse(argc, argv);
  cfg.check();

  IO::Format format(fmt_string());

  cout << "sentence pre = '" << format.sent_pre << "'\n";
  cout << "sentence post = '" << format.sent_post << "'\n";
  cout << "field separator = '" << format.field_sep << "'\n";
  cout << "word separator = '" << format.word_sep << "'\n";
  cout << "sent separator = '" << format.sent_sep << "'\n";
  cout << "fields = '" << format.fields << "'\n";
  cout << "separators = '" << format.separators << "'\n";
  cout << "has sentence pre = " << format.has_sent_pre << "\n";
  cout << "has sentence post = " << format.has_sent_post << "\n";

  return 0;
}

#include "main.h"
