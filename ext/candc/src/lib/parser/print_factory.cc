// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_printer.h"
#include "parser/print_factory.h"
#include "parser/print_deps.h"
#include "parser/print_grs.h"
#include "parser/print_prolog.h"
#include "parser/print_boxer.h"
#include "parser/print_ccgbank.h"
#include "parser/print_xml.h"
#include "parser/print_debug.h"
#include "parser/print_js.h"
#include "parser/print_latex.h"

static const char *PRINTERS[] = {"deps", "prolog", "boxer", "ccgbank", "grs", "xml", "debug", "js", "latex"};

using namespace std;

namespace NLP { namespace CCG {

void
PrinterFactory::die_unknown_printer(const string &name) {
  stringstream msg;
  msg << "unrecognised printer name '" << name << "' [";
  for (size_t i = 0; i != sizeof(PRINTERS)/sizeof(const char *); ++i) {
    if (i != 0)
      msg << ", ";
    msg << PRINTERS[i];
  }
  msg << "]";
  throw NLP::Exception(msg.str());
}

void
PrinterFactory::check(const string &name){
  for (size_t i = 0; i != sizeof(PRINTERS)/sizeof(const char *); ++i)
    if (name == PRINTERS[i])
      return;
  die_unknown_printer(name);
}

StreamPrinter *
PrinterFactory::create_printer(const string &name) const {
  if(name == "deps")
    return new DepsPrinter(cats, FORMAT, out, log);
  else if(name == "prolog")
    return new PrologPrinter(cats, FORMAT, out, log);
  else if(name == "boxer")
    return new BoxerPrinter(cats, FORMAT, out, log);
  else if(name == "ccgbank")
    return new CCGbankPrinter(cats, FORMAT, out, log);
  else if(name == "grs")
    return new GRsPrinter(cats, FORMAT, out, log);
  else if(name == "xml")
    return new XMLPrinter(cats, FORMAT, out, log);
  else if(name == "debug")
    return new DebugPrinter(cats, FORMAT, out, log);
  else if(name == "js")
    return new JSPrinter(cats, FORMAT, out, log);
  else if(name == "latex")
    return new LatexPrinter(cats, FORMAT, out, log);
  else {
    die_unknown_printer(name);
    return 0;
  }
}

PrinterFactory::PrinterFactory(const std::string &name, IO::Output &out, IO::Log &log, Categories &cats, const StreamPrinter::Format FORMAT) : StreamPrinter(cats, FORMAT, out, log), printer(create_printer(name)){}

} }
