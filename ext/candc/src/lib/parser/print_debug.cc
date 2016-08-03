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
#include "parser/print_debug.h"

using namespace std;

namespace NLP { namespace CCG {

void
DebugPrinter::header(const std::string &PREFACE){
  out.stream << PREFACE << endl;
  log.stream << PREFACE << endl;
}


static std::ostream &
output_cat(const SuperCat *sc, std::ostream &out, unsigned int depth) {
  for (unsigned int i = 0; i != depth; ++i) {
    out << '|';
    if (i == depth - 1)
      out << "-- ";
    else
      out << "   ";
  }
  out << Port::BOLD;
  sc->cat->out_novar(out, false);
  out << Port::OFF;
  return out;
}


void
DebugPrinter::recurse(const SuperCat *sc, Sentence &sent, unsigned int depth){
  if (sc->left) { 
    assert(sc->left->max);
    output_cat(sc, out.stream, depth);
    out.stream << /*" (" << (sc->right ? 2 : 1) << ")"*/ '\n';
    recurse(sc->left->max, sent, depth + 1);
    if (sc->right) {
      assert(sc->right->max);
      recurse(sc->right->max, sent, depth + 1);
    }
  }
  else { /* leaf */
    Position pos = (sc->vars[sc->cat->var]).pos();
    output_cat(sc, out.stream, depth);
    out.stream << ' ' << Port::RED << sent.words[pos - 1] << Port::OFF << '\n';
  }
}

void
DebugPrinter::derivation(const SuperCat *sc, Sentence &sent){
  recurse(sc, sent, 0);
}

} }
