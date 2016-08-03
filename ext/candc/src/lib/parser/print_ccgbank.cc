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
#include "parser/print_ccgbank.h"

using namespace std;

namespace NLP { namespace CCG {

void
CCGbankPrinter::header(const std::string &PREFACE){
  log.stream << PREFACE << endl;
}

void
CCGbankPrinter::unary(Sentence &sent){
  out.stream << "(<L " << sent.msuper[0][0].raw << ' ';
  out.stream << sent.pos[0] << ' ' << sent.pos[0];
  out.stream << sent.msuper[0][0].raw << ">)" << endl;
}

void
CCGbankPrinter::recurse(const SuperCat *sc, Sentence &sent, int index){
  if(sc->left){
    assert(sc->left->max);
    out.stream << "(<T ";
    sc->cat->out_novar(out.stream, false);
    out.stream << ' ' << index << ' ' << (sc->right ? 2 : 1) << "> ";
    recurse(sc->left->max, sent, 0);

    if(sc->right){
      out.stream << ' ';
      assert(sc->right->max);
      recurse(sc->right->max, sent, 1);
    }
    out.stream << ')';
  }else{
    // leaf case (<L NP[nb]/N DT DT the NP[nb]_131/N_131>)
    Position pos = (sc->vars[sc->cat->var]).pos();
    out.stream << "(<L ";
    sc->cat->out_novar(out.stream, false) << ' ';
    out.stream << sent.pos[pos - 1] << ' ' << sent.pos[pos - 1] << ' ';
    out.stream << sent.words[pos - 1] << ' ';
    sc->cat->out_novar(out.stream, false);
    out.stream << ">)";
  }
}

void
CCGbankPrinter::derivation(const SuperCat *sc, Sentence &sent){
  out.stream << "ID=" << nsentences << " PARSER=GOLD NUMPARSE=1" << endl;
  recurse(sc, sent, 1);
}

void
CCGbankPrinter::lexical(Sentence &){}

} }
