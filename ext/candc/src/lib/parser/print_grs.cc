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
#include "parser/print_deps.h"
#include "parser/print_grs.h"

using namespace std;

namespace NLP { namespace CCG {

void
GRsPrinter::derivation(const SuperCat *sc, Sentence &sent){
  get_grs(sc, sent);
  for(GRs::const_iterator i = grs.begin(); i != grs.end(); ++i){
    out.stream << '(' << i->label;
    for(Arguments::const_iterator j = i->args.begin(); j != i->args.end(); ++j){
      out.stream << ' ' << j->raw;
      if(j->pos >= 0)
	out.stream << '_' << j->pos;
    }
    out.stream << ')' << endl;
  }
}

void
GRsPrinter::get_grs(const SuperCat *sc, Sentence &sent){
  if(sc->left){
    assert(sc->left->max);
    get_grs(sc->left->max, sent);
    if(sc->right){
      assert(sc->right->max);
      get_grs(sc->right->max, sent);
    }
  }

  //store the lexical categories for printing
  if(!sc->left)
    sent.cats.push_back(sc->cat);

  sc->get_grs(grs, cats.relations, filled, sent);
}

} }
