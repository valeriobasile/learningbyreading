// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_decoder.h"

#include <limits>

using namespace std;

namespace NLP { namespace CCG {

const SuperCat *
Decoder::best_equiv(const SuperCat *sc){
  if(sc->max)
    return sc->max;

  const SuperCat *max_sc = 0;
  volatile double max_score = -numeric_limits<double>::max();

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    double current = best_score(equiv);
    if(current > max_score){
      max_score = current;
      max_sc = equiv;
    }
  }

  assert(max_sc->score == max_score);
  return sc->max = max_sc;
}

const SuperCat *
Decoder::best(Chart &chart){
  Cell &root = chart.root();

  const SuperCat *max_sc = 0;
  double max_score = -numeric_limits<double>::max();

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    const SuperCat *current = best_equiv(*i);
    if(current->score > max_score){
      max_score = current->score;
      max_sc = current;
    }
  }

  return max_sc;
}

} }
