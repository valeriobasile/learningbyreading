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
#include "parser/decoder_derivs.h"

namespace NLP { namespace CCG {

double
DerivsDecoder::best_score(const SuperCat *sc){
  double score = sc->score;

  if(sc->left){
    const SuperCat *b = best_equiv(sc->left);
    score += b->score;

    if(sc->right){
      const SuperCat *b = best_equiv(sc->right);
      score += b->score;
    }
  }
  return sc->score = score;
}

} }
