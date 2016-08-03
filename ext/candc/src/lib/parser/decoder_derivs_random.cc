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
#include "parser/decoder_derivs_random.h"

namespace NLP { namespace CCG {

double
DerivsRandomDecoder::best_score(const SuperCat *sc){
  if(sc->left){
    best_equiv(sc->left);

    if(sc->right)
      best_equiv(sc->right);
  }

  return sc->score = rand();
}

} }
