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
#include "parser/inside_outside.h"
#include "parser/decoder_deps_recall.h"

namespace NLP { namespace CCG {

DepsRecallDecoder::DepsRecallDecoder(void): inside_outside(new InsideOutside()) {}

DepsRecallDecoder::~DepsRecallDecoder(void){
  delete inside_outside;
}

const SuperCat *
DepsRecallDecoder::best(Chart &chart){
  inside_outside->calc(chart);
  return Decoder::best(chart);
}

double
DepsRecallDecoder::best_score(const SuperCat *sc){
  double score = 0.0;

  if(sc->left){
    const SuperCat *b = best_equiv(sc->left);
    score += b->score;

    if(sc->right){
      const SuperCat *b = best_equiv(sc->right);
      score += b->score;
    }
  }

  for(const Filled *filled = sc->filled; filled; filled = filled->next)
    score += inside_outside->depscores[filled] / filled->conj;

  assert(score >= 0.0);
  return sc->score = score;
}


} }
