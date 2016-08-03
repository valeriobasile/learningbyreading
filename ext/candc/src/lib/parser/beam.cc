// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_parser.h"

#include <limits>

using namespace std;

namespace NLP { namespace CCG {

double
Parser::_Impl::calc_beam_score(SuperCat *sc, const Words &words, const Words &tags){
  if(sc->left){
    if(sc->right)
      return calc_beam_score_binary(sc, words, tags);
    else
      return calc_beam_score_unary(sc, words, tags);
  }else
    return calc_beam_score_leaf(sc, words, tags);
}


double
Parser::_Impl::calc_beam_score_binary(SuperCat *sc, const Words &words, const Words &tags){
  sc->score = sc->left->score + sc->right->score;
  sc->score += score_binary_feats(sc, words, tags);

  return sc->score;
}


double
Parser::_Impl::calc_beam_score_unary(SuperCat *sc, const Words &words, const Words &tags){
  sc->score = sc->left->score;
  sc->score += rule_feats.score(sc, words, tags, URULE);
  sc->score += rule_head_feats.score(sc, words, tags, URULE);
  sc->score += genrule_feats.score(sc, words, tags, GEN_RULE); //sc: last 3 args not used

  return sc->score;
}


double
Parser::_Impl::calc_beam_score_leaf(SuperCat *sc, const Words &words, const Words &tags){
  sc->score = cat_feats.score(sc, words, tags, LEX);
  return sc->score;
}


void
Parser::_Impl::calc_beam_scores(Cell &cell, const Words &words, const Words &tags){
  // just use the canonical cat to store the inside score
  for(Cell::iterator i = cell.begin(); i != cell.end(); ++i){
    (*i)->score = calc_beam_score(*i, words, tags);
    for(SuperCat *equiv = (*i)->next; equiv; equiv = const_cast<SuperCat *>(equiv->next)){
      equiv->score = calc_beam_score(equiv, words, tags);
      (*i)->score = add_logs((*i)->score, equiv->score);
    }
  }
}

class BelowBeam {
private:
  double cutoff;
public:
  BelowBeam(double cutoff): cutoff(cutoff){};
  bool operator()(const SuperCat *sc){
    return sc->score < cutoff; 
  }
};

void
Parser::_Impl::apply_beam(Cell &cell, double beam){
  double max_score = -numeric_limits<double>::max();
  for(Cell::iterator i = cell.begin(); i != cell.end(); ++i)
    max_score = max(max_score, (*i)->score);

  BelowBeam below_beam(max_score + log(beam));
  cell.erase(remove_if(cell.begin(), cell.end(), below_beam), cell.end());
}


} }
