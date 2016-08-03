// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_feature_type.h"
#include "parser/feature_dist_type.h"
#include "parser/feature_dep_dist.h"

using namespace std;

namespace NLP { namespace CCG {

void
DepDistFeature::load(istream &in, const std::string &filename, ulong id, Type type){
  string tmp;
  ulong itmp;
  ushort dist;
  in >> tmp;
  Word head = lexicon[tmp];
  if(!head)
    throw NLP::IOException("could not find dist dependency feature head in lexicon " + tmp, filename, id);

  RelID rel;
  RuleID rule;
  CatID lrange;
  in >> itmp;
  rel = itmp;
  in >> itmp;
  rule = itmp;
  in >> itmp;
  lrange = itmp;
      
  in >> dist;

  dep_attrs.insert(id, type, head, rel, Word(), rule, lrange, dist);
}

ulong
DepDistFeature::get_id(istream &in, const std::string &, Type type,
		       std::vector<long> &) const {
  std::string tmp;
  in >> tmp;
  int itmp;
  Word head = lexicon[tmp];
  if(!head)
    return 0;

  RelID rel;
  RuleID rule;
  CatID lrange;
  in >> itmp;
  rel = itmp;
  in >> itmp;
  rule = itmp;
  in >> itmp;
  lrange = itmp;

  ushort dist;
  in >> dist;

  return dep_attrs(type, head, rel, Word(), rule, lrange, dist);
}


void
DepDistFeature::_add(const Filled *filled, Type type, const Words &heads, 
		     const Words &tags, std::vector<ulong> &ids) const {
  Word head = heads[filled->head - 1];
  if(!head)
    return;

  Position lpos, rpos;
  if(filled->head < filled->filler){
    lpos = filled->head;
    rpos = filled->filler;
  }else{
    rpos = filled->head;
    lpos = filled->filler;
  }
  ushort dist = calc_dist(type, lpos, rpos, tags);

  ulong id = dep_attrs(type, head, filled->rel, Word(), filled->rule, filled->lrange, dist);
  if(id)
    ids.push_back(id - 1);
}


void
DepDistFeature::add(const SuperCat *sc, const Words &words, const Words &tags,
		    Type, std::vector<ulong> &ids) const {
  for(const Filled *filled = sc->filled; filled; filled = filled->next){
    _add(filled, DIST_ADJ_HEAD, words, tags, ids);
    _add(filled, DIST_ADJ_POS, tags, tags, ids);
    _add(filled, DIST_VERBS_HEAD, words, tags, ids);
    _add(filled, DIST_VERBS_POS, tags, tags, ids);
    _add(filled, DIST_PUNCT_HEAD, words, tags, ids);
    _add(filled, DIST_PUNCT_POS, tags, tags, ids);
  }
}

double
DepDistFeature::_score(const Filled *filled, Type type,
		       const Words &heads, const Words &tags) const {
  Word head = heads[filled->head - 1];
  if(!head)
    return 0.0;

  Position lpos, rpos;
  if(filled->head < filled->filler){
    lpos = filled->head;
    rpos = filled->filler;
  }else{
    rpos = filled->head;
    lpos = filled->filler;
  }
  
  ushort dist = calc_dist(type, lpos, rpos, tags);

  return dep_attrs.weight(type, head, filled->rel, Word(), filled->rule, filled->lrange, dist);
}


double
DepDistFeature::score(const SuperCat *sc, const Words &words, const Words &tags, Type) const {
  double score = 0.0;

  for(const Filled *filled = sc->filled; filled; filled = filled->next){
    score += _score(filled, DIST_ADJ_HEAD, words, tags);
    score += _score(filled, DIST_ADJ_POS, tags, tags);
    score += _score(filled, DIST_VERBS_HEAD, words, tags);
    score += _score(filled, DIST_VERBS_POS, tags, tags);
    score += _score(filled, DIST_PUNCT_HEAD, words, tags);
    score += _score(filled, DIST_PUNCT_POS, tags, tags);
  }
  return score;
}


} }
