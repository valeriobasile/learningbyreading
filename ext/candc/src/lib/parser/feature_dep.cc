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
#include "parser/feature_dep.h"

using namespace std;

namespace NLP { namespace CCG {

void
DepFeature::load(istream &in, const std::string &filename, ulong id, Type type){
  string tmp;
  ulong itmp;
  in >> tmp;
  Word head = lexicon[tmp];
  if(!head)
    throw NLP::IOException("could not find dependency feature head in lexicon " + tmp, filename, id);

  RelID rel;
  RuleID rule;
  CatID lrange;
  in >> itmp >> tmp;
  rel = itmp;

  Word var = lexicon[tmp];
  if(!var)
    throw NLP::IOException("could not find dependency feature var in lexicon " + tmp, filename, id);

  in >> itmp;
  rule = itmp;
  in >> itmp;
  lrange = itmp;

  dep_attrs.insert(id, type, head, rel, var, rule, lrange, 0);
}


ulong
DepFeature::get_id(istream &in, const std::string &, Type type,
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
  in >> itmp >> tmp;
  rel = itmp;

  Word var = lexicon[tmp];
  if(!var)
    return 0;

  in >> itmp;
  rule = itmp;
  in >> itmp;
  lrange = itmp;

  return dep_attrs(type, head, rel, var, rule, lrange, 0);
}


void
DepFeature::_add(const Filled *filled, Type type, const Words &heads, const Words &words,
		 std::vector<ulong> &ids) const {
  Word head = heads[filled->head - 1];
  if(!head)
    return;

  Word value = words[filled->filler - 1];
  if(!value)
    return;

  ulong id = dep_attrs(type, head, filled->rel, value, filled->rule, filled->lrange, 0);
  if(id)
    ids.push_back(id - 1);
}

void
DepFeature::add(const SuperCat *sc, const Words &words, const Words &tags,
		Type, std::vector<ulong> &ids) const {
  for(const Filled *filled = sc->filled; filled; filled = filled->next){
    _add(filled, DEP_WORD, words, words, ids);
    _add(filled, DEP_POS, tags, tags, ids);
    _add(filled, DEP_WORD_POS, words, tags, ids);
    _add(filled, DEP_POS_WORD, tags, words, ids);
  }
}



double
DepFeature::_score(const Filled *filled, Type type,
		   const Words &heads, const Words &words) const {
  Word head = heads[filled->head - 1];
  if(!head)
    return 0.0;

  Word value = words[filled->filler - 1];
  if(!value)
    return 0.0;

  return dep_attrs.weight(type, head, filled->rel, value, filled->rule, filled->lrange, 0);
}



double
DepFeature::score(const SuperCat *sc, const Words &words, const Words &tags, Type) const {
  double score = 0.0;

  for(const Filled *filled = sc->filled; filled; filled = filled->next){
    score += _score(filled, DEP_WORD, words, words);
    score += _score(filled, DEP_POS, tags, tags);

    score += _score(filled, DEP_WORD_POS, words, tags);
    score += _score(filled, DEP_POS_WORD, tags, words);
  }
  return score;
}

} }
