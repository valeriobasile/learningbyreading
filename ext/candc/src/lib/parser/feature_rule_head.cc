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
#include "parser/feature_rule_head.h"

using namespace std;

namespace NLP { namespace CCG {

void
RuleHeadFeature::load(istream &in, const std::string &filename, ulong id, Type type){
  string cat1str, cat2str, cat3str, tmp;
  if(!(in >> cat1str >> cat2str >> cat3str >> tmp))
    throw NLP::IOException("error parsing rule feature in load_rule_head", filename, id);

  Word value = lexicon[tmp];
  if(!value)
    throw NLP::IOException("could not find value in lexicon " + tmp, filename, id);

  try {
    const Cat *cat1 = cats.canonize(cat1str.c_str());
    const Cat *cat2 = cats.canonize(cat2str.c_str());
    const Cat *cat3 = cats.canonize(cat3str.c_str());

    rule_attrs.insert(id, type, cat1, cat2, cat3, value, Word());
  }catch(NLP::Exception e){
    throw NLP::IOException("error parsing category in rule feature ", filename, id);
  }
}

ulong
RuleHeadFeature::get_id(istream &in, const std::string &filename,
			Type type, std::vector<long> &) const{
  string cat1str, cat2str, cat3str, tmp;
  if(!(in >> cat1str >> cat2str >> cat3str >> tmp))
    throw NLP::IOException("error parsing rule feature ", filename);
  
  Word value = lexicon[tmp];
  if(!value)
    return 0;

  const Cat *cat1 = cats.canonize(cat1str.c_str());
  const Cat *cat2 = cats.canonize(cat2str.c_str());
  const Cat *cat3 = cats.canonize(cat3str.c_str());

  return rule_attrs(type, cat1, cat2, cat3, value, Word());
}

void
RuleHeadFeature::_add(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var,
		      Type type, const Words &words, std::vector<ulong> &ids) const {
  const Position *const end = var->fillers + Variable::NFILLERS;
  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;
    Word value = words[*p - 1];
    if(!value)
      continue;

    ulong id = rule_attrs(type, cat1, cat2, cat3, value, Word());
    if(id)
      ids.push_back(id - 1);
  }
}

void
RuleHeadFeature::_add(const Cat *cat1, const Cat *cat2, const Cat *cat3, const SuperCat *sc,
		      Type type, const Words &words, std::vector<ulong> &ids) const {
  _add(cat1, cat2, cat3, &sc->vars[sc->cat->var], type, words, ids);
}

void
RuleHeadFeature::add(const SuperCat *sc, const Words &words, const Words &tags,
		     Type type, std::vector<ulong> &ids) const {
  if(type == BRULE){
    _add(sc->left->cat, sc->right->cat, sc->cat, sc, BRULE_HEAD, words, ids);
    _add(sc->left->cat, sc->right->cat, sc->cat, sc, BRULE_POS, tags, ids);
  }
  else if(type == URULE){
    _add(sc->left->cat, sc->cat, sc->cat, sc, URULE_HEAD, words, ids);
    _add(sc->left->cat, sc->cat, sc->cat, sc, URULE_POS, tags, ids);
  }else
    throw NLP::IOException("need URULE or BRULE type as argument to add_rule function");
}

double
RuleHeadFeature::_score(const Cat *cat1, const Cat *cat2, const Cat *cat3, Type type,
			const Word value) const {
  if(!value)
    return 0.0;

  return rule_attrs.weight(type, cat1, cat2, cat3, value, Word());
}

double
RuleHeadFeature::_score(const Cat *cat1, const Cat *cat2, const Cat *cat3,
			const Variable *var, Type type, const Words &words) const {
  double weight = 0.0;
  const Position *const end = var->fillers + Variable::NFILLERS;
  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;
    
    Word value = words[*p - 1];
    if(!value)
      continue;

    weight += _score(cat1, cat2, cat3, type, value);
  }

  return weight;
}

double
RuleHeadFeature::_score(const Cat *cat1, const Cat *cat2, const Cat *cat3,
			const SuperCat *sc, Type type, const Words &words) const {
  return _score(cat1, cat2, cat3, &sc->vars[sc->cat->var], type, words);
}

double
RuleHeadFeature::score(const SuperCat *sc, const Words &words,
		       const Words &tags, Type type) const {
  double score = 0.0;
  
  if(type == BRULE){
    score += _score(sc->left->cat, sc->right->cat, sc->cat, sc, BRULE_HEAD, words);
    score += _score(sc->left->cat, sc->right->cat, sc->cat, sc, BRULE_POS, tags);
  }
  else if(type == URULE){
    score += _score(sc->left->cat, sc->cat, sc->cat, sc, URULE_HEAD, words);
    score += _score(sc->left->cat, sc->cat, sc->cat, sc, URULE_POS, tags);
  }else
    throw NLP::IOException("need URULE or BRULE type as argument to score_rule function");

  return score;
}

} }
