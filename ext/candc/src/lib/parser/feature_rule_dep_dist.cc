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
#include "parser/feature_rule_dep_dist.h"

using namespace std;

namespace NLP { namespace CCG {

void
RuleDepDistFeature::load(istream &in, const std::string &filename, ulong id, Type type){
  string cat1str, cat2str, cat3str, head;
  ushort count;
  if(!(in >> cat1str >> cat2str >> cat3str >> head >> count))
    throw NLP::IOException("error parsing rule feature in load_rule_dep_dist", filename, id);

  Word value = lexicon[head];
  if(!value)
    throw NLP::IOException("could not find value in lexicon " + head, filename, id);

  try {
    const Cat *cat1 = cats.canonize(cat1str.c_str());
    const Cat *cat2 = cats.canonize(cat2str.c_str());
    const Cat *cat3 = cats.canonize(cat3str.c_str());

    depdist_attrs.insert(id, type, cat1, cat2, cat3, value, count);  

  }catch(NLP::Exception e){
    throw NLP::IOException("error parsing category in rule feature ", filename, id);
  }
}

ulong
RuleDepDistFeature::get_id(std::istream &in, const std::string &, Type type,
			   vector<long> &) const{
  string cat1str, cat2str, cat3str, head;
  ushort count;
  if(!(in >> cat1str >> cat2str >> cat3str >> head >> count))
    throw NLP::IOException("error parsing rule feature in load_feature");
  
  Word value = lexicon[head];
  if(!value)
    return 0;
  
  const Cat *cat1 = cats.canonize(cat1str.c_str());
  const Cat *cat2 = cats.canonize(cat2str.c_str());
  const Cat *cat3 = cats.canonize(cat3str.c_str());
  
  return depdist_attrs(type, cat1, cat2, cat3, value, count);
}

void
RuleDepDistFeature::_add(const Cat *catl, const Cat *catr, const Cat *cat,
			 const Variable *varl, const Variable *varr, const Variable *var,
			 Type type, const Words &values, const Words &tags,
			 std::vector<ulong> &ids) const {
  const Position *const end = var->fillers + Variable::NFILLERS;
  const Position *const endl = varl->fillers + Variable::NFILLERS;
  const Position *const endr = varr->fillers + Variable::NFILLERS;

  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    Word value = values[*p - 1];
    if(!value)
      continue;

    for(const Position *l = varl->fillers; l != endl && *l != Variable::SENTINEL; ++l){
      if(!*l)
	continue;

      for(const Position *r = varr->fillers; r != endr && *r != Variable::SENTINEL; ++r){
	if(!*r)
	  continue;

	if(*p != *l && *p != *r)
	  continue;
	
	ushort dist = calc_dist(type, *l, *r, tags);
	ulong id = depdist_attrs(type, catl, catr, cat, value, dist);
	if(id)
	  ids.push_back(id - 1);
      }
    }
  }
}

void
RuleDepDistFeature::_add(const SuperCat *scl, const SuperCat *scr, const SuperCat *sc, 
			 Type type, const Words &values, const Words &tags, 
			 std::vector<ulong> &ids) const {
  const Cat *catl = scl->cat;
  const Cat *catr = scr->cat;
  const Cat *cat = sc->cat;

  _add(catl, catr, cat, &scl->vars[catl->var], &scr->vars[catr->var], &sc->vars[cat->var], 
       type, values, tags, ids);
}


void
RuleDepDistFeature::add(const SuperCat *sc, const Words &words, const Words &tags,
			Type, std::vector<ulong> &ids) const {
  _add(sc->left, sc->right, sc, DIST_ADJ_HEAD, words, tags, ids);
  _add(sc->left, sc->right, sc, DIST_VERBS_HEAD, words, tags, ids);
  _add(sc->left, sc->right, sc, DIST_PUNCT_HEAD, words, tags, ids);
  _add(sc->left, sc->right, sc, DIST_ADJ_POS, tags, tags, ids);
  _add(sc->left, sc->right, sc, DIST_VERBS_POS, tags, tags, ids);
  _add(sc->left, sc->right, sc, DIST_PUNCT_POS, tags, tags, ids);
}


double
RuleDepDistFeature::_score(const Cat *catl, const Cat *catr, const Cat *cat, Type type, 
			   const Word value, ushort dist) const {
  if(!value)
    return 0.0;

  return depdist_attrs.weight(type, catl, catr, cat, value, dist);
}


double
RuleDepDistFeature::_score(const Cat *catl, const Cat *catr, const Cat *cat, 
			   const Variable *varl, const Variable *varr, const Variable *var,
			   Type type, const Words &values, const Words &tags) const {
  double weight = 0.0;

  const Position *const end = var->fillers + Variable::NFILLERS;
  const Position *const endl = varl->fillers + Variable::NFILLERS;
  const Position *const endr = varr->fillers + Variable::NFILLERS;

  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    Word value = values[*p - 1];
    if(!value)
      continue;

    for(const Position *l = varl->fillers; l != endl && *l != Variable::SENTINEL; ++l){
      if(!*l)
        continue;

      for(const Position *r = varr->fillers; r != endr && *r != Variable::SENTINEL; ++r){
        if(!*r)
          continue;

	if(*p != *l && *p != *r)
	  continue;

	ushort dist = calc_dist(type, *l, *r, tags);
	weight += _score(catl, catr, cat, type, value, dist);
      }
    }
  }

  return weight;  
}
  
double
RuleDepDistFeature::_score(const SuperCat *scl, const SuperCat *scr, const SuperCat *sc,
			   Type type, const Words &values, const Words &tags) const {
  const Cat *catl = scl->cat;
  const Cat *catr = scr->cat;
  const Cat *cat = sc->cat;

  return _score(catl, catr, cat, &scl->vars[catl->var], &scr->vars[catr->var],
		&sc->vars[cat->var], type, values, tags);
}

double
RuleDepDistFeature::score(const SuperCat *sc, const Words &words, const Words &tags, Type) const {
  double score = 0.0;

  score += _score(sc->left, sc->right, sc, DIST_ADJ_HEAD, words, tags);
  score += _score(sc->left, sc->right, sc, DIST_VERBS_HEAD, words, tags);
  score += _score(sc->left, sc->right, sc, DIST_PUNCT_HEAD, words, tags);
  score += _score(sc->left, sc->right, sc, DIST_ADJ_POS, tags, tags);
  score += _score(sc->left, sc->right, sc, DIST_VERBS_POS, tags, tags);
  score += _score(sc->left, sc->right, sc, DIST_PUNCT_POS, tags, tags);

  return score;
}

} }
