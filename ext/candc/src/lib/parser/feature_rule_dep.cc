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
#include "parser/feature_rule_dep.h"

using namespace std;

namespace NLP { namespace CCG {

void
RuleDepFeature::load(istream &in, const std::string &filename, ulong id, Type type){
  string cat1str, cat2str, cat3str, head1, head2;
  if(!(in >> cat1str >> cat2str >> cat3str >> head1 >> head2))
    throw NLP::IOException("error parsing rule feature in load_rule_dep", filename, id);

  Word value1 = lexicon[head1];
  if(!value1)
    throw NLP::IOException("could not find value in lexicon " + head1, filename, id);

  Word value2 = lexicon[head2];
  if(!value2)
    throw NLP::IOException("could not find value in lexicon " + head2, filename, id);

  try {
    const Cat *cat1 = cats.canonize(cat1str.c_str());
    const Cat *cat2 = cats.canonize(cat2str.c_str());
    const Cat *cat3 = cats.canonize(cat3str.c_str());

    rule_attrs.insert(id, type, cat1, cat2, cat3, value1, value2);  

  }catch(NLP::Exception e){
    throw NLP::IOException("error parsing category in rule feature ", filename, id);
  }
}

ulong
RuleDepFeature::get_id(std::istream &in, const std::string &filename, Type type,
		       std::vector<long> &) const{
  string cat1str, cat2str, cat3str, head1, head2;
  if(!(in >> cat1str >> cat2str >> cat3str >> head1 >> head2))
    throw NLP::IOException("error parsing rule feature ", filename);
  
  Word value1 = lexicon[head1];
  Word value2 = lexicon[head2];
  if(!value1 || !value2)
    return 0;
  
  const Cat *cat1 = cats.canonize(cat1str.c_str());
  const Cat *cat2 = cats.canonize(cat2str.c_str());
  const Cat *cat3 = cats.canonize(cat3str.c_str());

  return rule_attrs(type, cat1, cat2, cat3, value1, value2);
}


void
RuleDepFeature::_add(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var1,
		     const Variable *var2, Type type, const Words &words1, const Words &words2,
		     std::vector<ulong> &ids) const {
  const Position *const end1 = var1->fillers + Variable::NFILLERS;
  const Position *const end2 = var2->fillers + Variable::NFILLERS;
  for(const Position *p = var1->fillers; p != end1 && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    Word value1 = words1[*p - 1];
    if(!value1)
      continue;

    for(const Position *q = var2->fillers; q != end2 && *q != Variable::SENTINEL; ++q){
      if(!*q)
	continue;

      Word value2 = words2[*q - 1];
      if(!value2)
        continue;

      ulong id = rule_attrs(type, cat1, cat2, cat3, value1, value2);
      if(id)
        ids.push_back(id - 1);
    }
  }
}


void
RuleDepFeature::_add(const Cat *cat1, const Cat *cat2, const Cat *cat3, const SuperCat *sc1,
		     const SuperCat *sc2, Type type, const Words &words1, const Words &words2,
		     std::vector<ulong> &ids) const {
  _add(cat1, cat2, cat3, &sc1->vars[sc1->cat->var], &sc2->vars[sc2->cat->var], type,
       words1, words2, ids);
}


void
RuleDepFeature::add(const SuperCat *sc, const Words &words, const Words &tags, Type,
		    std::vector<ulong> &ids) const {
  _add(sc->left->cat, sc->right->cat, sc->cat, sc->left, sc->right, BRULE_HEAD_HEAD,
       words, words, ids);
  _add(sc->left->cat, sc->right->cat, sc->cat, sc->left, sc->right, BRULE_POS_HEAD, 
       tags, words, ids);
  _add(sc->left->cat, sc->right->cat, sc->cat, sc->left, sc->right, BRULE_HEAD_POS, 
       words, tags, ids);
  _add(sc->left->cat, sc->right->cat, sc->cat, sc->left, sc->right, BRULE_POS_POS,
       tags, tags, ids);
}


double
RuleDepFeature::_score(const Cat *cat1, const Cat *cat2, const Cat *cat3, Type type, 
		       const Word value1, const Word value2) const {
  if(!value1 || !value2)
    return 0.0;

  return rule_attrs.weight(type, cat1, cat2, cat3, value1, value2);
}


double
RuleDepFeature::_score(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var1,
		       const Variable *var2, Type type, const Words &words1, 
		       const Words &words2) const {
  double weight = 0.0;
  const Position *const end1 = var1->fillers + Variable::NFILLERS;
  const Position *const end2 = var2->fillers + Variable::NFILLERS;
  for(const Position *p = var1->fillers; p != end1 && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    Word value1 = words1[*p - 1];
    if(!value1)
      continue;

    for(const Position *q = var2->fillers; q != end2 && *q != Variable::SENTINEL; ++q){
      if(!*q)
        continue;

      Word value2 = words2[*q - 1];
      if(!value2)
	continue;

      weight += _score(cat1, cat2, cat3, type, value1, value2);
    }
  }

  return weight;
}

double
RuleDepFeature::_score(const Cat *cat1, const Cat *cat2, const Cat *cat3, const SuperCat *sc1,
		       const SuperCat *sc2, Type type, const Words &words1,
		       const Words &words2) const {
  return _score(cat1, cat2, cat3, &sc1->vars[sc1->cat->var], &sc2->vars[sc2->cat->var],
		type, words1, words2);
}

double
RuleDepFeature::score(const SuperCat *sc, const Words &words, const Words &tags, Type) const {
  double score = 0.0;

  score += _score(sc->left->cat, sc->right->cat, sc->cat, sc->left, sc->right, BRULE_HEAD_HEAD,
		  words, words);
  score += _score(sc->left->cat, sc->right->cat, sc->cat, sc->left, sc->right, BRULE_POS_HEAD,
		  tags, words);
  score += _score(sc->left->cat, sc->right->cat, sc->cat, sc->left, sc->right, BRULE_HEAD_POS,
		  words, tags);
  score += _score(sc->left->cat, sc->right->cat, sc->cat, sc->left, sc->right, BRULE_POS_POS,
		  tags, tags);

  return score;
}

} }
