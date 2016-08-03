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
#include "parser/feature_rule.h"

using namespace std;

namespace NLP { namespace CCG {

void
RuleFeature::load(istream &in, const std::string &filename, ulong id, Type type){
  std::string cat1str, cat2str, cat3str;
  if(!(in >> cat1str >> cat2str >> cat3str))
    throw NLP::IOException("error parsing rule feature in load", filename, id);

  try {
    const Cat *cat1 = cats.canonize(cat1str.c_str());
    const Cat *cat2 = cats.canonize(cat2str.c_str());
    const Cat *cat3 = cats.canonize(cat3str.c_str());
    
    rule_attrs.insert(id, type, cat1, cat2, cat3, Word(), Word());
  }catch(NLP::Exception e){
    throw NLP::IOException("error parsing category in rule feature ", filename, id);
  }
}

ulong
RuleFeature::get_id(istream &in, const std::string &filename, Type type,
		    vector<long> &rules) const{
  string cat1str, cat2str, cat3str;
  if(!(in >> cat1str >> cat2str >> cat3str))
    throw NLP::IOException("error parsing rule feature in load_feature", filename);
    
  const Cat *cat1 = cats.canonize(cat1str.c_str());
  const Cat *cat2 = cats.canonize(cat2str.c_str());
  const Cat *cat3 = cats.canonize(cat3str.c_str());

  ulong id = rule_attrs(type, cat1, cat2, cat3, Word(), Word());

  // TODO this only allows rules with frequency above the cutoff
  // -1 indicates rule not a feature
  rules.push_back(id - 1);

  return id;
}

void
RuleFeature::_add(const Cat *cat1, const Cat *cat2, const Cat *cat3,
		  Type type, std::vector<ulong> &ids) const {
  ulong id = rule_attrs(type, cat1, cat2, cat3, Word(), Word());
  if(id)
    ids.push_back(id - 1);
}

void
RuleFeature::add(const SuperCat *sc, const Words &, const Words &,
		 Type type, std::vector<ulong> &ids) const {
  if(type == BRULE)
    _add(sc->left->cat, sc->right->cat, sc->cat, BRULE, ids);
  else if(type == URULE)
    _add(sc->left->cat, sc->cat, sc->cat, URULE, ids);
  else
    throw NLP::IOException("need URULE or BRULE type as argument to add_rule function");
}


double
RuleFeature::_score(const Cat *cat1, const Cat *cat2, const Cat *cat3, Type type) const {
  return rule_attrs.weight(type, cat1, cat2, cat3, Word(), Word());
}

double
RuleFeature::score(const SuperCat *sc, const Words &, const Words &, Type type) const {
  if(type == BRULE)
    return _score(sc->left->cat, sc->right->cat, sc->cat, BRULE);
  else if(type == URULE)
    return _score(sc->left->cat, sc->cat, sc->cat, URULE);
  else
    throw NLP::IOException("need URULE or BRULE type as argument to score_rule function");

  return 0.0;
}

} }
