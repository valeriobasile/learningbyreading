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
#include "parser/feature_cat.h"

using namespace std;

namespace NLP { namespace CCG {

void
CatFeature::load(istream &in, const std::string &filename, ulong nfeatures, Type type){
  string tmp;
  in >> tmp;
  const Cat *cat = cats.markedup[tmp];
  if(!cat){
    if(type >= ROOT && type <= ROOT_POS){
      try {
	cat = cats.parse(tmp);
      }catch(NLP::Exception e){
	cerr << "maxent.exception: " << e.what() << endl;
	cerr << "attempting to parse category " << tmp << " on line " << nfeatures << endl;
      }
    }else
      throw NLP::IOException("could not find category in markedup " + tmp, filename, nfeatures);
  }
  if(type == ROOT)
    cv_attrs.insert(nfeatures, type, cat, Word());
  else{
    in >> tmp;
    Word value = lexicon[tmp];
    if(!value)
      throw NLP::IOException("could not find value in lexicon " + tmp, filename, nfeatures);
    cv_attrs.insert(nfeatures, type, cat, value);
  }
}

ulong
CatFeature::get_id(istream &in, const std::string &filename, Type type,
		   std::vector<long> &) const {
  std::string tmp;
  in >> tmp;
  const Cat *cat = cats.markedup[tmp];
  if(!cat){
    if(type >= ROOT && type <= ROOT_POS){
      try {
	cat = cats.parse(tmp);
      }catch(NLP::Exception e){
	cerr << "maxent.exception: " << e.what() << endl;
	cerr << "attempting to parse category " << tmp << endl;
      }
    }else
      throw NLP::IOException("could not find category in markedup " + tmp, filename);
  }

  if(type == ROOT)
    return cv_attrs(type, cat, Word());
  else{
    in >> tmp;
    Word value = lexicon[tmp];
    if(!value)
      return 0;
    return cv_attrs(type, cat, value);
  }
}

void
CatFeature::_add_cv(const SuperCat *sc, const Variable *var, Type type, const Words &values,
		    std::vector<ulong> &ids) const {
  const Position *const end = var->fillers + Variable::NFILLERS;
  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    Word value = values[*p - 1];
    if(!value)
      continue;

    ulong id = cv_attrs(type, sc->cat, value);
    if(id)
      ids.push_back(id - 1);
  }
}

void
CatFeature::_add_c(const SuperCat *sc, Type type, std::vector<ulong> &ids) const {
  ulong id;
  if((id = cv_attrs(type, sc->cat, Word())))
    ids.push_back(id - 1);
}

void
CatFeature::_add_cv(const SuperCat *sc, Type type, const Words &values,
			   std::vector<ulong> &ids) const {
  _add_cv(sc, &sc->vars[sc->cat->var], type, values, ids);
}

void
CatFeature::add(const SuperCat *sc, const Words &words, const Words &tags,
			  Type type,  std::vector<ulong> &ids) const {
  if(type == LEX){
    _add_cv(sc, LEX_WORD, words, ids);
    _add_cv(sc, LEX_POS, tags, ids);
  }else if(type == ROOT){
    _add_c(sc, ROOT, ids);
    _add_cv(sc, ROOT_WORD, words, ids);
    _add_cv(sc, ROOT_POS, tags, ids);
  }else
    throw NLP::IOException("need LEX or ROOT type as argument to add function");
}

double
CatFeature::_score_c(const SuperCat *sc, Type type) const {
  return cv_attrs.weight(type, sc->cat, Word());
}

double
CatFeature::_score_cval(const SuperCat *sc, Type type, const Word value) const {
  if(!value)
    return 0.0;
  return cv_attrs.weight(type, sc->cat, value);
}

double
CatFeature::_score_cvar(const SuperCat *sc, const Variable *var, Type type,
			const Words &values) const {
  double weight = 0.0;
  const Position *const end = var->fillers + Variable::NFILLERS;
  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    Word value = values[*p - 1];
    if(!value)
      continue;

    weight += _score_cval(sc, type, value);
  }

  return weight;
}

double
CatFeature::_score_cvar(const SuperCat *sc, Type type, const Words &values) const {
  return _score_cvar(sc, &sc->vars[sc->cat->var], type, values);
}

double
CatFeature::score(const SuperCat *sc, const Words &words, const Words &tags,
			   Type type) const {
  double score = 0.0;

  if(type == LEX){
    score += _score_cvar(sc, LEX_WORD, words);
    score += _score_cvar(sc, LEX_POS, tags);
  }else if(type == ROOT){
    score += _score_c(sc, ROOT);
    score += _score_cvar(sc, ROOT_WORD, words);
    score += _score_cvar(sc, ROOT_POS, tags);
  }else
    throw NLP::IOException("need LEX or ROOT type as argument to score function");

  return score;
}

} }
