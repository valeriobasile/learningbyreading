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
#include "parser/feature_genrule.h"

using namespace std;

namespace NLP { namespace CCG {

void
GenruleFeature::load(istream &in, const std::string &filename, ulong id, Type){
  std::string catstr;
  ushort stmp;
  if(!(in >> catstr >> stmp))
    throw NLP::IOException("error parsing genrule feature in load", filename, id);

  try {
    const Cat *cat = cats.canonize(catstr.c_str());
    
    genrule_attrs.insert(id, cat, stmp);  

  }catch(NLP::Exception e){
    throw NLP::IOException("error parsing category in genrule feature ", filename, id);
  }
}

ulong
GenruleFeature::get_id(istream &in, const std::string &filename, Type,
		       vector<long> &) const{
  string catstr;
  ushort stmp;
  if(!(in >> catstr >> stmp))
    throw NLP::IOException("error parsing genrule feature in load_feature", filename);

  const Cat *cat = cats.canonize(catstr.c_str());

  return genrule_attrs(cat, stmp);
}

void
GenruleFeature::add(const SuperCat *sc, const Words &, const Words &,
		    Type, std::vector<ulong> &ids) const {
  ulong id = genrule_attrs(sc->cat, SuperCat::GEN_RULES & sc->flags);

  if(id)
    ids.push_back(id - 1);
}

double
GenruleFeature::score(const SuperCat *sc, const Words &, const Words &,
		      Type) const {
  return genrule_attrs.weight(sc->cat, SuperCat::GEN_RULES & sc->flags);
}

} }
