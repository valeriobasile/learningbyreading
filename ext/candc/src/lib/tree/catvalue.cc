// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "tree/_baseimpl.h"

using namespace std;

namespace NLP { namespace Tree {

using namespace NLP::HashTable;
using namespace NLP::CCG;

struct CatValue {
  const Type type;
  const Cat *cat;
  const Word value;

  CatValue(const Type type, const Cat *cat, const Word value)
    : type(type), cat(cat), value(value){}

  Hash hash(void) const {
    Hash h(static_cast<ulong>(value));
    h += cat->rhash;
    h += type;
    return h;
  }
  
  bool equal(const CatValue &other) const {
    if(other.value == value && other.type == type){
      if(other.cat == cat)
	return true;

      // JRC: should this be full equality here?
      // all other feature types used relaxed equality (equ)
      // eg. should this be taking variables into account?
      return *other.cat == *cat;
    }else
      return false;
  }
};

class CatValueAttributes::Impl: public AttributesImpl<CatValue, MEDIUM, LARGE>, public Shared {
public:
  Impl(void): AttributesImpl<CatValue, MEDIUM, LARGE>("CatValueAttributes"){}
};

CatValueAttributes::CatValueAttributes(void): _impl(new Impl) {}
CatValueAttributes::CatValueAttributes(CatValueAttributes &other): _impl(share(other._impl)) {}
CatValueAttributes::~CatValueAttributes(void){ release(_impl); }

size_t CatValueAttributes::size(void) const { return _impl->size; }
void CatValueAttributes::set_weights(const double *weights){
  _impl->set_weights(weights);
}

ulong
CatValueAttributes::get_id(Type type, const Cat *cat, Word value) const {
  CatValue catvalue(type, cat, value);
  return _impl->find_id(catvalue);
}

double
CatValueAttributes::weight(Type type, const Cat *cat, Word value) const {
  CatValue catvalue(type, cat, value);
  return _impl->find_weight(catvalue);
}

void
CatValueAttributes::insert(ulong id, Type type, const Cat *cat, Word value) {
  CatValue catvalue(type, cat, value);
  _impl->insert(catvalue, id);
}

} }
