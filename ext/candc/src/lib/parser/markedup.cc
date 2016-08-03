// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "utils.h"

#include "hashtable/base.h"
#include "hashtable/ordered.h"

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"
#include "parser/markedup.h"

#include "share.h"

using namespace std;

namespace NLP { namespace CCG {

using namespace HashTable;

struct CatMarkedup {
  const Cat *cat;
  std::string markedup;
};

typedef Ordered<Entry<CatMarkedup>, const string &, TINY, TINY> _ImplBase;

class Markedup::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const std::string &name): _ImplBase(name) {}
  virtual ~_Impl(void) {}

  void add(const std::string &plain_str, const std::string &markedup_str, const Cat *cat){
    CatMarkedup cm = { cat, markedup_str };
    _ImplBase::add(plain_str)->value = cm;
    _ImplBase::add(markedup_str)->value = cm;
  }
};

Markedup::Markedup(const std::string &name):
    _impl(new _Impl(name)){}

Markedup::Markedup(const Markedup &other):
    _impl(share(other._impl)){}

Markedup &
Markedup::operator=(Markedup &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Markedup::~Markedup(void){
  release(_impl);
}

const std::string
Markedup::name(void) const {
  return _impl->name;
}

std::size_t
Markedup::size(void) const {
  return _impl->entries.size();
}

const Cat *
Markedup::cat(const std::string &str) const {
  Entry<CatMarkedup> *entry = _impl->find(str);

  if(!entry)
    return 0;

  return entry->value.cat;
}

const std::string &
Markedup::markedup(const std::string &str) const {
  Entry<CatMarkedup> *entry = _impl->find(str);

  if(!entry)
    throw NLP::Exception("category " + str + " is missing markedup string");

  return entry->value.markedup;
}

const Cat *
Markedup::cat(CatID id) const {
  return _impl->entries[id]->value.cat;
}

void
Markedup::add(const std::string &plain_str,
	      const std::string &markedup_str, const Cat *cat){
  _impl->add(plain_str, markedup_str, cat);
}

} }
