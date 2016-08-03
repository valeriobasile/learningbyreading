// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Model::Registry
// mapping from Attributes classes which store the
// efficient representation of contextual predicates
// (which we call attributes) in the tagging code
// There are different attributes classes depending on the
// type of feature being stored, e.g. since tags have
// a fixed number of values they can be stored and accessed
// much more efficiently than just throwing all of the features
// into a large hash table

#include "base.h"

#include "config/config.h"

#include "hashtable/base.h"

#include "affix.h"

#include "model/types.h"
#include "model/feature.h"
#include "model/attribute.h"
#include "model/attributes.h"
#include "model/registry.h"

#include "cluster.h"

#include "share.h"

using namespace std;

namespace NLP { namespace Model {

namespace HT = NLP::HashTable;

struct RegEntry {
  Type type;
  Attributes *attribs;
};

typedef HT::Base<HT::Entry<RegEntry>, const std::string &, HT::BABY, HT::BABY> _ImplBase;

class Registry::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const std::string &name): _ImplBase(name){}
  virtual ~_Impl(void) {}

  void reg(const Type &type, Attributes *attribs){
    Entry *e = _ImplBase::find(type.id);
    if(e){
      string id = type.id;
      throw NLP::Exception("cannot register multiple Attributes with id '" + id + "'");
    }
    e = _ImplBase::add(type.id);
    e->value.type = type;
    e->value.attribs = attribs;
  }

  Entry *find(const std::string &txt) const {
    Entry *e = _ImplBase::find(txt);
    if(!e)
      throw NLP::Exception("no Attributes object in registry matching '" + txt + "'");

    return e;
  }
};

Registry::Registry(const std::string &name): _impl(new _Impl(name)){}
Registry::Registry(const Registry &other): _impl(share(other._impl)){}

Registry &
Registry::operator=(const Registry &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Registry::~Registry(void){
  release(_impl);
}

const std::string
Registry::name(void) const {
  return _impl->name;
}

size_t
Registry::size(void) const {
  return _impl->size;
}

void
Registry::reg(const Type &type, Attributes &attribs){
  _impl->reg(type, &attribs);
}

void
Registry::reg(const Type &type){
  _impl->reg(type, 0);
}

Attributes &
Registry::get(const std::string &txt){
  return *_impl->find(txt)->value.attribs;
}

const char *
Registry::canonize(const std::string &txt) const{
  return _impl->find(txt)->value.type.id;
}

Attribute &
Registry::load(const std::string &txt, std::istream &in){
  HT::Entry<RegEntry> *e = _impl->find(txt);
  return e->value.attribs->load(e->value.type, in);
}

} }
