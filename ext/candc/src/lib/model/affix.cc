// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Model::AffixAttributes
// hash table storage for affix attributes in the taggers
// a type field is used to disambiguate features with the
// same value

#include "base.h"

#include "config/config.h"

#include "hashtable/base.h"

#include "affix.h"

#include "model/types.h"
#include "model/feature.h"
#include "model/attribute.h"
#include "model/attributes.h"
#include "model/registry.h"

#include "share.h"

using namespace std;

namespace NLP { namespace Model {

using namespace NLP::HashTable;

// specialised affix hash function and hash table entry
class _AffixAttrEntry {
public:
  // hash function takes low bits of affix value and the attribute type
  static Hash hash(ulong type, Affix affix){
    Hash h(0ul);
    for(int i = 0; i < 4; ++i)
      h += affix[i];
    h *= 2;
    h |= type;
    return h;
  }
  // old hash function (>= avg chains len 10) new one avg 1.06
  //  { return Hash((affix.value() << 3) | type); };
protected:
  const ulong _type;		// attribute type
  const Affix _affix;		// affix value
  _AffixAttrEntry *_next;	// chain for hash table collisions
  Attribute _attribute;		// begin and end feature pointers
public:
  _AffixAttrEntry(ulong type, Affix affix, _AffixAttrEntry *next):
      _type(type), _affix(affix), _next(next) {};
  ~_AffixAttrEntry(void) {};

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); };
  void operator delete(void *, Pool *) { /* do nothing */ };

  static _AffixAttrEntry *create(Pool *, const std::string &,
				 ulong, NLP::Hash, _AffixAttrEntry *){ return 0; }

  ulong type(void) const { return _type; };
  Affix affix(void) const { return _affix; };
  Attribute &attribute(void) { return _attribute; };
  Attribute attribute(void) const { return _attribute; };

  bool equal(ulong t, Affix a){ return a == _affix && t == _type; };

  Attribute find(ulong type, Affix affix){
    for(_AffixAttrEntry *l = this; l != 0; l = l->_next)
      if(l->equal(type, affix))
        return l->attribute();

    return NONE;
  };

  ulong nchained(void){
    return _next ? _next->nchained() + 1 : 1;
  };
};

// this hash table does not need a string memory pool
typedef Base<_AffixAttrEntry, const char *, SMALL, MEDIUM> _ImplBase;

class AffixAttributes::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const string &name): _ImplBase(name){}
  virtual ~_Impl(void){}

  using _ImplBase::insert;
  Attribute &insert(ulong type, Affix value){
    ulong bucket = _AffixAttrEntry::hash(type, value) % NBUCKETS_;

    _AffixAttrEntry *entry = new (pool_) _AffixAttrEntry(type, value, buckets_[bucket]);
    buckets_[bucket] = entry;
    ++size;

    return entry->attribute();
  }

  Attribute find(ulong type, Affix value) const {
    Hash hash = _AffixAttrEntry::hash(type, value);
    return buckets_[hash % NBUCKETS_]->find(type, value);
  }
};

AffixAttributes::AffixAttributes(void)
  : Attributes("AffixAttributes"), _impl(new _Impl("AffixAttributes")) {}
AffixAttributes::AffixAttributes(AffixAttributes &other)
  : Attributes("AffixAttributes"), _impl(share(other._impl)) {}

AffixAttributes::~AffixAttributes(void){
  release(_impl);
}

size_t
AffixAttributes::size(void) const {
  return _impl->size;
}

Attribute &
AffixAttributes::load(const Type &type, std::istream &in){
  if(!type.flags){
    cerr << type.id << ": " << type.desc << endl;
    throw NLP::Exception("all loaded Affix attribute values must be added");
  }

  string value;
  in >> value;
  return _impl->insert(type.index, Affix(value));
}

Attribute
AffixAttributes::get(const Type &type, Affix value) const {
  return _impl->find(type.index, value);
}

Attribute &
AffixAttributes::insert(const Type &type, Affix value) {
  return _impl->insert(type.index, value);
}

void
AffixAttributes::print_stats(ostream &out) const {
  out << "statistics for the affix attributes hash table" << endl;
  out << "----------------------------------------------" << endl;
  _impl->printstats(out);
}

} }
