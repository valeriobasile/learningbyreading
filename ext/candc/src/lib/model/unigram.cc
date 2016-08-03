// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Model::UniAttributes
// hash table for storing unigram (i.e. word features)
// however, it is also used to store more general string
// features such as the word types as well
// a type field is used to disambiguate entries with the
// same value (e.g. word type 'a' is the same as word 'a'
// so different values for type are used)

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

class _UniAttribEntry {
public:
  // treat the Word pointer as an integer and add the type
  // since the Word pointer is a canonical value
  static Hash hash(ulong type, Word value){ return Hash((value >> 2)*7 + type); };
protected:
  const ulong _type;		// feature type
  const Word _value;		// feature value (a canonical string)
  _UniAttribEntry *_next;	// chain for hash table collisions
  Attribute _attribute;		// start and end feature pointers
public:
  _UniAttribEntry(ulong type, Word value, _UniAttribEntry *next):
      _type(type), _value(value), _next(next) {};
  ~_UniAttribEntry(void) {};

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); };
  void operator delete(void *, Pool *) { /* do nothing */ };

  static _UniAttribEntry *create(Pool *, const std::string &,
				 ulong, NLP::Hash, _UniAttribEntry *){ return 0; }

  ulong type(void) const { return _type; };
  Word value(void) const { return _value; };
  Attribute &attribute(void) { return _attribute; };
  Attribute attribute(void) const { return _attribute; };

  // value is a canonical string so string comparisons aren't necessary
  bool equal(ulong type, Word value){ return value == _value && type == _type; };

  Attribute find(ulong type, Word value){
    for(_UniAttribEntry *l = this; l; l = l->_next)
      if(l->equal(type, value))
        return l->attribute();

    return NONE;
  };

  ulong nchained(void){
    return _next ? _next->nchained() + 1 : 1;
  };
};

// a string memory pool is not necessary because the unigram strings
// have already been converted to canonical strings by being filtered
// through the tagger lexicon

// this also applies to other string feature types such as wordtypes
// although this is a little messy.
typedef Base<_UniAttribEntry, const char *, MEDIUM, LARGE> _ImplBase;
class UniAttributes::_Impl: public _ImplBase, public Shared {
public:
  Lexicon _lexicon;

  _Impl(const string &name, Lexicon lexicon)
    : _ImplBase(name), _lexicon(lexicon) {}
  virtual ~_Impl(void){}

  using _ImplBase::insert;
  Attribute &insert(ulong type, Word value){
    ulong bucket = _UniAttribEntry::hash(type, value) % NBUCKETS_;

    _UniAttribEntry *entry = new (pool_) _UniAttribEntry(type, value, buckets_[bucket]);
    buckets_[bucket] = entry;
    ++size;

    return entry->attribute();
  }

  Attribute find(ulong type, Word value) const {
    Hash hash = _UniAttribEntry::hash(type, value);
    return buckets_[hash % NBUCKETS_]->find(type, value);
  }
};

UniAttributes::UniAttributes(Lexicon lexicon)
  : Attributes("UniAttributes"), _impl(new _Impl("UniAttributes", lexicon)) {}

UniAttributes::UniAttributes(UniAttributes &other)
  : Attributes("UniAttributes"), _impl(share(other._impl)) {}

UniAttributes::~UniAttributes(void){
  release(_impl);
}

size_t
UniAttributes::size(void) const {
  return _impl->size;
}

Attribute &
UniAttributes::load(const Type &type, std::istream &in){
  string value;
  in >> value;

  if(type.flags)
    return _impl->insert(type.index, _impl->_lexicon.attribute_add(value));
  else
    return _impl->insert(type.index, _impl->_lexicon.attribute_check(value));
}

Attribute
UniAttributes::get(const Type &type, Word value) const {
  return _impl->find(type.index, value);
}

Attribute &
UniAttributes::insert(const Type &type, Word value) {
  return _impl->insert(type.index, value);
}

void
UniAttributes::print_stats(ostream &out) const {
  out << "statistics for the unigram attributes hash table" << endl;
  out << "------------------------------------------------" << endl;
  _impl->printstats(out);
}

} }
