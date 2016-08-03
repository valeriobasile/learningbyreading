// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Model::TrigramAttributes
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

class _TrigramAttribEntry {
public:
  // treat the Word pointer as an integer and add the type
  // since the Word pointer is a canonical value
  static Hash hash(ulong type, Word word1, Word word2, Word word3){ 
	Hash h(word3 >> 2); 
        h += (word2 >> 2);
        h += (word1 >> 2)*7 + type;
  	return h;
  }
protected:
  const ulong _type;		// feature type
  const Word _word1;		// feature value (a canonical string)
  const Word _word2;
  const Word _word3;
  _TrigramAttribEntry *_next;	// chain for hash table collisions
  Attribute _attribute;		// start and end feature pointers
public:
  _TrigramAttribEntry(ulong type, Word word1, Word word2, Word word3, _TrigramAttribEntry *next):
      _type(type), _word1(word1), _word2(word2), _word3(word3), _next(next) {};
  ~_TrigramAttribEntry(void) {};

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); };
  void operator delete(void *, Pool *) { /* do nothing */ };

  static _TrigramAttribEntry *create(Pool *, const std::string &,
				 ulong, NLP::Hash, _TrigramAttribEntry *){ return 0; }

  ulong type(void) const { return _type; };
  Word word1(void) const { return _word1; };
  Word word2(void) const { return _word2; };
  Attribute &attribute(void) { return _attribute; };
  Attribute attribute(void) const { return _attribute; };

  // value is a canonical string so string comparisons aren't necessary
  bool equal(ulong type, Word word1, Word word2, Word word3){ return word1 == _word1 && word2 == _word2 && word3 == _word3 && type == _type; };

  Attribute find(ulong type, Word word1, Word word2, Word word3){
    for(_TrigramAttribEntry *l = this; l; l = l->_next)
      if(l->equal(type, word1, word2, word3))
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
typedef Base<_TrigramAttribEntry, const char *, LARGE, LARGE> _ImplBase;
class TrigramAttributes::_Impl: public _ImplBase, public Shared {
public:
  Lexicon _lexicon;

  _Impl(const string &name, Lexicon lexicon)
    : _ImplBase(name), _lexicon(lexicon) {}
  virtual ~_Impl(void){}

  using _ImplBase::insert;
  Attribute &insert(ulong type, Word word1, Word word2, Word word3){
    ulong bucket = _TrigramAttribEntry::hash(type, word1, word2, word3) % NBUCKETS_;

    _TrigramAttribEntry *entry = new (pool_) _TrigramAttribEntry(type, word1, word2, word3, buckets_[bucket]);
    buckets_[bucket] = entry;
    ++size;

    return entry->attribute();
  }

  Attribute find(ulong type, Word word1, Word word2, Word word3) const {
    Hash hash = _TrigramAttribEntry::hash(type, word1, word2, word3);
    return buckets_[hash % NBUCKETS_]->find(type, word1, word2, word3);
  }
};

TrigramAttributes::TrigramAttributes(Lexicon lexicon)
  : Attributes("TrigramAttributes"), _impl(new _Impl("TrigramAttributes", lexicon)) {}

TrigramAttributes::TrigramAttributes(TrigramAttributes &other)
  : Attributes("TrigramAttributes"), _impl(share(other._impl)) {}

TrigramAttributes::~TrigramAttributes(void){
  release(_impl);
}

size_t
TrigramAttributes::size(void) const {
  return _impl->size;
}

Attribute &
TrigramAttributes::load(const Type &type, std::istream &in){
  string word1;
  in >> word1;
  string word2;
  in >> word2;
  string word3;
  in >> word3;

  if(type.flags)
    return _impl->insert(type.index, _impl->_lexicon.attribute_add(word1), _impl->_lexicon.attribute_add(word2), _impl->_lexicon.attribute_add(word3));
  else
    return _impl->insert(type.index, _impl->_lexicon.attribute_check(word1), _impl->_lexicon.attribute_check(word2), _impl->_lexicon.attribute_check(word3));
}

Attribute
TrigramAttributes::get(const Type &type, Word word1, Word word2, Word word3) const {
  return _impl->find(type.index, word1, word2, word3);
}

Attribute &
TrigramAttributes::insert(const Type &type, Word word1, Word word2, Word word3) {
  return _impl->insert(type.index, word1, word2, word3);
}

void
TrigramAttributes::print_stats(ostream &out) const {
  out << "statistics for the bigram attributes hash table" << endl;
  out << "------------------------------------------------" << endl;
  _impl->printstats(out);
}

} }
