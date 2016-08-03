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

#include "hashtable/base.h"
#include "hashtable/frame.h"

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"
#include "parser/variable.h"
#include "parser/canonical.h"

#include "share.h"

using namespace std;

namespace NLP { namespace CCG {

using namespace HashTable;

class _CanonEntry {
protected:
  const Cat *_cat;
  _CanonEntry *_next;
public:
  _CanonEntry(const Cat *cat, _CanonEntry *next):
      _cat(cat), _next(next) {}
  ~_CanonEntry(void) { /* do nothing */ }

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); }
  void operator delete(void *, Pool *) { /* do nothing */ }

  const Cat *canonical(void) const { return _cat; }

  bool equal(const Cat *cat){
    // eq() is less stringent than ==
    // eq() doesn't look at variables
    return eq(cat, _cat);
  }

  _CanonEntry *find(const Cat *cat){
    for(_CanonEntry *l = this; l != 0; l = l->_next)
      if(l->equal(cat))
        return l;
    return 0;
  }

  ulong nchained(void){
    return _next ? _next->nchained() + 1 : 1;
  }
};

typedef Frame<_CanonEntry,LARGE,MEDIUM> _ImplBase;
class Canonical::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const std::string &name): _ImplBase(name) {}
  virtual ~_Impl(void) {}

  const Cat *add(const Cat *cat){
    ulong bucket = cat->rhash % _NBUCKETS;
    Entry *entry = _buckets[bucket]->find(cat);
    if(entry)
      return entry->canonical();

    entry = new (_ent_pool) Entry(cat, _buckets[bucket]);
    _buckets[bucket] = entry;
    ++size;

    return cat;
  }

  const Cat *get(const Cat *cat) const{
    ulong bucket = cat->rhash % _NBUCKETS;
    Entry *entry = _buckets[bucket]->find(cat);
    if(entry)
      return entry->canonical();

    return 0;      
  }
  
};

Canonical::Canonical(void):
    _impl(new _Impl("canonical")){}

Canonical::Canonical(const Canonical &other):
    _impl(share(other._impl)){}

Canonical &
Canonical::operator=(Canonical &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Canonical::~Canonical(void){
  release(_impl);
}

std::size_t
Canonical::size(void) const {
  return _impl->size;
}

const Cat *
Canonical::add(const Cat *cat){
  return _impl->add(cat);
}

const Cat *
Canonical::get(const Cat *cat) const{
  return _impl->get(cat);
}

} }
