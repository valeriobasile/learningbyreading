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
#include "hashtable/frame.h"

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"
#include "parser/variable.h"
#include "parser/gr_constraints.h"
#include "parser/gr.h"
#include "parser/relation.h"
#include "parser/dependency.h"
#include "parser/filled.h"
#include "parser/relations.h"
#include "parser/supercat.h"
#include "parser/equiv.h"

#include "share.h"

using namespace std;

namespace NLP { namespace CCG {

using namespace HashTable;

ulong equiv_add = 0;
ulong equiv_collisions = 0;

class _EquivEntry {
protected:
  const Position _pos;
  const Position _span;
  const Hash _ehash;
  const SuperCat *_begin;
  SuperCat *_end;
  _EquivEntry *_next;
public:
  _EquivEntry(Position pos, Position span, Hash ehash, SuperCat *begin, _EquivEntry *next):
      _pos(pos), _span(span), _ehash(ehash), _begin(begin), _end(begin), _next(next) {}
  ~_EquivEntry(void) { /* do nothing */ }

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); }
  void operator delete(void *, Pool *) { /* do nothing */ }

  const SuperCat *canonical(void) const { return _begin; }
  void add(SuperCat *sc) {
    _end->next = sc;
    _end = sc;
    sc->next = 0;
  }

  bool equal(Position pos, Position span, Hash ehash, SuperCat *sc){
    return ehash == _ehash && pos == _pos && span == _span && equivalent(sc, _begin);
  }

  _EquivEntry *find(Position pos, Position span, Hash ehash, SuperCat *sc){
    for(_EquivEntry *l = this; l != 0; l = l->_next)
      if(l->equal(pos, span, ehash, sc))
        return l;
      else
	++equiv_collisions;
    return 0;
  }

  ulong nchained(void){
    return _next ? _next->nchained() + 1 : 1;
  }
};

typedef Frame<_EquivEntry,SMALL/2,MEDIUM> _ImplBase;
class Equivalence::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const std::string &name): _ImplBase(name) {}
  virtual ~_Impl(void) {}

  bool add(Position pos, Position span, SuperCat *sc){
    Hash hash = sc->ehash();
    hash += pos;
    hash += span;

    ulong bucket = hash % _NBUCKETS;
    Entry *entry = _buckets[bucket]->find(pos, span, hash, sc);
    if(entry){
      entry->add(sc);
      return false;
    }

    entry = new (_ent_pool) Entry(pos, span, hash, sc, _buckets[bucket]);
    _buckets[bucket] = entry;
    ++size;

    return entry;
  }

  void clear(void){
    _ent_pool->clear();
    memset(_buckets, 0, sizeof(_buckets));
    size = 0;
  }
};

Equivalence::Equivalence(const std::string &name):
    _impl(new _Impl(name)){}

Equivalence::Equivalence(const Equivalence &other):
    _impl(share(other._impl)){}

Equivalence &
Equivalence::operator=(Equivalence &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Equivalence::~Equivalence(void){
  release(_impl);
}

const std::string
Equivalence::name(void) const {
  return _impl->name;
}

std::size_t
Equivalence::size(void) const {
  return _impl->size;
}

bool
Equivalence::add(Position pos, Position span, SuperCat *sc){
  ++equiv_add;
  return _impl->add(pos, span, sc);
}

void
Equivalence::clear(void){
  _impl->clear();
}

} }
