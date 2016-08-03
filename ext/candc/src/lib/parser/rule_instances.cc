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

#include "share.h"

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"
#include "parser/gr_constraints.h"
#include "parser/gr.h"
#include "parser/relation.h"
#include "parser/variable.h"
#include "parser/dependency.h"
#include "parser/distance.h"
#include "parser/filled.h"
#include "parser/relations.h"
#include "parser/supercat.h"
#include "parser/unify.h"

#include "parser/rule_instances.h"

using namespace std;

namespace NLP { namespace CCG {

using namespace NLP::HashTable;
using namespace NLP::CCG;

class _RuleInstEntry {
public:
  static Hash hash(const Cat *cat1, const Cat *cat2){
    return Hash((cat1->rhash.value() ^ cat1->rhash.value()*37) + cat2->rhash.value());
  }

  const Hash _h1;
  const Hash _h2;

  _RuleInstEntry *_next;
public:
  _RuleInstEntry(const Cat *cat1, const Cat *cat2, _RuleInstEntry *next):
    _h1(cat1->rhash), _h2(cat2->rhash), _next(next) {}
  ~_RuleInstEntry(void) {}

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); }
  void operator delete(void *, Pool *) { /* do nothing */ }

  bool equal(const Cat *cat1, const Cat *cat2){
    return cat1->rhash == _h1 && cat2->rhash == _h2;
  }

  bool find(const Cat *cat1, const Cat *cat2){
    for(_RuleInstEntry *l = this; l; l = l->_next)
      if(l->equal(cat1, cat2))
        return true;

    return false;
  }

  ulong nchained(void){
    return _next ? _next->nchained() + 1 : 1;
  }
};

typedef Frame<_RuleInstEntry, MEDIUM, LARGE> _ImplBase;
class RuleInstances::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const string &name): _ImplBase(name){}
  virtual ~_Impl(void) {}

  void insert(const Cat *cat1, const Cat *cat2){
    ulong bucket = Entry::hash(cat1, cat2) % _NBUCKETS;
    _buckets[bucket] = new (_ent_pool) Entry(cat1, cat2, _buckets[bucket]);
    ++size;
  }

  bool find(const Cat *cat1, const Cat *cat2) const {
    ulong bucket = Entry::hash(cat1, cat2) % _NBUCKETS;
    return _buckets[bucket]->find(cat1, cat2);
  }
};

RuleInstances::RuleInstances(void): _impl(new _Impl("")) {}
RuleInstances::RuleInstances(RuleInstances &other): _impl(share(other._impl)) {}

RuleInstances::~RuleInstances(void){
  release(_impl);
}

size_t
RuleInstances::size(void) const {
  return _impl->size;
}

bool
RuleInstances::get(const Cat *cat1, const Cat *cat2) const {
  if(_impl->find(cat1, cat2)){
    //    cat1->out_novar_noX(cerr, false) << ' ';
    //    cat2->out_novar_noX(cerr, false) << endl;
    return true;
  }
  return false;
}

void
RuleInstances::insert(const Cat *cat1, const Cat *cat2) {
  _impl->insert(cat1, cat2);
}

} }
