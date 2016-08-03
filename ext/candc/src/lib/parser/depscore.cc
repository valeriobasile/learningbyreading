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
#include "parser/markedup.h"
#include "parser/depscore.h"

#include "share.h"

using namespace std;

namespace NLP { namespace CCG {

using namespace HashTable;

class _DepEntry {
protected:
  const Filled * const _filled;
  _DepEntry *_next;
  double _score;
public:
  _DepEntry(const Filled *filled, double score, _DepEntry *next):
      _filled(filled), _next(next), _score(score) {}
  ~_DepEntry(void) { /* do nothing */ }

  void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); }
  void operator delete(void *, Pool *) { /* do nothing */ }

  void add(double score) { _score += score; }
  double score(void) const { return _score; }

  const Filled *filled(void) const { return _filled; }

  _DepEntry *next(void) const { return _next; }

  bool equal(const Filled *other){ return *_filled == *other; }

  _DepEntry *find(const Filled *other){
    for(_DepEntry *l = this; l != 0; l = l->_next)
      if(l->equal(other))
        return l;
    return 0;
  }

  ulong nchained(void){
    return _next ? _next->nchained() + 1 : 1;
  }
};

Hash hash(const Filled *filled){
  Hash hash(filled->head);
  hash += filled->rel;
  hash += filled->filler;
  hash += filled->rule;
  return hash;
}

typedef Frame<_DepEntry,MEDIUM,MEDIUM> _ImplBase;
class DepScore::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const std::string &name): _ImplBase(name) {}
  virtual ~_Impl(void) {}

  void add(const Filled *filled, double score){
    ulong bucket = hash(filled) % _NBUCKETS;
    Entry *entry = _buckets[bucket]->find(filled);
    if(entry){
      entry->add(score);
      return;
    }

    entry = new (_ent_pool) Entry(filled, score, _buckets[bucket]);
    _buckets[bucket] = entry;
    ++size;
  }

  double find(const Filled *filled) const {
    ulong bucket = hash(filled) % _NBUCKETS;
    Entry *entry = _buckets[bucket]->find(filled);
    if(entry)
      return entry->score();
    else
      return 0.0;
  }

  void clear(void){
    _ent_pool->clear();
    memset(_buckets, 0, sizeof(_buckets));
    size = 0;
  }

  void dump(ostream &out, const Markedup &markedup, const Relations &rels, const vector<std::string> &words){
    for(ulong i = 0; i < _NBUCKETS; ++i){
      for(Entry *e = _buckets[i]; e; e = e->next()){
	out << std::setprecision(15) << e->score() << ' ';
	const Filled *dep = e->filled();
	ulong ruleid = dep->rule;
	if(dep->lrange){
	  out << words[dep->head - 1] << '_' << ulong(dep->head) << ' ';
	  rels[dep->rel].print_slot(out, false);
	  out << ' ' << words[dep->filler - 1] << '_' << ulong(dep->filler) << ' ' << ruleid
	      << ' ' << *markedup[dep->lrange] << '\n';
	}else{
	  out << words[dep->head - 1] << '_' << ulong(dep->head) << ' ';
	  rels[dep->rel].print_slot(out, false);
	  out << ' ' << words[dep->filler - 1] << '_' << ulong(dep->filler)
	      << ' ' << ruleid << '\n';
	}
      }
    }
  }

  void dump(ostream &out) const {
    for(ulong i = 0; i < _NBUCKETS; ++i)
      for(Entry *e = _buckets[i]; e; e = e->next())
	out << std::setprecision(15) << e->score() << ' ' << *e->filled() << endl;
  }
};

DepScore::DepScore(const std::string &name):
    _impl(new _Impl(name)){}

DepScore::DepScore(const DepScore &other):
    _impl(share(other._impl)){}

DepScore &
DepScore::operator=(DepScore &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

DepScore::~DepScore(void){
  release(_impl);
}

const std::string
DepScore::name(void) const {
  return _impl->name;
}

std::size_t
DepScore::size(void) const {
  return _impl->size;
}

void
DepScore::add(const Filled *filled, double score){
  _impl->add(filled, score);
}

double
DepScore::get(const Filled *filled) const {
  return _impl->find(filled);
}

void
DepScore::clear(void){
  _impl->clear();
}

void
DepScore::dump(ostream &out, const Markedup &markedup, const Relations &rels, const vector<std::string> &words){
  _impl->dump(out, markedup, rels, words);
}

void
DepScore::dump(ostream &out){
  _impl->dump(out);
}

} }
