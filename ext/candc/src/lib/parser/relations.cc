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
#include "parser/gr_constraints.h"
#include "parser/gr.h"
#include "parser/relation.h"
#include "parser/variable.h"
#include "parser/dependency.h"
#include "parser/filled.h"
#include "parser/relations.h"

#include "parser/markedup.h"
#include "parser/canonical.h"
#include "parser/categories.h"

#include "share.h"

using namespace std;

namespace NLP { namespace CCG {

using namespace HashTable;

RelID Relations::conj1 = 0;

class RelationEntry {
public:
  Relation rel;
  const Hash hash;
  RelID index;
  RelationEntry *next;
  char str[MIN_STR_BUFFER];
public:
  RelationEntry(ulong slot, ulong jslot, ulong index,
		NLP::Hash hash, RelationEntry *next)
    : rel(reinterpret_cast<const char *>(&str), slot, jslot),
      hash(hash), index(index), next(next){}
  ~RelationEntry(void) { /* do nothing */ }

  void *operator new(size_t size, Pool *pool, size_t len){
    return (void *)pool->alloc(size + aligned_size(len, sizeof(RelationEntry)));
  }
  void operator delete(void *, Pool *, size_t) { /* do nothing */ }

  static RelationEntry *create(Pool *, const char *,
			       ulong, NLP::Hash, RelationEntry *){ return 0; }
  static RelationEntry *create(Pool *pool, const std::string &cat, ulong slot, ulong jslot,
			       ulong index, NLP::Hash hash, RelationEntry *next){
    RelationEntry *entry = new (pool, get_remainder(cat.size())) RelationEntry(slot, jslot, index, hash, next);
    strcpy(entry->str, cat.c_str());
    return entry;
  }

  bool equal(const std::string &cat, ulong slot, const Hash h){
    return hash == h && slot == rel.slot && strequal(cat.c_str(), rel.cat_str);
  }
  
  RelationEntry *find(const std::string &cat, ulong slot, const Hash h){
    for(RelationEntry *l = this; l != 0; l = l->next)
      if(l->equal(cat.c_str(), slot, h))
        return l;
    return 0;
  }

  void set_constraints(const Categories &cats){
    if(rel.gr)
      rel.gr->set_cat(cats);
  }

  void set_cats(const Categories &cats){
    rel.cat = cats.markedup[rel.cat_str];
  }

  ulong nchained(void){
    return next ? next->nchained() + 1 : 1;
  }
};

typedef Ordered<RelationEntry, const char *, TINY,TINY> _ImplBase;

class Relations::_Impl: public _ImplBase, public Shared {
public:
  std::ostringstream msg;

  _Impl(const std::string &name): _ImplBase(name) {};
  ~_Impl(void) {};

  void die(std::ostream &out){
    std::ostringstream &sout = dynamic_cast<std::ostringstream &>(out);
    std::string msg = sout.str();
    throw NLP::Exception(msg);
  }

  RelID get(const std::string &cat, ulong slot) const {
    Hash hash(cat);
    hash += slot;

    ulong bucket = hash % NBUCKETS_;
    Entry *entry = buckets_[bucket]->find(cat, slot, hash);
    if(entry)
      return entry->index;

    char slot_char = '0' + static_cast<char>(slot);
    throw NLP::Exception("missing relation " + cat + ' ' + slot_char);
  }

  RelID add(const std::string &cat, ulong slot, ulong jslot){
    Hash hash(cat);
    hash += slot;

    ulong bucket = hash % NBUCKETS_;
    Entry *entry = buckets_[bucket]->find(cat, slot, hash);
    if(entry)
      return entry->index;

    entry = Entry::create(pool_, cat, slot, jslot, size, hash, buckets_[bucket]);
    buckets_[bucket] = entry;
    entries.push_back(entry);
    return size++;
  }

  void add_gr(const Categories &cats, const std::string &cat, ulong slot,
	      const std::string &fmt){
    Hash hash(cat);
    hash += slot;

    ulong bucket = hash % NBUCKETS_;
    Entry *entry = buckets_[bucket]->find(cat, slot, hash);
    if(!entry)
      die(msg << "cannot label missing slot with GR (category '"
	  << cat << "', slot " << slot << ", gr '" << fmt << "')");

    GRTemplate *gr = new GRTemplate(cats, cat, slot, fmt);
    if(!entry->rel.gr)
      entry->rel.gr = gr;
    else{
      GRTemplate *i = entry->rel.gr;
      for( ; i->next; i = i->next)
	;
      i->next = gr;
    }
  }

  void set_constraints(const Categories &cats){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if(*i)
	(*i)->set_constraints(cats);
  }

  void set_cats(const Categories &cats){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if(*i)
	(*i)->set_cats(cats);
  }
};

Relations::Relations(const std::string &name):  _impl(new _Impl(name)){
  _impl->add("", 0, 0);
}

Relations::Relations(const Relations &other): _impl(share(other._impl)){}

Relations &
Relations::operator=(const Relations &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Relations::~Relations(void){
  release(_impl);
}

const std::string
Relations::name(void) const {
  return _impl->name;
}

std::size_t
Relations::size(void) const {
  return _impl->entries.size();
}

void
Relations::init_conj(const Categories &cats){
  conj1 = add("conj", 1, 1);
  add_gr(cats, "conj", 1, "conj %l %f");
}

const Relation &
Relations::rel(const RelID id) const {
  return _impl->entries[id]->rel;
}

const Relation &
Relations::rel_checked(const RelID id) const {
  if(id >= size())
    _impl->die(_impl->msg << "dependency id " << id << " is out of range");

  return _impl->entries[id]->rel;
}

void
Relations::add_gr(const Categories &cats, const std::string &markedup, ulong slot,
		  const std::string &gr){
  _impl->add_gr(cats, markedup, slot, gr);
}

void
Relations::set_constraints(const Categories &cats){
  _impl->set_constraints(cats);
}

void
Relations::set_cats(const Categories &cats){
  _impl->set_cats(cats);
}

RelID
Relations::add(const std::string &str, ulong slot, ulong jslot) const {
  return _impl->add(str, slot, jslot);
}

RelID
Relations::get(const std::string &str, ulong slot) const {
  return _impl->get(str, slot);
}

} }
