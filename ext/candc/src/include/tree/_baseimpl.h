/* -*- Mode: C++; -*- */
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

#include "tree/attributes.h"

namespace NLP {

  using namespace HashTable;

  namespace Tree {

    template <class Value>
    class AttribEntry {
    public:
      const Hash hash;
      ulong id;
      double weight;
      Value value;
      AttribEntry<Value> *next;

      AttribEntry(const Value &value, ulong id, Hash hash, AttribEntry<Value> *next):
	hash(hash), id(id), weight(0.0), value(value), next(next){}
      ~AttribEntry(void){ /* do nothing */ }

      void *operator new(size_t size, Pool *pool){
	return (void *)pool->alloc(size);
      }
      void operator delete(void *, Pool *){ /* do nothing */ }
 
      static AttribEntry<Value> *create(Pool *pool, const Value &value, ulong index,
					NLP::Hash hash, AttribEntry<Value> *next){
	AttribEntry *entry = new (pool) AttribEntry<Value>(value, index, hash, next);
	return entry;
      }

      bool equal(const Value &v, const NLP::Hash h){
	return hash == h && value.equal(v);
      }

      AttribEntry<Value> *find(const Value &v, const NLP::Hash h){
	for(AttribEntry<Value> *l = this; l; l = l->next)
	  if(l->equal(v, h))
	    return l;

	return 0;
      }

      ulong find_id(const Value &v, const NLP::Hash h){
	for(AttribEntry<Value> *l = this; l; l = l->next)
	  if(l->equal(v, h))
	    return l->id;

	return 0;
      }

      double find_weight(const Value &v, const NLP::Hash h){
	for(AttribEntry<Value> *l = this; l; l = l->next)
	  if(l->equal(v, h))
	    return l->weight;

	return 0.0;
      }

      void set_weights(const double *weights){
	weight = weights[id - 1];
	if(next)
	  next->set_weights(weights);
      }

      ulong nchained(void){
	return next ? next->nchained() + 1 : 1;
      }
    };

    // this should really inherit from Base but there is a bug in gcc 4.0
    // which stops this from compiling if you do (it works in gcc 3.2.2)
    template <class Value, ulong NBUCKETS, ulong SPOOL>
    class AttributesImpl {
    public:
      typedef AttribEntry<Value> Entry;
      typedef std::vector<Entry *> Entries;

      const std::string name;
      size_t size;
    protected:
      Pool *const pool_;
      const bool shared_;

      static const ulong NBUCKETS_ = NBUCKETS;
      static const ulong PENTRIES_ = SPOOL;

      Entry *buckets_[NBUCKETS];
    public:
      AttributesImpl(const std::string &name, Pool *pool = 0)
	: name(name), size(0),
          pool_(pool ? pool : new Pool(SPOOL)),
          shared_(pool != 0){
        memset(buckets_, 0, sizeof(buckets_));
      }

      virtual ~AttributesImpl(void){
        if(!shared_)
          delete pool_;
      }

      Pool *pool(void) { return pool_; }

      Entry *insert(const Value &value, ulong id, const Hash hash, const ulong bucket){
	Entry *entry = Entry::create(pool_, value, id, hash, buckets_[bucket]);
	buckets_[bucket] = entry; 
        ++size;
	return entry;
      }

      void insert(const Value &value, ulong id){
	const Hash hash = value.hash();
	insert(value, id, hash, hash % NBUCKETS_);
      }

      Entry *add(const Value &value){
	const Hash hash = value.hash();
        ulong bucket = hash % NBUCKETS;
        Entry *entry = buckets_[bucket]->find(value, hash);
        if(entry)
          return entry;

        return insert(value, 0, hash, bucket);
      }

      ulong find_id(const Value &value) const {
	Hash hash = value.hash();
	return buckets_[hash % NBUCKETS_]->find_id(value, hash);
      }

      double find_weight(const Value &value) const {
	Hash hash = value.hash();
	return buckets_[hash % NBUCKETS_]->find_weight(value, hash);
      }

      void set_weights(const double *weights){
	for(ulong i = 0; i < NBUCKETS_; ++i)
	  if(buckets_[i])
	    buckets_[i]->set_weights(weights);
      }
    };

  }
}
