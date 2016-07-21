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

// NLP::HashTable::Base
// simple hash table template class
// uses linked lists to overcome hash collisions
// supports hash lookup with an entries and strings memory pool

#include "pool.h"
#include "hashtable/size.h"
#include "hashtable/entry.h"

namespace NLP {
  namespace HashTable {

    template <class E, class K, ulong NBUCKETS, ulong SPOOL>
    class Base {
    public:
      typedef E Entry;
      typedef K Key;

      const std::string name;
      size_t size;
    protected:
      Pool *const pool_;
      const bool shared_;

      const static ulong NBUCKETS_ = NBUCKETS;
      const static ulong SPOOL_ = SPOOL;

      Entry *buckets_[NBUCKETS];
    public:
      Base(const std::string &name, Pool *pool = 0)
	: name(name), size(0),
          pool_(pool ? pool : new Pool(SPOOL)),
          shared_(pool != 0){
        memset(buckets_, 0, sizeof(buckets_));
      }

      virtual ~Base(void){
        if(!shared_)
          delete pool_;
      }

      Pool *pool(void) { return pool_; }

      virtual Entry *insert(Key key, const Hash hash, const ulong bucket){
	Entry *entry = Entry::create(pool_, key, size, hash, buckets_[bucket]);
        buckets_[bucket] = entry;
        ++size;
        return entry;
      }

      virtual void clear(void){
	if(size){
	  size = 0;
	  memset(buckets_, 0, sizeof(buckets_));
	  pool_->clear();
	}
      }

      Entry *insert(Key key){
	const Hash hash(key);
	return insert(key, hash, hash % NBUCKETS);
      }

      Entry *add(Key key){
	const Hash hash(key);
        ulong bucket = hash % NBUCKETS;
        Entry *entry = buckets_[bucket]->find(key, hash);
        if(entry)
          return entry;

        return insert(key, hash, bucket);
      }

      Entry *find(Key key) const {
	const Hash hash(key);
        return buckets_[hash % NBUCKETS]->find(key, hash);
      }
      Entry *find(const char c) const { return buckets_[NLP::Hash(c) % NBUCKETS]->find(c); }

      void printstats(std::ostream &os) const {
        size_t maxchain = 0;
        size_t nbins = 0;
        size_t nbytes = 0;

        for(ulong i = 0; i < NBUCKETS; i++){
          if(buckets_[i]){
            ulong temp = buckets_[i]->nchained();
            if(maxchain < temp)
              maxchain = temp;
            nbins++;
          }
        }

        os << "number of entries " << size << '\n';
        os << "number of bins used " << nbins << " (of " << NBUCKETS << ")\n";
        os << "used bins/nbins " << nbins/static_cast<float>(NBUCKETS) << '\n';
        os << "maximum chain length " << maxchain << '\n';
        os << "average chain length " << size/static_cast<float>(nbins) << '\n';

        nbytes = size * sizeof(Entry);
        os << "      entry objs " << nbytes << " bytes\n";
        nbytes += sizeof(buckets_);
        os << "      bin []     " << sizeof(buckets_) << " bytes\n";
        os << "total            " << nbytes << " bytes\n";
      }
    };

  }
}
