// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Model::Cache
// store the tag that a word was last tagged with
// in the current 'document' for the history feature

#include "base.h"

#include "hashtable/base.h"

#include "prob.h"
#include "model/feature.h"
#include "model/attribute.h"
#include "model/cache.h"
#include "share.h"

using namespace std;

ulong total_nonzeros = 0;

namespace NLP {

  using namespace HashTable;

  namespace Model {

// this will be a reasonably small hash table
// notice it might include words we haven't seen
// in the lexicon, so we need a string memory pool
// as well
typedef Base<Entry<PDF>, const string &, MEDIUM, MEDIUM> ImplBase;

class Cache::Impl_: public ImplBase, public Shared {
public:
  Impl_(const string &name): ImplBase(name){}
  virtual ~Impl_(void){}

  const PDF *get(const string &str) const {
    Entry *entry = ImplBase::find(str);
    if(entry)
      return &entry->value;
    return 0;
  }

  void add(const string &str, const PDF &dist){
    ImplBase::add(str)->value = dist;
  }
};

Cache::Cache(const string &name): impl_(new Impl_(name)) {}
Cache::Cache(const Cache &other): impl_(share(other.impl_)){}

Cache::~Cache(void){
  impl_->printstats(cerr);
  release(impl_);
}

const string
Cache::name(void) const { return impl_->name; }

size_t
Cache::size(void) const { return impl_->size; }

const PDF *
Cache::get(const string &str) const { return impl_->get(str); }

void
Cache::add(const string &str, const PDF &dist){
  impl_->add(str, dist);
}

void
Cache::clear(void){ impl_->clear(); }

} }
