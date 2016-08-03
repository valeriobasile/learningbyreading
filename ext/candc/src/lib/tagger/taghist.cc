// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Tagger::TagHist
// store the tag that a word was last tagged with
// in the current 'document' for the history feature

#include "base.h"

#include "hashtable/base.h"

#include "tagger/taghist.h"
#include "share.h"

using namespace std;

namespace NLP {

  using namespace HashTable;

  namespace Taggers {

// this will be a reasonably small hash table
// notice it might include words we haven't seen
// in the lexicon, so we need a string memory pool
// as well
typedef Base<Entry<ushort>, const string &, MEDIUM, MEDIUM> _ImplBase;

class TagHist::_Impl: public _ImplBase, public Shared {
public:
  _Impl(const string &name): _ImplBase(name){}
  virtual ~_Impl(void){}

  Tag get(const string &str) const {
    Entry *entry = _ImplBase::find(str);
    if(entry)
      return entry->value;

    return NONE;
  }

  void add(const string &str, Tag tag){
    _ImplBase::add(str)->value = tag.value();
  }
};

TagHist::TagHist(const string &name): _impl(new _Impl(name)) {}
TagHist::TagHist(const TagHist &other): _impl(share(other._impl)){}

TagHist::~TagHist(void){
  release(_impl);
}

const string
TagHist::name(void) const { return _impl->name; }

size_t
TagHist::size(void) const { return _impl->size; }

Tag
TagHist::get(const string &str) const { return _impl->get(str); }

void
TagHist::add(const string &str, const Tag tag){
  _impl->add(str, tag);
}

void
TagHist::clear(void){ _impl->clear(); }

} }
