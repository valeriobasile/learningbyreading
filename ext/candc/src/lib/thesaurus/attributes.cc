// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <string>
#include <vector>

#include <iostream>
#include <iomanip>
#include <stdexcept>

#include <algorithm>

using namespace std;

#include "except.h"
#include "utils.h"
#include "utils/io.h"

#include "hash.h"
#include "pool.h"
#include "hashtable/entry.h"
#include "hashtable/size.h"
#include "hashtable/count.h"

#include "thesaurus/options.h"
#include "thesaurus/type.h"
#include "thesaurus/types.h"
#include "thesaurus/attribute.h"
#include "thesaurus/attributes.h"

#include "share.h"

namespace NLP { namespace Thesaurus {

using namespace HashTable;

typedef HashTable::Count<Attribute,LARGE,LARGE,LARGE> _ImplBase;

class Attributes::_Impl: public _ImplBase, public Shared {
protected:
  Types _types;
public:
  _Impl(Types types, Pool *pool = 0, Pool *lpool = 0): _ImplBase("attributes", pool, lpool), _types(types) {};
  ~_Impl(void) { /* do nothing */ };

  Attribute *add(const char *word){
    Hash hash(word);
    ulong bucket = hash % _NBUCKETS;
    Entry *entry = _buckets[bucket]->find(word, hash);
    if(entry)
      return entry;

    Type *type = _types.extract(word);

    if(_PSTRINGS)
      word = _str_pool->strdup(word);

    entry = new (_ent_pool) Entry(word, type, size, hash, _buckets[bucket]);
    _buckets[bucket] = entry;
    entries.push_back(entry);
    ++size;

    return entry;
  }

  Attribute *add(const char *word, float freq){
    Attribute *attr = add(word);
    attr->inc(freq);
    return attr;
  }

  // serializing interface
  void dump_1(std::ostream &out) const {
    dump_ulong(out, size);
    for(ulong i = 0; i < size; i++)
      dump_string(out, entries[i]->str());
  };

  void load_1(std::istream &in, const std::string &fname){
    ulong nattribs;
    if(!load_ulong(in, nattribs))
      throw NLP::IOException("missing number of attributes", fname, in.tellg());

    char buffer[1024];
    for(ulong i = 0; i < nattribs; i++){
      if(!load_string(in, buffer, sizeof(buffer)))
        throw NLP::IOException("could not load attribute string", fname, in.tellg());
      add(buffer);
    }
  };

  void dump_2(std::ostream &out) const {
    dump_ulong(out, size);
    for(ulong i = 0; i < size; i++){
      dump_string(out, entries[i]->str());
      dump_float(out, entries[i]->freq());
    }
  };

  void load_2(std::istream &in, const std::string &fname){
    ulong nattribs;
    if(!load_ulong(in, nattribs))
      throw NLP::IOException("missing number of attributes", fname, in.tellg());

    char buffer[1024];
    float freq;
    for(ulong i = 0; i < nattribs; i++){
      if(!load_string(in, buffer, sizeof(buffer)))
        throw NLP::IOException("could not load attribute string", fname, in.tellg());
      if(!load_float(in, freq))
        throw NLP::IOException("could not load attribute frequency", fname, in.tellg());
      add(buffer, freq);
    }
  };
};

Attributes::Attributes(Types types, Pool *pool, Pool *lpool): _impl(new _Impl(types, pool, lpool)){}
Attributes::Attributes(const Attributes &other): _impl(share(other._impl)){}
Attributes::~Attributes(void){ release(_impl); }

Attributes &Attributes::operator=(const Attributes &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

ulong
Attributes::size(void) const { return _impl->size; }

Attribute *Attributes::add(char *word){ return _impl->add(word); }
Attribute *Attributes::add(char *word, float freq){ return _impl->add(word, freq); }

Attribute *Attributes::get(ulong id) const { return _impl->entries[id]; };
Attribute *Attributes::find(const char *s) const { return _impl->find(s); };

// serializing interface
void Attributes::load_1(std::istream &in, const std::string &fname){
  _impl->load_1(in, fname);
}
void Attributes::dump_1(std::ostream &out) const{
  _impl->dump_1(out);
}

void Attributes::load_2(std::istream &in, const std::string &fname){
  _impl->load_2(in, fname);
}
void Attributes::dump_2(std::ostream &out) const{
  _impl->dump_2(out);
}

void Attributes::printstats(std::ostream &os) const { _impl->printstats(os); }

void Attributes::sort_by_alpha(void){ _impl->sort_by_alpha(); }

} }
