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

using namespace std;

#include "except.h"
#include "utils.h"
#include "utils/io.h"

#include "hash.h"
#include "pool.h"

#include "hashtable/size.h"
#include "hashtable/entry.h"
#include "hashtable/count.h"

#include "thesaurus/options.h"
#include "thesaurus/type.h"
#include "thesaurus/types.h"

#include "share.h"

namespace NLP { namespace Thesaurus {

using namespace HashTable;

typedef HashTable::Count<Type,TINY,TINY,TINY> _ImplBase;

class Types::_Impl: public _ImplBase, public Shared {
public:
  _Impl(Pool *pool = 0, Pool *lpool = 0): _ImplBase("types", pool, lpool) {};
  ~_Impl(void) { /* do nothing */ };

   using _ImplBase::add;

   Type *extract(const char *word){
     for( ; *word; ++word)
       if(*word == ' ' || *word == '@')
         return add(word + 1);
       return add(word);
   }

   Type *add(const char *const word, ulong freq){
     Type *type = add(word);
     type->inc(freq);
     return type;
   }

  // serializing interface
  void dump_1(std::ostream &out) const {
    dump_ulong(out, size);
    for(ulong i = 0; i < size; i++)
      dump_string(out, entries[i]->str());
  };

  void load_1(std::istream &in, char *fname){
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
      dump_ulong(out, entries[i]->freq());
    }
  };

  void load_2(std::istream &in, char *fname){
    ulong nattribs;
    if(!load_ulong(in, nattribs))
      throw NLP::IOException("missing number of attributes", fname, in.tellg());
  
    char buffer[1024];
    ulong freq;
    for(ulong i = 0; i < nattribs; i++){
      if(!load_string(in, buffer, sizeof(buffer)))
        throw NLP::IOException("could not load attribute string", fname, in.tellg());
      if(!load_ulong(in, freq))
        throw NLP::IOException("could not load attribute frequency", fname, in.tellg());
      add(buffer, freq);
    }
  };
};

Types::Types(Pool *pool, Pool *lpool): _impl(new _Impl(pool, lpool)) {};

Types::Types(const Types &other): _impl(share(other._impl)){}

Types &
Types::operator=(const Types &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Types::~Types(void) { release(_impl); };

ulong Types::size(void) const { return _impl->size; };

Type *Types::find(std::string s) const { return _impl->find(s.c_str()); }
Type *Types::get(ulong id) const{ return _impl->entries[id]; }

Type *Types::add(const char *word){ return _impl->add(word); }
Type *Types::add(const char *word, ulong freq){ return _impl->add(word, freq); }

Type *Types::extract(const char *word){ return _impl->extract(word); };

// serializing interface
void Types::load_1(std::istream &in, char *fname){ _impl->load_1(in, fname); }
void Types::dump_1(std::ostream &out) const{ _impl->dump_1(out); }

void Types::load_2(std::istream &in, char *fname){ _impl->load_2(in, fname); }
void Types::dump_2(std::ostream &out) const{ _impl->dump_2(out); }

} }
