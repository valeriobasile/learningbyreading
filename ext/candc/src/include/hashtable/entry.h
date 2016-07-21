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

// NLP::HashTable::Entry, NLP::HashTable::Counter and NLP::HashTable::FCounter
// basic hash table entry types
// the counter classes provide an additional integer and float frequency fields
// and methods to set, get and update the frequency count

namespace NLP {
  namespace HashTable {

    inline size_t
    aligned_size(size_t len, size_t alignment){
      return (len/alignment + 1)*alignment;
    }

    static const size_t MIN_STR_BUFFER = 4;

    inline size_t
    get_remainder(size_t len){
      size_t remainder = len + 1;
      if(remainder < MIN_STR_BUFFER)
	return 0;
      else
	return remainder - MIN_STR_BUFFER;
    }

    template <class Value>
    class Entry {
    private:
      Entry(ulong index, NLP::Hash hash, Entry *next)
        : index(index), next(next), value(), hash(hash){}

      //      Entry(const Entry *other, ulong index, Entry *next)
      //: str(other->str), hash(other->hash), index(index), next(next), freq(0){}

      ~Entry(void) { /* do nothing */ }

      void *operator new(size_t size, Pool *pool, size_t len){
        ulong alignment = (sizeof(Value) > sizeof(Entry *)) ? sizeof(Value) : sizeof(Entry *);
        return (void *)pool->alloc(size + aligned_size(len, alignment));
      }

      void operator delete(void *, Pool *, size_t) { /* do nothing */ }
    public:
      ulong index;
      Entry *next;
      Value value;
      const NLP::Hash hash;
      char str[MIN_STR_BUFFER];

      static Entry *create(Pool *pool, const std::string &str, ulong index, NLP::Hash hash, Entry *next){

        Entry *entry = new (pool, get_remainder(str.size())) Entry(index, hash, next);
        strcpy(entry->str, str.c_str());
        return entry;
      }

      bool equal(const std::string &str, const Hash hash){
        return hash == this->hash && str == this->str;
      }

      bool equal(const char c){ return str[0] == c && str[1] == '\0'; }

      Entry *find(const Hash hash){
        for(Entry *l = this; l != 0; l = l->next)
          if(l->hash == hash)
            return l;
        return 0;
      }

      Entry *find(const std::string &str, const Hash hash){
        for(Entry *l = this; l != 0; l = l->next)
          if(l->equal(str, hash))
            return l;
        return 0;
      }

      Entry *find(const char c){
        for(Entry *l = this; l != 0; l = l->next)
          if(l->equal(c))
            return l;
        return 0;
      }

      ulong nchained(void){
        return next ? next->nchained() + 1 : 1;
      }
      
      std::ostream &save(std::ostream &out) const {
        return out << str << ' ' << std::setprecision(4) << value;
      }
    };

  }
}
