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

    template <class Value>
    class WordEntry {
    private:
      WordEntry(const Word word, NLP::Hash hash, WordEntry *next)
	: str(word), hash(hash), next(next), value(){}

      //      Entry(const Entry *other, ulong index, Entry *next)
      //: str(other->str), hash(other->hash), index(index), next(next), freq(0){}

      ~WordEntry(void) { /* do nothing */ }

      void *operator new(size_t size, Pool *pool){
	return (void *)pool->alloc(size);
      }

      void operator delete(void *, Pool *, size_t){ /* do nothing */ }
    public:
      const Word str;
      const NLP::Hash hash;
      WordEntry *next;
      Value value;

      static WordEntry *create(Pool *pool, Word word, ulong, NLP::Hash hash, WordEntry *next){
	return new (pool) WordEntry(word, hash, next);
      }

      bool equal(const Word word){ return word == this->str; }

      bool equal(const std::string &str, const Hash hash){
	return hash == this->hash && str == this->str;
      }

      WordEntry *find(const Word word, const Hash){
        for(WordEntry *l = this; l != 0; l = l->next)
          if(l->equal(word))
            return l;
        return 0;
      }

      WordEntry *find(const std::string &str, const Hash hash){
        for(WordEntry *l = this; l != 0; l = l->next)
          if(l->equal(str, hash))
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
