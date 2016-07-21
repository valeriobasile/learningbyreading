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


namespace NLP {
  namespace HashTable {

    template <class E>
    class AlphaCmp {
    public:
      bool operator ()(const E *const e1, const E *const e2){
        return strcmp(e1->str, e2->str) < 0;
      }
    };

    template <class E>
    class ValueCmp {
    public:
      bool operator ()(const E *const e1, const E *const e2){
        return e1->value < e2->value;
      }
    };

    template <class E>
    class RevValueCmp {
    public:
      bool operator ()(const E *const e1, const E *const e2){
        return e1->value > e2->value;
      }
    };

    template <class E>
    class IndexCmp {
    public:
      bool operator ()(const E *const e1, const E *const e2){
        return e1->index < e2->index;
      }
    };

    template <class Entry, class Key, ulong NBUCKETS, ulong SPOOL>
    class Ordered: public Base<Entry, Key, NBUCKETS, SPOOL>{
    public:
      typedef Base<Entry, Key, NBUCKETS, SPOOL> Super;

      typedef std::vector<Entry *> Entries;
      typedef typename Entries::iterator iterator;
      typedef typename Entries::const_iterator const_iterator;

      Entries entries;

      Ordered(std::string name, Pool *pool = 0)
	: Super(name, pool){}
      virtual ~Ordered(void){ /* do nothing */ }

      using Super::insert;
      virtual Entry *insert(Key key, const Hash hash, const ulong bucket){
	Entry *entry = Super::insert(key, hash, bucket);
        entries.push_back(entry);
        return entry;
      }

      virtual void clear(void){
	Super::clear();
	entries.resize(0);
      }

      virtual void renumber(void){
        for(ulong i = 0; i != entries.size(); ++i)
          entries[i]->index = i;
      }

      void compact(void){
        iterator new_end = std::remove(entries.begin(), entries.end(), reinterpret_cast<Entry *>(0));
        entries.erase(new_end, entries.end());
      }

      void compress(void){
        iterator new_end = std::remove(entries.begin(), entries.end(), reinterpret_cast<Entry *>(0));
        entries.erase(new_end, entries.end());
        renumber();
      }

      template <class Comparator>
      void sort(Comparator cmp){ std::sort(entries.begin(), entries.end(), cmp); }

      void sort_by_alpha(void){ sort(AlphaCmp<Entry>()); renumber(); }
      void sort_by_value(void){ sort(ValueCmp<Entry>()); renumber(); };
      void sort_by_rev_value(void){ sort(RevValueCmp<Entry>()); renumber(); };

      void save(std::ostream &out) const {
	for(const_iterator i = entries.begin(); i != entries.end(); ++i)
	  (*i)->save(out) << '\n';
      }
    };

  }
}
