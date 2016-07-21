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

// NLP::HashTable::Count
// simple counting hash table template class
// uses linked lists to overcome hash collisions
// supports hash lookup, frequency counts and entry lists
// with an entries and strings memory pool

namespace NLP {
  namespace HashTable {

    template <class Entry, ulong NBUCKETS, ulong SPOOL>
    class Count: public Ordered<Entry, NBUCKETS, SPOOL> {
    public:
      typedef Ordered<Entry, NBUCKETS, SPOOL> Super;

      typedef typename Super::const_iterator const_iterator;

      Count(std::string name, Pool *pool = 0)
	: Super(name, pool){}

      virtual ~Count(void){ /* do nothing */ }

      using Super::find;
      using Super::renumber;

      ulong index(const std::string &str) const {
        Entry *entry = find(str);
        if(entry)
          return entry->index;

        return 0;
      }

      ulong freq(const std::string &str) const {
        Entry *entry = find(str);
        if(entry)
          return entry->freq;

        return 0;
      }

      void sort_by_freq(void){ sort(FreqCmp<Entry>()); renumber(); };
      void sort_by_rev_freq(void){ sort(RevFreqCmp<Entry>()); renumber(); };

      void save_freq(std::ostream &out) const {
        for(const_iterator i = entries.begin(); i != entries.end(); ++i)
          (*i)->save_freq(out) << '\n';
      }
    };

  }
}
