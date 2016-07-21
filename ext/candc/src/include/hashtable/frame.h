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

    template <class E, ulong NBUCKETS, ulong PENTRIES>
    class Frame {
    public:
      typedef E Entry;
      typedef std::vector<Entry *> Entries;
    protected:
      static const ulong _NBUCKETS = NBUCKETS;
      static const ulong _PENTRIES = PENTRIES;

      Pool *const _ent_pool;
      const bool _shared_ent_pool;
    public:
      const std::string name;
      size_t size;
    protected:
      Entry *_buckets[_NBUCKETS];
    public:

      Frame(const std::string &name, Pool *entpool = 0):
          _ent_pool(_PENTRIES ? (entpool ? entpool : new Pool(_PENTRIES)) : 0),
          _shared_ent_pool(entpool != 0),
          name(name), size(0) {
        memset(_buckets, 0, sizeof(_buckets));
      }

      ~Frame(void){
        if(!_shared_ent_pool)
          delete _ent_pool;
      }

      Pool *ent_pool(void) { return _ent_pool; };

      void printstats(std::ostream &os) const {
        size_t maxchain = 0;
        size_t nbins = 0;
        size_t nbytes = 0;

        for(ulong i = 0; i < _NBUCKETS; i++){
          if(_buckets[i]){
            ulong temp = _buckets[i]->nchained();
            if(maxchain < temp)
              maxchain = temp;
            nbins++;
          }
        }

        os << "number of entries " << size << '\n';
        os << "number of bins used " << nbins << " (of " << _NBUCKETS << ")\n";
        os << "used bins/nbins " << nbins/static_cast<float>(_NBUCKETS) << '\n';
        os << "maximum chain length " << maxchain << '\n';
        os << "average chain length " << size/static_cast<float>(nbins) << '\n';

        nbytes = size * sizeof(Entry);
        os << "      entry objs " << nbytes << " bytes\n";
        nbytes += sizeof(_buckets);
        os << "      bin []     " << sizeof(_buckets) << " bytes\n";
        os << "total            " << nbytes << " bytes\n";
      }
    };

  }
}
