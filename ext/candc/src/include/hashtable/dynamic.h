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
    class Dynamic {
    public:
      typedef E Entry;
      typedef std::vector<Entry *> Entries;
    protected:
      const ulong _NBUCKETS;
      const ulong _PENTRIES;
      const ulong _PSTRINGS;

      Pool *const _ent_pool;
      const bool _shared_ent_pool;

      Pool *const _str_pool;
      const bool _shared_str_pool;
    public:
      const std::string name;
      size_t size;
    protected:
 
      Entry **_buckets;
    public:

      Dynamic(const std::string &name, ulong SIZE, Pool *entpool = 0, Pool *strpool = 0):
          _NBUCKETS(SIZE), _PENTRIES(SIZE*4), _PSTRINGS(SIZE*4),
          _ent_pool(_PENTRIES ? (entpool ? entpool : new Pool(_PENTRIES)) : 0),
          _shared_ent_pool(entpool != 0),
          _str_pool(_PSTRINGS ? (strpool ? strpool : new Pool(_PSTRINGS)) : 0),
          _shared_str_pool(strpool != 0),
          name(name), size(0), _buckets(new Entry *[_NBUCKETS]){
        memset(_buckets, 0, sizeof(Entry *)*_NBUCKETS);
      }

      ~Dynamic(void){
        if(!_shared_str_pool)
          delete _str_pool;

        if(!_shared_ent_pool)
          delete _ent_pool;
      }

      Pool *str_pool(void) { return _str_pool; };
      Pool *entry_pool(void) { return _ent_pool; };

      Entry *insert(const char *str, const Hash hash, const size_t len){
        ulong bucket = hash % _NBUCKETS;

        if(_PSTRINGS)
          str = _str_pool->strdup(str, len);

        Entry *entry = new (_ent_pool) Entry(str, size, hash, _buckets[bucket]);
        _buckets[bucket] = entry;
        ++size;

        return entry;
      };
      Entry *insert(const char *str, Hash hash){ return insert(str, hash, _PSTRINGS ? strlen(str) : 0); }
      Entry *insert(const char *const str){ return insert(str, NLP::Hash(str)); }

      Entry *add(const char *str, const Hash hash, const size_t len){
        ulong bucket = hash % _NBUCKETS;
        Entry *entry = _buckets[bucket]->find(str, hash);
        if(entry)
          return entry;

        if(_PSTRINGS)
          str = _str_pool->strdup(str, len);

        entry = new (_ent_pool) Entry(str, size, hash, _buckets[bucket]);
        _buckets[bucket] = entry;
        ++size;

        return entry;
      }
      Entry *add(const char *str, Hash hash){ return add(str, hash, _PSTRINGS ? strlen(str) : 0); }
      Entry *add(const char *const str){ return add(str, NLP::Hash(str)); }

      Entry *find(const char *const str, Hash hash) const {
        return _buckets[hash % _NBUCKETS]->find(str, hash);
      }
      Entry *find(const char *const str) const { return find(str, NLP::Hash(str)); }
      Entry *find(const char c) const { return _buckets[hash % _NBUCKETS]->find(c); }

      const char *canonize(const char *const str, const Hash hash) const {
        Entry *entry = _buckets[hash % _NBUCKETS]->find(str, hash);
        if(entry)
          return entry->str();

        return 0;
      }
      const char *canonize(const char *const str) const {
        return canonize(str, NLP::Hash(str));
      }

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
        nbytes += sizeof(Entry *)*_NBUCKETS;
        os << "      bin []     " << sizeof(_buckets) << " bytes\n";
        os << "total            " << nbytes << " bytes\n";
      }
    };

  }
}
