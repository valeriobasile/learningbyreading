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

// NLP::Hash
// hash value type and operator overloaded support methods
// allows new hash functions to be implemented in a uniform
// manner with the same basic shift and add approach to
// implementing the hash function

// in practice this methods works quite well

namespace NLP {

  class Hash {
  private:
    // value to set hash values to if initially undefined
    const static ulong _INIT = 0;

    // shift value to multiply by with each additional
    // component of the hash function
    // this number is normally selected to be a smallish prime
    // 31 and 37 seem to work well
    const static ulong _INC = 31;

    // hash value currently stored in an unsigned long
    ulong _hash;
  public:
    // += operators which do the shift and add automatically
    Hash &operator +=(const char c){
      _hash = _hash*_INC + static_cast<uchar>(c);
      return *this;
    }

    Hash &operator +=(const char *str){
      for( ; *str; ++str)
        *this += *str;
      return *this;
    };

    Hash &operator +=(const std::string &str){
      for(const char *s = str.c_str(); *s; ++s)
        *this += *s;
      return *this;
    };

    Hash &operator +=(const Hash &hash){
      _hash *= _INC;
      _hash += hash._hash;
      return *this;
    }

    Hash &operator +=(uchar value){
      _hash *= _INC;
      _hash += value;
      return *this;
    }
    
    Hash &operator +=(ushort value){
      _hash *= _INC;
      _hash += value;
      return *this;
    }

    Hash &operator +=(ulong value){
      _hash *= _INC;
      _hash += value;
      return *this;
    }

    Hash &operator +=(int value){ return *this += static_cast<ulong>(value); };

    // for greater manual control over hash function shifting
    // and adding, there are the separated out add and shift (multiply) steps
    Hash &operator |=(ulong value){ _hash += value; return *this; };
    Hash &operator *=(ulong value){ _hash *= value; return *this; };

    Hash(const Hash &other): _hash(other._hash) {};

    // make all the constructors explicit so there are no nasty
    // surprises with automatic casts
    explicit Hash(ulong hash): _hash(hash) {};
    explicit Hash(ushort hash): _hash(hash) {};
    explicit Hash(uchar hash): _hash(hash) {};
    explicit Hash(char c): _hash(_INIT) { *this += c; };
    explicit Hash(const char *str): _hash(_INIT) { *this += str; };
    explicit Hash(const std::string &str): _hash(_INIT) { *this += str; };

    // get a unsigned long back for indexing by applying mod
    ulong operator %(const size_t buckets) const { return _hash % buckets; };

    // get the unsigned long internal value
    ulong value(void) const { return _hash; };
  };

  inline bool operator ==(const Hash &a, const Hash &b){ return a.value() == b.value(); }
  inline bool operator !=(const Hash &a, const Hash &b){ return a.value() != b.value(); }
}
