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

// NLP::HashTable::TINY -- NLP::HashTable::MASSIVE
// constant sizes for hash table template parameters
// these parameters are either the size of the string and entry memory pools
// (well in fact the size of each arena that the memory pool creates)
// and also the number of buckets in the hashtable

namespace NLP {
  namespace HashTable {

    static const ulong BABY = 1 << 10;
    static const ulong TINY = 1 << 16;
    static const ulong SMALL = 1 << 18;
    static const ulong MEDIUM = 1 << 20;
    static const ulong LARGE = 1 << 22;
    static const ulong MASSIVE = 1 << 24;

  }
}
