// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "pool.h"
#include "hashtable/entry.h"

namespace NLP {

typedef HashTable::Entry<ulong> WordInfo;

Hash Word::hash_(void) const {
  assert(sizeof(ID) == sizeof(WordInfo *));
  return reinterpret_cast<const WordInfo *>(id_)->hash;
}

ulong Word::freq_(void) const{
  assert(sizeof(ID) == sizeof(WordInfo *));
  return reinterpret_cast<const WordInfo *>(id_)->value;
}

const char *Word::str_(void) const {
  assert(sizeof(ID) == sizeof(WordInfo *));
  return reinterpret_cast<const WordInfo *>(id_)->str;
}

ulong Word::index_(void) const{
  assert(sizeof(ID) == sizeof(WordInfo *));
  return reinterpret_cast<const WordInfo *>(id_)->index;
}

}
