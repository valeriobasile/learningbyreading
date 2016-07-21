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


// grefenstette jaccard pairs measure

#include "utils.h"
#include "utils/io.h"

#include "hash.h"
#include "pool.h"

#include "hashtable/size.h"
#include "hashtable/entry.h"
#include "hashtable/count.h"

template <class V>
struct stats {
  const V &val;

  stats(const V &val): val(val) {};
};

template <class V>
inline std::ostream &
operator <<(std::ostream &out, const stats<V> &s){
  s.val.printstats(out);
  return out;
}

#include "thesaurus/options.h"
#include "thesaurus/type.h"
#include "thesaurus/types.h"
#include "thesaurus/attribute.h"
#include "thesaurus/relation.h"
#include "thesaurus/object.h"
#include "thesaurus/objects.h"
