// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <cmath>
#include <string>
#include <vector>
#include <algorithm>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <stdexcept>

using namespace std;

#include "except.h"
#include "utils.h"
#include "utils/io.h"

#include "hash.h"
#include "pool.h"
#include "hashtable/entry.h"

#include "thesaurus/options.h"
#include "thesaurus/type.h"
#include "thesaurus/types.h"
#include "thesaurus/attribute.h"
#include "thesaurus/attributes.h"
#include "thesaurus/weight.h"
#include "thesaurus/relation.h"
#include "thesaurus/object.h"
#include "thesaurus/objects.h"

#include "thesaurus/match.h"
#include "thesaurus/measure.h"
#include "thesaurus/explain.h"

#include "thesaurus/measures/jaccard2.h"

namespace NLP { namespace Thesaurus {

float
MJaccard2::measure(const Object *const obj1, const Object *const obj2) const {
  const Relations &r1 = obj1->relations;
  const Relations &r2 = obj2->relations;

  Relations::const_iterator i = r1.begin();
  Relations::const_iterator j = r2.begin();

  float num = 0;
  float denom = 0;
  while(i != r1.end() && j != r2.end()){
    if(i->equal(*j)){
      // we have an identical relation
      num += i->score()*j->score();
      i++;
      j++;
    }else if(i->compare(*j) < 0)
      i++;
    else
      j++;
  }
  denom = obj1->score() + obj2->score() - num;
  return div0(num, denom);
};

bool
MJaccard2::heuristic(const Object *const obj1, const Object *const obj2) const {
  const vector<Relation *> &r1 = obj1->canonical;
  const vector<Relation *> &r2 = obj2->canonical;

  vector<Relation *>::const_iterator i = r1.begin();
  vector<Relation *>::const_iterator j = r2.begin();
  while(i != r1.end() && j != r2.end()){
    if((*i)->equal(*j))
      return true;
    else if((*i)->compare(*j) < 0)
      i++;
    else
      j++;
  }
  return false;
};

void
MJaccard2::explain(std::ostream &out, const Object *const obj1, const Object *const obj2) const {
  float num = 0;
  float denom = 0;
  ulong ncommon = 0;
  ulong n1 = 0;
  ulong n2 = 0;

  if(true){
    const Relations &r1 = obj1->relations;
    const Relations &r2 = obj2->relations;

    Relations::const_iterator i = r1.begin();
    Relations::const_iterator j = r2.begin();

    out << obj1->str() << " and " << obj2->str() << "share the following terms:\n";
    while(i != r1.end() && j != r2.end()){
      if(i->equal(*j)){
        // we have an identical relation
        print_unequal(out, obj2, j);
        num += i->score()*j->score();
        ncommon++;
        i++;
        j++;
      }else if(i->compare(*j) < 0){
        n1++;
        i++;
      }else{
        n2++;
        j++;
      }
    }
    n1 += r1.end() - i;
    n2 += r2.end() - j;

    print_common(out, ncommon, num, 1.0);
    print_unique(out, obj1, n1, 0.0);
    print_unique(out, obj2, n2, 0.0);
    denom = obj1->score() + obj2->score() - num;
    print_score(out, "Jaccard2", num, denom);
  }else
    print_failed(out, obj1, obj2);
};

} }
