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

#include <iostream>
#include <iomanip>
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
#include "thesaurus/weights/utils.h"

#include "thesaurus/weights/lr.h"

namespace NLP { namespace Thesaurus {

float
WLR::operator()(const Object *o, const Attribute *a, const Relation *r) const {
  // log likelihood based on Manning and Sch\"{u}tze
  float N = Object::ftotal;
  long c1 = o->freq();
  long c2 = a->freq();
  long c12 = r->freq();
  float p = c2/N;
  float np = 1.0 - p;
  float p1 = c12/(float)c1;
  float p2 = (c2 - c12)/(N - c1);

  float value = 0;

  if(p1 != 1.0) value -= (c1 - c12)*log(np/(1 - p1));
  if(p2 != 1.0) value -= ((N - c1) - (c2 - c12))*log(np/(1 - p2));

  if(c12 != 0) value -= c12*log(p/p1);
  if(c2 != c12) value -= (c2 - c12)*log(p/p2);

  return value;
};

} }
