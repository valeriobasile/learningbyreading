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

#include "thesaurus/weights/chi2.h"

namespace NLP { namespace Thesaurus {

float
WChi2::operator()(const Object *o, const Attribute *a, const Relation *r) const {
  // Chi^2 test contingency table
  float N = Object::ftotal;
  float t11 = f(r);
  float t12 = f(o) - t11;	// obj and not attrib
  float t21 = f(a) - t11;	// not obj and attrib
  float t22 =  N - t12 - t21 + t11;	// not obj and not attrib

  return (t11*t22 - t12*t21)*(t11*t22 - t12*t21)/
         ((t11 + t12)*(t11 + t21)*(t12 + t22)*(t21 + t22));
};

} }
