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
#include "thesaurus/measures.h"

#include "thesaurus/measures/cosine.h"
#include "thesaurus/measures/bincosine.h"

#include "thesaurus/measures/dice.h"
#include "thesaurus/measures/dice2.h"
#include "thesaurus/measures/bindice.h"

#include "thesaurus/measures/jaccard.h"
#include "thesaurus/measures/jaccard2.h"
#include "thesaurus/measures/binjaccard.h"

#include "thesaurus/measures/lin.h"
#include "thesaurus/measures/lin2.h"

#include "thesaurus/measures/manhattan.h"
#include "thesaurus/measures/euclidean.h"

#include "thesaurus/measures/skew.h"
#include "thesaurus/measures/js.h"

namespace NLP { namespace Thesaurus {

MeasureRegistry::MeasureRegistry(void){
  install(new MCosine);
  install(new MBinaryCosine);
  install(new MDice);
  install(new MDice2);
  install(new MBinaryDice);
  install(new MJaccard);
  install(new MJaccard2);
  install(new MBinaryJaccard);
  install(new MLin);
  install(new MLin2);
  install(new MManhattan);
  install(new MEuclidean);
  install(new MSkew);
  install(new MJS);
}

} }
