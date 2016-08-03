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

#include "prob.h"

#include "io/reader.h"
#include "io/writer.h"

#include "config/config.h"

#include "affix.h"

#include "model/model.h"
#include "model/types.h"
#include "model/feature.h"
#include "model/attribute.h"
#include "model/attributes.h"
#include "model/registry.h"

#include "pool.h"
#include "classifier.h"

#include "timer.h"
#include "share.h"

using namespace std;
using namespace NLP::Model;

namespace NLP { namespace Classifier {

Classifier::Config::Config(const OpPath *base, Mode mode,
			   const std::string &name, const std::string &desc,
			   double SIGMA, ulong NITER)
  : NLP::Model::Config(name, desc, base, mode, SIGMA, NITER),
    cutoff(*this, "cutoff", "the minimum frequency cutoff for features", 1){}



    }
}
