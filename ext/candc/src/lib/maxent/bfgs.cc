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
#include "cluster.h"

#include <valarray>
#include <deque>
#include <cstring>

using namespace std;

#include "prob.h"

#include "config/config.h"

#include "model/model.h"

#include "maxent/feature.h"
#include "maxent/attribute.h"
#include "maxent/context.h"
#include "maxent/gis.h"
#include "maxent/bfgs.h"

#include "maxent/solver.h"

namespace NLP { namespace MaxEnt {

BFGS::BFGS(const Model::Model &model, bool verbose)
  : GIS(model, verbose),
    _solver(new Solver<BFGS>(*this, info.nfeatures(), 10, 1e-6, verbose)){}

BFGS::~BFGS(void){
  delete _solver;
}

void
BFGS::init(void){
  _solver->init();
}

double
BFGS::llhood(void){
  PDF p_classes(NKLASSES, 0.0);

  for(Features::iterator i = features.begin(); i != features.end(); ++i)
    i->est = 0.0;

  double result = 0.0;
  for(Contexts::iterator context = contexts.begin(); context != contexts.end(); ++context){
    _sum(*context, p_classes);
    _normalise(*context, p_classes);
    _estimate(*context, p_classes);
    result += (*context)->llhood(p_classes);
  }
  // Add up the result values for all the contexts (distributed over the nodes)
  assert(!std::isnan(result));

  if(Cluster::USE_MPI)
    Cluster::sum(result);

  _sum_estimates();

  return result;
}

double
BFGS::compute(Gradient &grad){
  double objective = llhood();
  for(ulong i = 0; i < features.size(); ++i){
     grad[i] = -features[i].grad_gaussian(ALPHA);
     objective -= features[i].penalty_gaussian(ALPHA);
  }
  return -objective;
}

void
BFGS::update(Direction &dir, double scale){
  for(ulong i = 0; i < features.size(); ++i)
    features[i].lambda += scale*dir[i];
}

bool
BFGS::iteration(void){
  return _solver->iteration();
}

void
BFGS::iterate(void){
  _solver->iterate(VERBOSE, model.niterations());
}

} }
