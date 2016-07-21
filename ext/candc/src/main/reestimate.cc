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

#include "base.h"

#include "cluster.h"

#include "prob.h"

#include "config/config.h"
#include "io/format.h"
#include "config/format.h"

#include "model/model.h"
#include "model/types.h"

#include "io/reader.h"
#include "io/reader_factory.h"
#include "io/writer.h"

#include "tagger/tagdict.h"
#include "tagger/tagger.h"

#include "maxent/feature.h"
#include "maxent/attribute.h"
#include "maxent/context.h"
#include "maxent/gis.h"
#include "maxent/bfgs.h"
#include "maxent/perceptron.h"

#include "timer.h"

const char *PROGRAM_NAME = "reestimate";

#include "main.h"

using namespace std;
using namespace NLP;

int
run(int argc, char **argv){
  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);

  Config::Op<bool> verbose(cfg, "verbose", "print information about training on stderr", false);

  Model::Config model_cfg("new", "the existing model directory", 0, Model::Config::ESTIMATE, 1.414, 200);

  Config::Alias model_alias(cfg, model_cfg, "model", model_cfg.NAME);

  Config::Alias weights_alias(cfg, SPACE, model_cfg.model.weights, "weights", model_cfg.NAME + "-model-weights");
  Config::Alias comment(cfg, model_cfg.model.comment, "comment", model_cfg.NAME + "-model-comment");

  Config::Alias solver_alias(cfg, SPACE, model_cfg.model.solver, "solver", model_cfg.NAME + "-model-solver");
  Config::Alias niter_alias(cfg, model_cfg.model.niterations, "niterations", model_cfg.NAME + "-model-niterations");
  Config::Alias sigma_alias(cfg, model_cfg.model.sigma, "sigma", model_cfg.NAME + "-model-sigma");

  Config::OpPath continuation(cfg, SPACE, "continue", "continue estimating the existing model", "", &model_cfg.path);

  cfg.reg(model_cfg);

  cfg.parse(argc, argv);
  cfg.check();

  if(model_cfg.model.weights.is_default())
    throw ConfigError("the weights option must be defined for re-estimating a model", "weights");

  Timer timer("total");

  NLP::Cluster::init(argc, argv, false);
  MaxEnt::GIS *solver = 0;
  if(model_cfg.model.solver() == "gis")
    solver = new MaxEnt::GIS(model_cfg.model, verbose());
  else if(model_cfg.model.solver() == "bfgs")
    solver = new MaxEnt::BFGS(model_cfg.model, verbose());
  else if(model_cfg.model.solver() == "perceptron")
    solver = new MaxEnt::Perceptron(model_cfg.model, verbose());
  else
    assert(!"unknown MaxEnt solver should have been caught!");

  if(!continuation.is_default())
    solver->load_weights(continuation());

  solver->init();
  solver->iterate();
  solver->save(PREFACE.str());

  return 0;
}
