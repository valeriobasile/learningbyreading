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

using namespace std;
using namespace NLP;

template <class TAGGER, class EXTRACTOR>
int
run_train(const char *IFMT, int argc, char **argv){
  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);
  typename TAGGER::Config tagger_cfg(0, TAGGER::Config::TRAIN);

  Config::Op<bool> verbose(cfg, "verbose", "print information about training on stderr", false);

  Config::Alias comment_alias(cfg, SPACE, tagger_cfg.model.comment, "comment", tagger_cfg.NAME + "-model-comment");

  Config::Alias input(cfg, SPACE, tagger_cfg.model.data, "input", tagger_cfg.NAME + "-model-data");
  Config::Op<IO::Format> ifmt(cfg, "ifmt", "the input data field format", Format(IFMT));

  Config::Alias model_alias(cfg, SPACE, tagger_cfg, "model", tagger_cfg.NAME);

  Config::Alias solver_alias(cfg, SPACE, tagger_cfg.model.solver, "solver", tagger_cfg.NAME + "-model-solver");
  Config::Alias niter_alias(cfg, tagger_cfg.model.niterations, "niterations", tagger_cfg.NAME + "-model-niterations");
  Config::Alias sigma_alias(cfg, tagger_cfg.model.sigma, "sigma", tagger_cfg.NAME + "-model-sigma");

  Config::Op<bool> extract_only(cfg, SPACE, "extract_only", "perform the extraction phase only", false);

  cfg.reg(tagger_cfg, SPACE);

  cfg.parse(argc, argv);
  cfg.check();

  Timer timer("total");

  IO::ReaderFactory reader(tagger_cfg.model.data(), ifmt());

  EXTRACTOR extractor(tagger_cfg, PREFACE.str(), verbose());
  extractor.extract(reader);

  if (!extract_only()) {
    NLP::Cluster::init(argc, argv, false);
    MaxEnt::GIS *solver = 0;
    if(tagger_cfg.model.solver() == "gis")
      solver = new MaxEnt::GIS(tagger_cfg.model, verbose());
    else if(tagger_cfg.model.solver() == "bfgs")
      solver = new MaxEnt::BFGS(tagger_cfg.model, verbose());
    else if(tagger_cfg.model.solver() == "perceptron")
      solver = new MaxEnt::Perceptron(tagger_cfg.model, verbose());
    else
      assert(!"unknown MaxEnt solver should have been caught!");

    solver->init();
    solver->iterate();
    solver->save(PREFACE.str());
  }

  return 0;
}
