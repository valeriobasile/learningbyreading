// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <mpi.h>

#include <cassert>
#include <cmath>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>
#include <limits>
#include <valarray>

#include <sys/types.h>
#include <unistd.h>

#include "base.h"

#include "cluster.h"

#include "config/config.h"
#include "io/format.h"
#include "config/format.h"

#include "prob.h"

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

#include "tagger/tagsetdict.h"
#include "tagger/super.h"
#include "extract/super.h"

const char *PROGRAM_NAME = "mpi_train_super";

int rank = 1;
int size = 1;

using namespace std;
using namespace NLP;

int
main(int argc, char* argv[]){
  NLP::Cluster::init(argc, argv, true);
  const char *IFMT = "%w|%p|%s \n";

  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);
  Taggers::Super::Config tagger_cfg(0, Taggers::Super::Config::TRAIN);

  Config::Op<bool> verbose(cfg, "verbose", "print information about training on stderr", false);

  Config::Alias comment_alias(cfg, SPACE, tagger_cfg.model.comment, "comment", tagger_cfg.NAME + "-model-comment");

  Config::Alias input(cfg, SPACE, tagger_cfg.model.data, "input", tagger_cfg.NAME + "-model-data");
  Config::Op<IO::Format> ifmt(cfg, "ifmt", "the input data field format", Format(IFMT));

  Config::Alias model_alias(cfg, SPACE, tagger_cfg, "model", tagger_cfg.NAME);

  Config::Alias solver_alias(cfg, SPACE, tagger_cfg.model.solver, "solver", tagger_cfg.NAME + "-model-solver");
  Config::Alias niter_alias(cfg, tagger_cfg.model.niterations, "niterations", tagger_cfg.NAME + "-model-niterations");
  Config::Alias sigma_alias(cfg, tagger_cfg.model.sigma, "sigma", tagger_cfg.NAME + "-model-sigma");

  cfg.reg(tagger_cfg, SPACE);
  
  cfg.parse(argc, argv);
  cfg.check();

  Timer timer("total");

  string data_file = tagger_cfg.model.data();
  if(tagger_cfg.model.dist_input()){
    tagger_cfg.model.split_input.set_value(false);
    data_file += '.' + Cluster::rank_str;
  }

  IO::ReaderFactory reader(data_file, ifmt());
  
  Extract::Super *extractor = new Extract::Super(tagger_cfg, PREFACE.str(), verbose());
  extractor->extract(reader);
  delete extractor;

  ostringstream out;
  out << "mpi_gis " << NLP::Cluster::rank << " of " << NLP::Cluster::size;
  out << " on " << NLP::Cluster::processor << " pid " << getpid() << '\n';
  cerr << out.str() << flush;

  MaxEnt::GIS *solver = 0;
  if(tagger_cfg.model.solver() == "gis")
    solver = new MaxEnt::GIS(tagger_cfg.model, verbose());
  else if(tagger_cfg.model.solver() == "bfgs")
    solver = new MaxEnt::BFGS(tagger_cfg.model, verbose());
  else if(tagger_cfg.model.solver() == "perceptron")
    solver = new MaxEnt::Perceptron(tagger_cfg.model, verbose());
  else
    assert(!"unknown MaxEnt solver should have been caught!");

  try {
		solver->init();
    // Adjust this to produce some useful profiling info (as per lib/tree/gis.cc)
//     if(NLP::Cluster::rank == 0)
//       solver->profile();
    solver->iterate();
    if(NLP::Cluster::rank == 0)
      solver->save(PREFACE.str());
    // Adjust this to produce useful stats at the end (as per lib/tree/gis.cc)
//     solver->stats();
  }catch(NLP::IOException e){
    if(NLP::Cluster::rank == 0){
      cerr << "maxent.ioexception[" << NLP::Cluster::rank << "]:" << e.msg << endl;
      cerr << "  in location " << e.uri << ':' << e.line << endl;
    }
  }catch(NLP::Exception e){
    if(NLP::Cluster::rank == 0)
      cerr << "maxent.exception[" << NLP::Cluster::rank << "]: " << e.msg << endl;
  }

  NLP::Cluster::finalize();

  return 0;
}
