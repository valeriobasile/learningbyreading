// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "std.h"

using namespace std;

#include "utils.h"
#include "except.h"
#include "timer.h"

#include "tree/options.h"
#include "tree/feature.h"
#include "tree/node.h"
#include "tree/forest.h"
#include "tree/gis.h"
#include "tree/bfgs.h"
#include "tree/perceptron.h"

#include <sys/types.h>
#include <unistd.h>

int rank = 1;
int size = 1;

using namespace NLP::Tree;
namespace Cluster = NLP::Cluster;

int
main(int argc, char* argv[]){
  Cluster::init(argc, argv, true);
  
  ostringstream out;
  out << "tree_gis " << Cluster::rank << " of " << Cluster::size;
  out << " on " << Cluster::processor << " pid " << getpid() << '\n';
  cerr << out.str() << flush;

  if(argc != 2){
    cerr << "expected 1 argument <control>" << endl;
    return 1;
  }

  try {
    BFGS gis(argv[1]);
    if(Cluster::rank == 0)
      gis.profile();
    gis.iterate();
    if(Cluster::rank == 0)
      gis.save();
    gis.stats();
  }catch(NLP::IOException e){
    if(Cluster::rank == 0){
      cerr << "maxent.ioexception[" << Cluster::rank << "]:" << e.msg << endl;
      cerr << "  in location " << e.uri << ':' << e.line << endl;
    }
  }catch(NLP::Exception e){
    if(Cluster::rank == 0)
      cerr << "maxent.exception[" << Cluster::rank << "]: " << e.msg << endl;
  }

  Cluster::finalize();

  return 0;
}
