// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <cassert>
#include <cmath>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>
#include <limits>
#include <valarray>

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

int
main(int argc, char* argv[]){
  if(argc != 2){
    cerr << "expected 1 argument <control>" << endl;
    return 1;
  }

  try {
    Perceptron perceptron(argv[1]);
    perceptron.profile();
    perceptron.iterate();
    perceptron.save();
  }catch(NLP::IOException e){
    cerr << "maxent.ioexception:" << e.msg << endl;
    cerr << "  in location " << e.uri << ':' << e.line << endl;
  }catch(NLP::Exception e){
    cerr << "maxent.exception: " << e.msg << endl;
  }

  return 0;
}
