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

#include "timer.h"

#include "input.h"

#include "except.h"
#include "tree/options.h"
#include "tree/feature.h"
#include "tree/node.h"
#include "tree/forest.h"
#include "tree/gis.h"
#include "tree/perceptron.h"

namespace NLP { namespace Tree {

Perceptron::Perceptron(std::string filename): GIS(filename, false) {
}

Perceptron::~Perceptron(void){
}

void
Perceptron::_save_weights(long iteration){
  ostringstream s;

  s << op.model << "/weights" << iteration;

  string filename = s.str();
  ofstream out(filename.c_str());
  if(!out)
    throw NLP::IOException("could not open weights file for writing", filename);

  for(Features::iterator feature = features.begin(); feature != features.end(); ++feature)
    out << "current: " << feature->lambda << " total: " << feature->est
        << " nforests: " << nforests
        << " average: " << feature->est / ((iteration + 1) * nforests) << '\n';
}

void 
Perceptron::iterate(void){
  const string filename = op.base + ".out";
  nforests = 0;
  ulong ninstances = 1;
  ulong nlines = 0;

  for(ulong iteration = 0; iteration < op.niterations; ++iteration){
    if(Cluster::rank == 0)
      cout << "iteration: " << iteration << '\n' << flush;

    ifstream in(filename.c_str());
    if(!in)
      throw NLP::IOException("could not open forests file for reading", filename);

    comment += read_preface(filename, in, nlines);

    // only consider forest if there is at least 1 correct derivation
    ulong sentence;
    while(in >> sentence){
      Forest *forest = new Forest(in, features);
      if(!forest->check_deriv(features)){
        delete forest;
        continue;
      }

      if(sentence % 1000 == 0)
        cout << "processing forest " << sentence << endl;

      if(iteration == 0)
        nforests++;

      DisjNode *max_root = forest->viterbi();
      // this now does the accumulated averaged parameters also (if using Daume's method):
      forest->perc_update(max_root, features, ninstances);

      ninstances++;

      // naive version:
      for(ulong i = 0;  i < features.size(); ++i){
        // feature.est stores the cummulative lambda value
        features[i].est += features[i].lambda;
      }

      /*
        if(sentence % 1000 == 0){
        cout << "saving weights\n";
        _save_weights(iteration);
        }
      */

      delete forest;
    }
 
    if(!in.eof())
      throw NLP::IOException("error reading in forest", filename);
    
    cout << "saving weights\n";
    _save_weights(iteration);
  }
}

void
Perceptron::save(void){
  const string filename = op.model + "/weights";
  ofstream out(filename.c_str());
  if(!out)
    throw NLP::IOException("could not open weights file for writing", filename);

  // from p.19 of Daume's thesis
  for(Features::iterator feature = features.begin(); feature != features.end(); ++feature)
    // out << feature->lambda - feature->est / (op.niterations * nforests + 1) << '\n';
    // naive version:
    out << feature->est / (op.niterations * nforests) << '\n';
}

} }
