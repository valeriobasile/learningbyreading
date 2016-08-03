// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// Perceptron based model training
// Note that the context and feature fields are used as follows:
// feature, lambda -- the current perceptron weight
// feature, est    -- the total perceptron weight (for averaging)
// feature, emp    -- the last iteration to modify this feature weight
// context, emp    -- the number of times this context was observed

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
#include "maxent/perceptron.h"

namespace NLP { namespace MaxEnt {

Perceptron::Perceptron(const Model::Model &model, bool verbose)
  : GIS(model, verbose), SHUFFLE(model.shuffle_contexts()){
}

Perceptron::~Perceptron(void){
}

void
Perceptron::init(void){
  for(Features::iterator f = features.begin(); f != features.end(); ++f){
    f->emp = 0;
    f->lambda = 0.0;
    f->est = 0.0;
  }
}

void
Perceptron::perceptron_update(Context *context, ushort klass, ulong cur_iter, double delta){
  Attribute *const a_begin = context->begin();
  Attribute *const a_end = context->end();
  for(const Attribute *attr = a_begin; attr != a_end; ++attr){
    Feature *const begin = attr->begin;
    Feature *const end = attr->end;
    for(Feature *feature = begin; feature != end; ++feature){
      if(feature->klass == klass){
        feature->est += (cur_iter - feature->emp)*feature->lambda;
        feature->emp = cur_iter;
        feature->lambda += delta;
        break;
      }
    }
  }
}

void
Perceptron::send(const ulong &errors, const ulong &cur_iter, const ulong &dest){
  std::vector<double> est;
  est.reserve(features.size());
  std::vector<double> lambda;
  lambda.reserve(features.size());
  std::vector<long> emp;
  emp.reserve(features.size());
  for(Features::iterator f = features.begin(); f != features.end(); ++f){
    est.push_back(f->est);
    emp.push_back(f->emp);
    lambda.push_back(f->lambda);
  }
  Cluster::send(est, dest, 0);
  Cluster::send(lambda, dest, 1);
  Cluster::send(emp, dest, 2);
  Cluster::send(errors, dest, 3);
  Cluster::send(cur_iter, dest, 4);
}

void
Perceptron::recv(ulong &errors, ulong &cur_iter, ulong &src){
  std::vector<double> est;
  est.resize(features.size());
  std::vector<double> lambda;
  lambda.resize(features.size());
  std::vector<long> emp;
  emp.resize(features.size());
  Cluster::recv(est, src, 0);
  Cluster::recv(lambda, src, 1);
  Cluster::recv(emp, src, 2);
  Cluster::recv(errors, src, 3);
  Cluster::recv(cur_iter, src, 4);
  for(unsigned int i = 0; i < features.size(); i++){
    features[i].est = est[i];
    features[i].emp = emp[i];
    features[i].lambda = lambda[i];
  }
}

void
Perceptron::determine_order(ulong &first, ulong &last, ulong& src, ulong& dest){
  if(SHUFFLE){
    if(Cluster::rank == 0){
      // Setup
      vector<ulong> node_order;
      node_order.push_back(0);
      for(ulong i = 0; i < Cluster::size; ++i){
        node_order.push_back(i);
      }
      node_order.push_back(0);

      // Shuffle
      vector<ulong>::iterator begin = node_order.begin();
      begin++;
      vector<ulong>::iterator end = node_order.end();
      end--;
      std::random_shuffle(begin, end);

      // Record first and last for future use
      first = node_order[1];
      last = node_order[node_order.size() - 2];

      // Tell each node who precede and follow it
      for(ulong i = 1; i < node_order.size() - 1; ++i){
        if(node_order[i] != 0){
          Cluster::send(node_order[i-1], node_order[i], 5);
          Cluster::send(node_order[i+1], node_order[i], 6);
        }else{
          src = node_order[i-1];
          dest = node_order[i+1];
        }
      }
    }else{
      // Receive who will precede and follow this node
      Cluster::recv(src, 0, 5);
      Cluster::recv(dest, 0, 6);
    }
  }else{
    first = 0;
    last = Cluster::size - 1;
    src = Cluster::rank - 1;
    dest = Cluster::rank + 1;
    if(Cluster::rank == 0){
      src = 0;
    }else if(Cluster::rank == Cluster::size - 1){
      dest = 0;
    }
  }
}

ulong
Perceptron::perceptron_iteration(PDF &p_classes, ulong cur_iter){
  ulong errors = 0;
  
  ulong first = 0;
  ulong last = 0;
  ulong src = 0;
  ulong dest = 0;
  if(Cluster::USE_MPI && Cluster::size > 1)
    determine_order(first, last, src, dest);

  // 0 sends to the first node
  if(Cluster::USE_MPI && Cluster::rank == 0 && first != 0){
    if(VERBOSE)
      cout << "Processing contexts:" << endl;
    send(errors, cur_iter, first);
  }

  // Wait to receive from src
  if(Cluster::USE_MPI && Cluster::rank != src)
    recv(errors, cur_iter, src);

  // Process the local contexts
  if(VERBOSE && Cluster::USE_MPI)
      cout << ' ' << Cluster::rank;
  for(Contexts::iterator i = contexts.begin(); i != contexts.end(); ++i){
    Context *context = *i;

    _sum(context, p_classes);
    ushort predicted = max_element(p_classes.begin(), p_classes.end()) - p_classes.begin();
    if(predicted != context->klass){
      ++errors;
      double delta = 1.0;
      if(model.one_per_context())
        delta = context->emp;
      perceptron_update(context, predicted, cur_iter, -delta);
      perceptron_update(context, context->klass, cur_iter, delta);
    }

    cur_iter += context->emp;
  }

  // Send to the follower
  if(Cluster::USE_MPI && Cluster::rank != dest)
    send(errors, cur_iter, dest);
  
  // Return results to 0
  if(Cluster::USE_MPI && Cluster::rank == 0 && last != 0)
    recv(errors, cur_iter, last);

  if(VERBOSE && Cluster::USE_MPI && Cluster::rank == 0)
    cout << endl;

  return errors;
}

void
Perceptron::perceptron_finalise(ulong nupdates){
  for(Features::iterator f = features.begin(); f != features.end(); ++f){
    f->est += (nupdates - f->emp)*f->lambda;
    f->lambda = f->est/nupdates;
  }
}

void
Perceptron::perceptron_dump(ulong cur_iter){
  ostringstream filename;
  filename << model.weights() << cur_iter;

  ofstream stream(filename.str().c_str());
  if(!stream)
    throw IOException("Perceptron could not open weights file for writing", filename.str());

  stream << "# Iteration " << cur_iter << " dump\n" << "\n";
  
  double steps = cur_iter*info.nevents();
  for(ulong attr = 0; attr != attributes.size() - 1; ++attr)
    for(Feature *f = attributes[attr]; f != attributes[attr + 1]; ++f)
      stream << f->klass << ' ' << attr << ' ' << (f->est + (steps - f->emp)*f->lambda)/steps << '\n';
}

void
Perceptron::iterate(void){
  PDF p_classes(NKLASSES, 0.0);

  for(ulong iteration = 0; iteration < model.niterations(); ++iteration){
    if((VERBOSE) && (!Cluster::USE_MPI || Cluster::rank == 0))
      cout << "Iteration " << iteration << " of " << model.niterations() << ':' << endl;

    ulong objective = perceptron_iteration(p_classes, iteration*info.nevents());
    if((VERBOSE) && (!Cluster::USE_MPI || Cluster::rank == 0))
      cout << " nerrors = " << objective << endl;

///    if(!Cluster::USE_MPI || Cluster::rank == 0)
///      perceptron_dump(iteration + 1);
    if(SHUFFLE)
      std::random_shuffle(contexts.begin(), contexts.end());
  }

  perceptron_finalise(model.niterations()*info.nevents());
}

} }
