// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::MaxEnt::GIS
// Generalised Iterative Scaling implementation

#include <cassert>
#include <cmath>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include <sstream>

#include "base.h"

#include "cluster.h"

#include "prob.h"

#include "config/config.h"

#include "model/model.h"

#include "maxent/feature.h"
#include "maxent/attribute.h"
#include "maxent/context.h"
#include "maxent/gis.h"

#include  <cstring>

using namespace std;

namespace NLP { namespace MaxEnt {

void
GIS::_read_features(void){
  // initial lambda value
  // 1 for sum form; e for product form
  double initial = 1.0;

  // it is important to get the capacity of the features and
  // attributes vectors correct beforehand otherwise they will
  // need to be reallocated and this will invalidate the existing
  // pointers into the vectors
  tfeatures = 0;
  features.reserve(info.nfeatures());
  attributes.reserve(info.nattributes() + 1);

  ulong nlines = 0;
  ifstream stream(model.features().c_str());
  if(!stream)
    throw IOException("could not open features file", model.features());

  read_preface(model.features(), stream, nlines);

  if(Cluster::USE_MPI && Cluster::rank == 0)
    cout << "0 reading " << model.features() << endl << flush;

  ulong previous = static_cast<ulong>(-1);
  ulong klass, attribute, freq;
  while(stream >> klass >> attribute >> freq){
    ++nlines;

    features.push_back(Feature(initial, klass, freq));
    tfeatures += freq;

    // have we reached the end of an attribute
    // if so, push another pointer to the end of the attributes
    // array for later user in _read_contexts
    if(attribute != previous){
      attributes.push_back(&features.back());
      previous = attribute;
    }
  }

  // Create space for MPI storage of feature estimates
  if (Cluster::USE_MPI){
    local_est = new double[info.nfeatures()];
    global_est = new double[info.nfeatures()];
  }else{
    local_est = 0;
    global_est = 0;
  }

  if(!stream.eof())
    throw IOException("could not parse feature", model.features(), nlines);

  if(features.size() != info.nfeatures())
    throw IOException("number of features read != info file value", model.features(), nlines);

  // push a final pointer to mark the end of the features array
  attributes.push_back(&features.back() + 1);
  if(attributes.size() != info.nattributes() + 1)
    throw IOException("number of attributes read != info file value", model.features(), nlines);
}

void
GIS::_read_contexts(void){
  contexts.reserve(info.ncontexts());

  ulong nlines = 0;
  ifstream stream(model.contexts().c_str());
  if(!stream)
    throw IOException("could not open contexts file", model.contexts());

  read_preface(model.contexts(), stream, nlines);

  if(Cluster::USE_MPI && Cluster::rank == 0)
    cout << Cluster::rank << " reading " << model.contexts() << endl << flush;

  long freq;
  ushort klass, nattributes;
  while(stream >> freq >> klass >> nattributes){
    ++nlines;
    Context *context = new (nattributes) Context(freq, klass, nattributes);
    Attribute *attrs = context->begin();
    memset(attrs, 0, sizeof(Attribute *)*nattributes);
    for(ulong i = 0; i < nattributes; ++i){
      ulong id;
      stream >> id;
      if(!stream)
        throw IOException("error reading attribute from contexts file", model.contexts(), nlines);
      // use attributes to translate the id into a pair of Feature pointers
      // stored in the current Attribute object
      attrs[i].begin = attributes[id];
      attrs[i].end = attributes[id + 1];
    }
    if(stream.get() != '\n')
      throw IOException("expected end of line in contexts file", model.contexts(), nlines);

    contexts.push_back(context);
  }

  if(!stream.eof())
    throw IOException("could not parse feature", model.contexts(), nlines);
}

// find the maximum number of active attributes for a given context
// must now use the same loop reordering employed to calculate
// model feature expectations
ulong
GIS::_max_attributes(const Context *context, Actives &nactives){
  zero(nactives);

  const Attribute *const a_begin = context->begin();
  const Attribute *const a_end = context->end();
  for(const Attribute *attr = a_begin; attr != a_end; ++attr){
    const Feature *const begin = attr->begin;
    const Feature *const end = attr->end;
    for(const Feature *feature = begin; feature != end; ++feature)
      nactives[feature->klass]++;
  }

  return *max_element(nactives.begin(), nactives.end());
}

void
GIS::_calc_correction(void){
  Actives nactives(NKLASSES);

  ulong max_active = 0;
  for(Contexts::iterator context = contexts.begin(); context != contexts.end(); ++context)
    max_active = max(max_active, _max_attributes(*context, nactives));

  // For MPI we have to get max active attributes across all nodes
  if (Cluster::USE_MPI)
    Cluster::max(max_active);

  C = max_active;
  invC = 1.0/static_cast<double>(C);
  if((VERBOSE) &&
    (!Cluster::USE_MPI || Cluster::rank == 0))
    cout << "max_active = " << max_active << endl;
}

GIS::GIS(const Model::Model &model, bool verbose)
  : model(model), info(model.info()), NKLASSES(info.nklasses()),
    ALPHA(1/(model.sigma()*model.sigma())), VERBOSE(verbose){

  if((VERBOSE) &&
    (!Cluster::USE_MPI || Cluster::rank == 0))
    cout << "reading features in" << endl;
  _read_features();

  if((VERBOSE) &&
    (!Cluster::USE_MPI || Cluster::rank == 0))
    cout << "reading contexts in" << endl;
  _read_contexts();

  if((VERBOSE) &&
    (!Cluster::USE_MPI || Cluster::rank == 0))
    cout << "calculating correction constant maxC" << endl;
  _calc_correction();
}

GIS::~GIS(void){}

void
GIS::load_weights(const std::string &filename){
  if((VERBOSE) &&
    (!Cluster::USE_MPI || Cluster::rank == 0))
    cout << "reading existing weights from " << filename << endl;

  ulong nlines = 0;
  ifstream stream(filename.c_str());
  if(!stream)
    throw IOException("could not open features file", filename);

  read_preface(filename, stream, nlines);

  ulong nloaded = 0;
  ulong klass, attribute;
  double weight;
  while(stream >> klass >> attribute >> weight){
    ++nlines;
    Feature &f = features[nloaded];
    if(f.klass != klass)
      throw IOException("the weight file is not aligned with the features file", filename, nlines);
    f.lambda = weight;
    ++nloaded;
  }

  if(!stream.eof())
    throw IOException("could not parse feature", filename, nlines);

  if(features.size() != nloaded)
    throw IOException("number of weights read != number of features", filename, nlines);
}

void
GIS::_sum(const Context *context, PDF &p_classes){
  zero(p_classes);

  // get the beginning and end of the Attribute array in the Context
  const Attribute *const a_begin = context->begin();
  const Attribute *const a_end = context->end();
  for(const Attribute *attr = a_begin; attr != a_end; ++attr){
    // for each attribute, sum over the features seen in the training data
    const Feature *const begin = attr->begin;
    const Feature *const end = attr->end;
    for(const Feature *feature = begin; feature != end; ++feature)
      p_classes[feature->klass] += feature->lambda;
  }
}

void
GIS::_normalise(const Context *context, PDF &p_classes){
  double Z = 0.0;
  for(ulong klass = 2; klass != NKLASSES; ++klass){
    p_classes[klass] = exp(p_classes[klass]);
    Z += p_classes[klass];
  }

  double scale = context->emp/Z;
  for(ulong klass = 2; klass != NKLASSES; ++klass)
    p_classes[klass] *= scale;
}

// add the probability mass for the given context
// to each feature's model expectation
void
GIS::_estimate(Context *context, PDF &p_classes){
  Attribute *const a_begin = context->begin();
  Attribute *const a_end = context->end();
  for(const Attribute *attr = a_begin; attr != a_end; ++attr){
    Feature *const begin = attr->begin;
    Feature *const end = attr->end;
    for(Feature *feature = begin; feature != end; ++feature)
      feature->est += p_classes[feature->klass];
  }
}

double
GIS::_distribute(PDF &p_classes){
  double llhood = 0.0;
  for(Contexts::iterator context = contexts.begin(); context != contexts.end(); ++context){
    _sum(*context, p_classes);
    _normalise(*context, p_classes);
    _estimate(*context, p_classes);
    llhood += (*context)->llhood(p_classes);
  }

  // If we are using MPI, find the sum of the objectives
  if(Cluster::USE_MPI)
    Cluster::sum(llhood);

  return llhood;
}

void
GIS::_sum_estimates(void){
  if(!Cluster::USE_MPI)
    return;

  if(VERBOSE && Cluster::rank == 0)
    cout << ", sum" << flush;
      
  // Distribute the feature estimates
  for(ulong i = 0; i < info.nfeatures(); ++i)
    local_est[i] = features[i].est;

  Cluster::sum(local_est, global_est, info.nfeatures());

  // And copy back the results
  for(ulong i = 0; i < info.nfeatures(); ++i)
    features[i].est = global_est[i];
}

// call the update rule on every feature

void
GIS::_update_none(void){
  for(Features::iterator feature = features.begin(); feature != features.end(); ++feature)
    feature->update_sum(invC);
}

void
GIS::_update_gaussian(void){
  for(Features::iterator feature = features.begin(); feature != features.end(); ++feature)
    feature->update_gaussian(C, ALPHA);
}

void
GIS::_update(void){
  if(model.update() == "none")
    _update_none();
  else if(model.update() == "gaussian")
    _update_gaussian();
  else
    assert(!"not a legal update parameter");
}

void
GIS::init(void){ /* do nothing */ }

// run GIS for the number of iterations given in the Options object
void
GIS::iterate(void){
  PDF p_classes(NKLASSES, 0.0);

  for(ulong iteration = 0; iteration < model.niterations(); ++iteration){
    if((VERBOSE) &&
      (!Cluster::USE_MPI || Cluster::rank == 0))
      cout << iteration << ": normalise, estimate" << flush;

    double objective = _distribute(p_classes);

    if((VERBOSE) &&
      (!Cluster::USE_MPI || Cluster::rank == 0))
      cout << ", update" << flush;

    for(Features::iterator i = features.begin(); i != features.end(); ++i)
      objective -= i->penalty_gaussian(ALPHA);

    _sum_estimates();

    _update();

    if((VERBOSE) &&
      (!Cluster::USE_MPI || Cluster::rank == 0))
      cout << ", objective = " << objective << endl;
  }
  
  double objective = _distribute(p_classes);
  if((VERBOSE) && (!Cluster::USE_MPI || Cluster::rank == 0))
    cout << "final objective = " << objective << endl;
}

// perform a single iteration of GIS
bool
GIS::iteration(void){
  PDF p_classes(NKLASSES, 0.0);
  _distribute(p_classes);
  _update();

  return true;
}

// dump the weights out to disk, converting product form weights
// into sum (log) form weights which are used by the taggers
void
GIS::save(const std::string &PREFACE){
  ofstream stream(model.weights().c_str());
  if(!stream)
    throw IOException("could not open weights file for writing", model.weights());

  stream << PREFACE << '\n';
  stream.precision(15);

  for(ulong attr = 0; attr != attributes.size() - 1; ++attr)
    for(Feature* feature = attributes[attr]; feature != attributes[attr + 1]; ++feature)
      stream << feature->klass << ' ' << attr << ' ' << feature->lambda << '\n';
}

// return model statistics as a string
std::string
GIS::to_string(void) const {
  ostringstream s;

  s << "tfeatures = " << tfeatures << endl;
  s << "C = " << C << endl;
  s << "invC = " << invC << endl;

  return s.str();
}

// return statistics about the GIS memory usage as a string
std::string
GIS::profile(void) const {
  ostringstream s;

  ulong total = 0;
  s << "features vector[" << features.capacity() << "] x " << sizeof(features[0])
    << " = " << features.capacity()*sizeof(features[0]) << endl;
  total += features.capacity()*sizeof(features[0]);

  s << "attributes vector[" << attributes.capacity() << "] x " << sizeof(attributes[0])
    << " = " << attributes.capacity()*sizeof(attributes[0]) << endl;
  total += attributes.capacity()*sizeof(attributes[0]);

  s << "contexts vector[" << contexts.capacity() << "] x " << sizeof(contexts[0])
    << " = " << contexts.capacity()*sizeof(contexts[0]) << endl;
  total += contexts.capacity()*sizeof(contexts[0]);

  ulong nbytes = contexts.size()*sizeof(Context);
  for(Contexts::const_iterator i = contexts.begin(); i != contexts.end(); ++i)
    nbytes += (*i)->size()*sizeof(Attribute);

  s << "context objects (" << contexts.size() << " x varied) = " << nbytes << endl;
  total += nbytes;

  s << "total memory = " << total << endl;

  return s.str();
}

} }
