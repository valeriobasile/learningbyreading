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

#include "exception.h"
#include "tree/options.h"
#include "tree/feature.h"
#include "tree/node.h"
#include "tree/forest.h"

namespace NLP { namespace Tree {

//sc: added an additional argument - this gets used twice now for
//different gold feature vectors
void
Forest::_read_features(std::istream &in, Features &features, vector<Feature *> &gold_feats){
  //SC: sometimes the correct features are calculated from the forests
  //themselves, in which case this code isn't used
  ulong nfeatures;

  assert(in >> nfeatures);
  gold_feats.reserve(nfeatures);
  bool negid = 0;

  //  cout << "feats: ";

  for(ulong i = 0; i < nfeatures; ++i){
    long id;
    assert(in >> id);// && id < features.size());

    //cout << i+1 << ' ' << id << ' ';

    //sc: if id is -1 for the gold rules it means we haven't got that
    //feature, so just have a zero sized vector
    if(id == -1)
      negid = 1;

    if(negid == 0){
      assert(id >= 0);
      gold_feats.push_back(&features[id]);
    }
  }
  //  cout << '\n';

  if(negid == 1)
    gold_feats.resize(0);
}

//sc: added this function
bool
Forest::calc_emp(Features &feats){
  for(DisjNode *node = roots; node != end; ++node){
    if(node->inside == ncorrect)
      return node->calc_emp(correct, feats);
  }
  return false;
}

//sc: added this function
void
Forest::mark_correct(void){
  //sc: max_active is used to keep track of correct root disj nodes
  for(DisjNode *node = roots; node != end; ++node){
    node->max_active = 0;
    if(node->inside == ncorrect){
      node->max_active = 1;
      node->mark_correct();
    }
  }
}

void
Forest::feat_counts(){
  for(vector<Feature *>::iterator i = correct.begin(); i != correct.end(); ++i)
    (*i)->emp++;
}

void
Forest::_read_disjnode(std::istream &in, Features &features, DisjNode *disj, DisjNode *nodes){
  ulong nconj;

  assert(in >> nconj);
  disj->begin = new ConjNode[nconj];
  disj->end = disj->begin + nconj;

  assert(in.get() == '\n');
  for(ulong conj = 0; conj < nconj; ++conj)
    _read_conjnode(in, features, disj->begin + conj, nodes, conj == 0);
  disj->inside = 0;
}

void
Forest::_read_conjnode(std::istream &in, Features &features,
                       ConjNode *conj, DisjNode *nodes, bool first){
  ulong id;
  int count;

  char type;
  assert(in >> type);
  switch(type){
    case '0': /* lexical node */
      conj->left = 0;
      conj->right = 0;
      conj->inside = 0;
      break;
    case '1': /* unary rule node */
      //      assert(in >> id && id < ndisj);
      if(!(in >> id && id < ndisj))
	throw NLP::Exception("not(in >> id && id < ndisj) in forest.cc\n");
      conj->left = nodes + id;
      conj->right = 0;
      conj->inside = 0;
      break;
    case '2': /* binary rule node */
      //      assert(in >> id && id < ndisj);
      if(!(in >> id && id < ndisj))
	throw NLP::Exception("not(in >> id && id < ndisj) for left 2 in forest.cc\n");
      conj->left = nodes + id;
      //      assert(in >> id && id < ndisj);
      if(!(in >> id && id < ndisj))
	throw NLP::Exception("not(in >> id && id < ndisj) for right 2 in forest.cc\n");
      conj->right = nodes + id;
      if(!(in >> count))
	throw NLP::Exception("can't read count in forest.cc\n");
      conj->inside = count;
      break;
    case '3': /* root node */
      //      assert(in >> id && id < ndisj);
      if(!(in >> id && id < ndisj))
	throw NLP::Exception("not(in >> id && id < ndisj) for left 3 in forest.cc\n");
      conj->left = nodes + id;
      //      assert(in >> id && id < ndisj);
      if(!(in >> id && id < ndisj))
	throw NLP::Exception("not(in >> id && id < ndisj) for right 3 in forest.cc\n");
      conj->right = nodes + id;
      if(first)
        roots--;
      //      assert(in >> count);
      if(!(in >> count))
	throw NLP::Exception("not(in >> count) in forest.cc\n");
      conj->inside = count;
      break;
    default:
      //      assert(!"unexpected disjunctive node type");
      throw NLP::Exception("unexpected disjunctive node type in forest.cc\n");
  };

  ulong nfeatures;
  //  assert(in >> nfeatures);
  if(!(in >> nfeatures))
    throw NLP::Exception("not(in >> nfeatures) in forest.cc\n");

  if(nfeatures){
    conj->begin = new Feature *[nfeatures];
    conj->end = conj->begin + nfeatures;
  }else{
    conj->begin = 0;
    conj->end = 0;
  }

  ulong feature;
  for(ulong i = 0; i < nfeatures; ++i){
    if(!(in >> feature && feature < features.size()))
      throw NLP::Exception("feature greater than features.size\n");
    //    assert(in >> feature && feature < features.size());
    conj->begin[i] = &features[feature];
  }

  //  assert(in.get() == '\n');
  if(!(in.get() == '\n'))
    throw NLP::Exception("not(in.get() == '\n')\n");
}

Forest::Forest(std::istream &in, Features &features){
  in >> ncorrect;

  //SC: we sometimes calculate these using calc_emp()
  _read_features(in, features, correct);

  //sc: reading gold deriv features in now, too
  _read_features(in, features, gold_deriv);

  in >> ndisj;

  // SC: wrote this check to see if any of the forests were getting
  // really huge - they weren't (biggest around 60,000 nodes)

  //  if(ndisj > 20000)
  //cout << "ndisj: " << ndisj << endl;

  begin = new DisjNode[ndisj];
  end = begin + ndisj;
  roots = end;

  for(ulong disj = 0; disj < ndisj; ++disj){
    ulong id;

    assert(in >> id);
    if(id != disj)
      throw NLP::Exception("disjunctive id does not match expected value");
    _read_disjnode(in, features, begin + disj, begin);
  }
}

Forest::~Forest(void){ delete [] begin; }

ulong
Forest::max_active(void) {
  for(DisjNode *node = begin; node != end; ++node)
    node->calc_active();

  ulong max_active = 0;
  for(DisjNode *node = roots; node != end; ++node)
    max_active = max(max_active, node->max_active);
  
  return max_active;
}

double
Forest::inside(bool only_correct){
  for(DisjNode *node = begin; node != end; ++node)
    node->calc_inside(only_correct);

  DisjNode *node = roots;

  if(!only_correct){
    logZ = node->inside;
    for(++node; node != end; ++node){
      logZ = add_logs(logZ, node->inside);

      //    cout << "root inside: " << node->inside << " logZ: " << logZ << endl;
    }
    return logZ;
  }
  else{
    logZ_correct = 0.0;
    for( ; node != end; ++node){
      if(node->max_active == 1){
	if(logZ_correct == 0.0)
	  logZ_correct = node->inside;
	else
	  logZ_correct = add_logs(logZ_correct, node->inside);
      }
    }
    return logZ_correct;
  }
}

void Forest::outside(double invZ, bool only_correct){
  // initialise disjunctive nodes - root nodes with 1, others with 0
  for(DisjNode *node = roots; node != end; ++node)
    node->outside = 0.0;

  for(DisjNode *node = begin; node != roots; ++node)
    node->outside = 0.0;

  // iterating backwards over the disj nodes should ensure all parents
  // are dealt with before any children
  DisjNode *const rend = begin - 1;
  for(DisjNode *node = end - 1; node != rend; --node)
    node->calc_outside(invZ, only_correct);
}

void
Forest::profile(std::ostream &out){
  ulong ndisj = end - begin;
  ulong nconj = 0;
  ulong nfeatures = 0;

  for(DisjNode *node = begin; node != end; ++node){
    nconj += node->nconj();
    nfeatures += node->nfeatures();
  }
  out << "disjnode    " << ndisj << " x " << sizeof(DisjNode) << " = " << ndisj*sizeof(DisjNode) << endl;
  out << "conjnode    " << nconj << " x " << sizeof(ConjNode) << " = " << nconj*sizeof(ConjNode) << endl;
  out << "feature ptr " << nfeatures << " x " << sizeof(Feature *) << " = " << nfeatures*sizeof(Feature *) << endl;
}

//sc: added this to check if the gold derivation is in the forest
bool
Forest::check_deriv(Features &features){
  // gold_deriv is zero when there's a rule without a feature (so we
  // can't check for that rule)
  if(gold_deriv.size() == 0)
    return false;

  vector<Feature *>::iterator missing = gold_deriv.begin();

  for(DisjNode *node = roots; node != end; ++node){
    if(node->check_deriv(gold_deriv, gold_deriv.begin(), missing))
      return true;
  }

  return false;
}

double
Forest::count_correct(void){
  //  for(DisjNode *node = begin; node != end; ++node)
  //node->reset();

  for(DisjNode *node = begin; node != end; ++node)
    node->calc_correct();

  //  cout << "correct: " << ncorrect << endl;

  log_correct = -1.0;
  //SC: added this for debugging purposes
  double tmp_max = -1.0;
  for(DisjNode *node = roots; node != end; ++node){
    tmp_max = max(tmp_max, node->inside);

    if(node->inside == ncorrect){

      //  cout << "made it, outside: " << node->outside << endl;

      if(log_correct == -1.0)
	log_correct = node->outside;
      else
	log_correct = add_logs(log_correct, node->outside);
    }
  }
  //cout << "max. no. of correct deps: " << tmp_max << endl;

  return log_correct;
}

//sc: (23/9/03) this doesn't get used any more

void
Forest::count_features(void){
  //  inside();
  //outside(0.0);
}

double
Forest::llhood(bool norm_form){
  double sum = 0.0;

  if(norm_form){
    for(vector<Feature *>::iterator i = correct.begin(); i != correct.end(); ++i){
      sum += (*i)->lambda;
      //    cout << "weights sum: " << (*i)->lambda << " " << sum << endl;
    }
    //  cout << "forest likelihood: sum - logZ = " << sum << " - " << logZ << " = " << sum - logZ << endl;
    
    //sc: this one works for the normal form parsing too
    return sum - logZ;
    //return sum - logZ + log_correct;
  }
  else
    return logZ_correct - logZ;
}

//sc: added this function for the perceptron
DisjNode *Forest::viterbi(void){
  // outside DisjNode field is used to ensure we only visit a node once
  for(DisjNode *node = begin; node != end; ++node)
    node->outside = 0.0;

  DisjNode *max_root = 0;
  double max_score = -numeric_limits<double>::max();
  for(DisjNode *node = roots; node != end; ++node){
    double score = node->viterbi();
    if(score > max_score){
      max_score = score;
      max_root = node;
    }
  }
  
  return max_root;
}

void
Forest::perc_update(DisjNode *max_root, Features &features, ulong ninstances){
  for(vector<Feature *>::iterator i = correct.begin(); i != correct.end(); ++i){
    //    cout << *i - features.begin() << ' ' << (*i)->lambda;
    //    cout << (*i)->lambda << '\n';
    (*i)->lambda++;

    // Daume's version:
    //(*i)->est = (*i)->est + ninstances;
    //cout << ' ' << (*i)->lambda << '\n';
  }
  //cout << '\n';

  max_root->perc_update(features, ninstances);

  //cout << '\n';
}

} }
