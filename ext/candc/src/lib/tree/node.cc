// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <cctype>
#include <cmath>
#include <string>
#include <vector>
#include <iostream>
#include <iomanip>
#include <limits>

using namespace std;

#include "utils.h"
#include "exception.h"
#include "pool.h"

#include "tree/feature.h"
#include "tree/node.h"

namespace NLP { namespace Tree {

double
DisjNode::viterbi(void){
  if(outside != 0.0)
    return inside;

  double max_score = -numeric_limits<double>::max();
  double score;
  ConjNode *node max_node;

  for(ConjNode *node = begin; node != end; ++node){
    // use the inside field to record max node
    node->inside = 0.0;
    score = node->viterbi();
    if(score > max_score){
      max_score = score;
      max_node = node;
    }
    max_node->inside = -1.0;
  }

  outside = 1.0;
  inside = max_score;
  return max_score;
}

} }
