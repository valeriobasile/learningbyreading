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

#include <limits>
#include <set>

#include "cluster.h"

namespace NLP {
  namespace Tree {

    inline double add_logs(double x, double y){
      if(y <= x)
	return x + log1p(exp(y - x));
      else
	return y + log1p(exp(x - y));
    }

    class DisjNode;

    class ConjNode {
    public:
      DisjNode *left;
      DisjNode *right;

      Feature **begin;
      Feature **end;

      double inside;
      double tmp;

      ConjNode(void) {};
      ~ConjNode(void) { delete [] begin; };

      ulong calc_active(void);
      bool calc_emp(vector<Feature *> &correct, Features &feats);
      void mark_correct(void);
      double calc_inside(void);
      void calc_outside(double outside, double invZ, bool only_correct);

      double calc_correct(void);
      ulong check_deriv(vector<Feature *> &gold_deriv, vector<Feature *>::iterator iter,
			vector<Feature *>::iterator &missing);

      double viterbi(void);
      bool perc_update(Features &features, ulong ninstances);

      ulong ndisj(void) const {
        if(left){
          return right ? 2 : 1;
        }else
          return 0;
      }
      ulong nfeatures(void) const { return end - begin; }
    };

    class DisjNode {
    public:
      ConjNode *begin;
      ConjNode *end;
      ulong max_active;

      double inside;
      double outside;  

      DisjNode(void) { max_active = 0; }
      ~DisjNode(void) { delete [] begin; };

      void calc_active(void){
        max_active = 0;
        for(ConjNode *node = begin; node != end; ++node)
          max_active = max(max_active, node->calc_active());

      };

      void calc_inside(bool only_correct){
        ConjNode *node = begin;

	if(!only_correct){
	  inside = node->calc_inside();
	  for(++node; node != end; ++node)
	    inside = add_logs(inside, node->calc_inside());
	}
	else{
	  inside = 0.0;
	  for( ; node != end; ++node){
	    if(node->tmp == -2.0){
	      if(inside == 0.0)
		inside = node->calc_inside();
	      else
		inside = add_logs(inside, node->calc_inside());
	    }
	  }
	}
      };

      void calc_outside(double invZ, bool only_correct){
        for(ConjNode *node = begin; node != end; ++node)
	  if(!only_correct || node->tmp == -2.0)
	    node->calc_outside(outside, invZ, only_correct);
      };

      void reset(void){
	for(ConjNode *node = begin; node != end; ++node){
	  //	  node->inside = 10000000000;
	  node->tmp = 1000000000;
	}
	//	inside = 100000000;
	outside = 100000000;
      }

      //sc: added this
      bool calc_emp(vector<Feature *> &correct, Features &feats){
	ConjNode *max_node = 0;
	double max_deps = -numeric_limits<double>::max();
	for(ConjNode *node = begin; node != end; ++node){
	  // inside stores the max number of cumulative correct deps
	  if(node->inside > max_deps){
	    max_deps = node->inside;
	    max_node = node;
	  }
	}
	if(max_deps != -1.0)
	  return max_node->calc_emp(correct, feats);
	
	return false;
      }
      
      //sc: added this
      void mark_correct(void){
	double max_deps = -numeric_limits<double>::max();
	for(ConjNode *node = begin; node != end; ++node){
	  // inside stores the max number of cumulative correct deps
	  if(node->inside > max_deps)
	    max_deps = node->inside;
	}
	if(max_deps != -1.0){
	  for(ConjNode *node = begin; node != end; ++node){
	  // inside stores the max number of cumulative correct deps
	    if(node->inside == max_deps){
	      //sc: use max_active to keep track of correct conj nodes
	      node->mark_correct();
	    }
	  }
	}
      }
      
      ulong check_deriv(vector<Feature *> &gold_deriv, vector<Feature *>::iterator iter,
			vector<Feature *>::iterator &missing){
        // TODO: are the next 5 lines needed?  max_active stores the
	// position along the gold_deriv vector sc 8/1/06: I think we
	// can comment out the next line; just means that we've
	// already been there if max_active, hence no need to
	// calculate it again
	max_active = 0;
	if(max_active){
	  cout << "1st max_active: " << max_active << endl;
	  return max_active;
	}

	for(ConjNode *node = begin; node != end; ++node){
	  if((max_active = node->check_deriv(gold_deriv, iter, missing)))
	    return max_active;
	}

	return false;
      };

      double viterbi(void){
	if(outside != 0.0)
	  return inside;

	double max_score = -numeric_limits<double>::max();
	double score;
	ConjNode *max_node = 0;

	for(ConjNode *node = begin; node != end; ++node){
	  // use the inside field to record max node
	  node->inside = 0.0;
	  score = node->viterbi();
	  if(score > max_score){
	    max_score = score;
	    max_node = node;
	  }
	}
	max_node->inside = -1.0;

	outside = 1.0;
	inside = max_score;

	return max_score;
      }

      bool perc_update(Features &features, ulong ninstances){
	for(ConjNode *node = begin; node != end; ++node)
	  if(node->inside == -1.0)
	    return node->perc_update(features, ninstances);
	return false;
      }

      void calc_correct(void){
	double max_deps = -1.0;
	for(ConjNode *node = begin; node != end; ++node){
	  double ndeps = node->calc_correct();
	  if(ndeps > max_deps)
	    max_deps = ndeps;
	}

	inside = max_deps;
        outside = -1.0;

	if(max_deps == -1.0)
	  return;

	for(ConjNode *node = begin; node != end; ++node)
	  if(node->inside == max_deps){
	    if(outside == -1.0)
	      outside = node->tmp;
	    else
	      outside = add_logs(outside, node->tmp);
	  }

	//	cout << "disj inside: " << inside << " outside " << outside << endl;

      };

      size_t nconj(void) const { return end - begin; };
      size_t nfeatures(void) const {
        ulong nfeatures = 0;
        for(ConjNode *node = begin; node != end; ++node)
          nfeatures += node->nfeatures();
        return nfeatures;
      };
    };

    //sc: added this for the perceptron
    inline double
      ConjNode::viterbi(void){
      double score = 0;

      for(Feature **i = begin; i != end; ++i)
	score += (*i)->lambda;

      if(left){
	if(right)
	  return left->viterbi() + right->viterbi() + score;
	else
	  return left->viterbi() + score;
      }
      return score;
    }

    inline bool
    ConjNode::perc_update(Features &features, ulong ninstances){
      for(Feature **i = begin; i != end; ++i){
	(*i)->lambda--;

	// Daume's version:
	// (*i)->est = (*i)->est - ninstances;
	// cout << ' ' << (*i)->lambda << '\n';	
      }

      if(left){
	if(right)
	  return left->perc_update(features, ninstances) && right->perc_update(features, ninstances);
	else
	  return left->perc_update(features, ninstances);
      }
      return true;
    }

    inline ulong
    ConjNode::calc_active(void){
      ulong active = end - begin;
      if(left){
        active += left->max_active;
        if(right)
          active += right->max_active;
      }

      return active;
    }

    inline bool
    ConjNode::calc_emp(vector<Feature *> &correct, Features &feats){  
      for(Feature **i = begin; i != end; ++i){
	(*i)->emp++;
	correct.push_back(*i);
      }
      if(left){
	if(right)
	  return left->calc_emp(correct, feats) && right->calc_emp(correct, feats);
	else
	  return left->calc_emp(correct, feats);
      }
      return true;
    }

    inline void
    ConjNode::mark_correct(){  
      if(left){
	if(right && right->outside != -1.0){
	  //outside == -1.0 now means we've already been there
	  right->outside = -1.0;
	  right->mark_correct();
	}
	if(left->outside != -1.0){
	  left->outside = -1.0;
	  left->mark_correct();
	}
      }
      //sc: tmp == -2.0 is used to mark a correct derivation
      tmp = -2.0;
    }
    
    inline ulong
    ConjNode::check_deriv(vector<Feature *> &gold_deriv, vector<Feature *>::iterator iter,
			  vector<Feature *>::iterator &missing){
      if(!left){
	//	cout << "at leaf: " << iter - gold_deriv.begin() << endl;
	return iter - gold_deriv.begin();
      }

      bool seenf = 0;
      for(Feature **i = begin; i != end; ++i){
	if(*i == *iter){
	  seenf = 1;
	  break;
	}
      }

      if(!seenf){
	missing = max(missing, iter);
	return 0;
      }

      ulong pos = left->check_deriv(gold_deriv, iter + 1, missing);
      if(!pos)
	return 0;

      if(right)
	return right->check_deriv(gold_deriv, gold_deriv.begin() + pos, missing);
      else
	return pos;
    }

    inline double
    ConjNode::calc_inside(void){
      if(left){
         if(right)
           inside = left->inside + right->inside;
         else
           inside = left->inside;
      }else
         inside = 0.0;

      for(Feature **i = begin; i != end; ++i)
        inside += (*i)->lambda;

      return inside;
   }

   inline double
   ConjNode::calc_correct(void){
     if(left){
       if(left->inside == -1.0){
	 inside = -1.0;
	 tmp = 0.0;
	 return -1.0;
       }

       if(right){
	 if(right->inside == -1.0){
	   inside = -1.0;
	   tmp = 0.0;
	   return -1.0;
	 }

	 
	 //	 cout << "getting inside from left and right: " <<  left->inside << " " << right->inside << endl;


	 inside += left->inside + right->inside;
	 tmp = left->outside + right->outside;
       }else{

	 //	 cout << "getting inside from left: " << left->inside << endl;

	 inside += left->inside;
	 tmp = left->outside;
       }
     }else
       tmp = 0.0;


     //     cout << "final conj inside, outside: " << inside << " " << tmp << endl;


     return inside;
   }

   inline void
   ConjNode::calc_outside(double outside, double invZ, bool only_correct){
     double sum = 0.0;
     for(Feature **i = begin; i != end; ++i)
       sum += (*i)->lambda;
     sum += outside;

     // TODO (sc 18/05/05)
     // we shouldn't be using left->outside as a marker here, since in theory left->outside
     // could be zero from an earlier conj node passing down zero mass (although this must
     // be unlikely in practice). This bug is fixed in parser.cc using the is_visited() fn.

     if(left){
       if(right){
         if(left->outside)
           left->outside = add_logs(left->outside, right->inside + sum);
         else
           left->outside = right->inside + sum;

         if(right->outside)
           right->outside = add_logs(right->outside, left->inside + sum);
         else
           right->outside = left->inside + sum;
       }else{
         if(left->outside)
           left->outside = add_logs(left->outside, sum);
         else
           left->outside = sum;
       }
     }

     double p = exp(outside + inside + invZ);
     for(Feature **i = begin; i != end; ++i){
       if(!only_correct)
	 (*i)->est += p;
       else
	 (*i)->emp += p;
     }
  };

} }

