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


namespace NLP {
  namespace Tree {

    class Forest {
    protected:
      void _read_features(std::istream &in, Features &features, vector<Feature *> &gold_feats);
      void _read_disjnode(std::istream &in, Features &features, DisjNode *disj, DisjNode *nodes);
      void _read_conjnode(std::istream &in, Features &features,
                          ConjNode *conj, DisjNode *nodes, bool first);

      void _update(void);
      void _update_gaussian(void);
      void _update_gamma(void);
    public:
      DisjNode *begin;
      DisjNode *roots;
      DisjNode *end;

      ulong ndisj;		// number of disjunctive nodes
      ulong tfeatures; 		// total number of features (tokens)
      ulong ncorrect;           // number of gold standard dependencies

      // list of correct features for the Gold standard dep-struct
      vector<Feature *> correct;

      //sc: added this so can find correct derivation
      vector<Feature *> gold_deriv;

      Forest(std::istream &in, Features &features);
      ~Forest(void);

      bool calc_emp(Features &feats);
      void mark_correct(void);
      bool check_deriv(Features &features);
      void feat_counts();
      void profile(std::ostream &out);
      void iterate(void);
      void save(void);

      double logZ;
      double logZ_correct;
      double log_total;
      double log_correct;

      ulong max_active(void);
      double inside(bool only_correct);
      void outside(double invZ, bool only_correct);

      void count_features(void);
      double count_total(void);
      double count_correct(void);

      double llhood(bool norm_form);

      DisjNode* viterbi(void);
      void perc_update(DisjNode *max_root, Features &features, ulong ninstances);
    };

    typedef std::vector<Forest *> Forests;
  }
}
