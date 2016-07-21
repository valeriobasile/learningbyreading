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

#include "base.h"

#include "config/config.h"

#include "pool.h"

#include "parser/parser.h"

#include "parser/variable.h"
#include "parser/dependency.h"
#include "parser/distance.h"
#include "parser/filled.h"
#include "parser/supercat.h"
#include "parser/unify.h"
#include "parser/rule.h"
#include "parser/cell.h"
#include "parser/equiv.h"
#include "parser/treebank.h"
#include "parser/chart.h"
#include "parser/rule_instances.h"
#include "tree/attributes.h"
#include "parser/depscore.h"
#include "parser/feature_type.h"
#include "parser/feature_dist_type.h"
#include "parser/feature_cat.h"
#include "parser/feature_rule.h"
#include "parser/feature_rule_head.h"
#include "parser/feature_rule_dep.h"
#include "parser/feature_rule_dep_dist.h"
#include "parser/feature_dep.h"
#include "parser/feature_dep_dist.h"
#include "parser/feature_genrule.h"
#include "parser/inside_outside.h"

namespace NLP {
  namespace CCG {

    inline double add_logs(double x, double y){
      if(y <= x)
	return x + log1p(exp(y - x));
      else
	return y + log1p(exp(x - y));
    }

    using namespace NLP::Tree;

    class InsideOutside;
  
    class Parser::_Impl {
    protected:
      void _count_rule(const SuperCat *sc, const Words &words, const Words &tags, const Type type);
      void _count_rule_dep(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var1,
                           const Variable *var2, const Words &words1, const Words &words2, const Type type);
      void _count_rule_head(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var,
                            const Words &words, const Words &tags, const Type type);
      
      void _load_features(const std::string &filename);
      void _load_weights(const std::string &filename);
      void _load_rules(const std::string &filename);
    public:
      const Config &cfg;

      std::string comment;

      Sentence &sent;
      ulong nsentences;

      Categories &cats;

      Lexicon lexicon;
      DependencyAttributes dep_attrs;
      DistanceAttributes dist_attrs;
      RuleAttributes rule_attrs;

      CatFeature cat_feats;
      RuleFeature rule_feats;
      RuleHeadFeature rule_head_feats;
      RuleDepFeature rule_dep_feats;
      RuleDepDistFeature rule_dep_dist_feats;
      DepFeature dep_feats;
      DepDistFeature dep_dist_feats;
      GenruleFeature genrule_feats;

      // used only in the normal form parser when checking if a rule
      // instance is in CCGbank
      RuleInstances rule_instances;

      ulong nfeatures;
      double *weights;

      Chart chart;
      Rules rules;
      SuperCats results;
      std::vector<ulong> ids;

      InsideOutside inside_outside;

      Statistics stats;

      _Impl(const Config &cfg, Sentence &sent,
	    Categories &cats, ulong load);
      ~_Impl(void){ delete [] weights; };

      void reset(void);

      void combine(Cell &left, Cell &right, long pos, long span);
      bool parse(double BETA, bool repair);

      bool deps_count(std::ostream &out);
      void calc_stats(Statistics &stats);
      bool count_rules(void);
      bool print_forest(InsideOutside &inside_outside, std::ostream &out, ulong id,
			const std::vector<ulong> &correct, const std::vector<long> &rules);

      void print_leaf_features(InsideOutside &inside_outside, std::ostream &out, ulong pos,
			       const SuperCat *leaf, const Words &words, const Words &tags);
      void print_unary_features(InsideOutside &inside_outside, std::ostream &out, const SuperCat *sc,
				const Words &words, const Words &tags);
      void print_binary_features(InsideOutside &inside_outside, std::ostream &out, const SuperCat *sc,
				 const Words &words, const Words &tags);
      void print_root_features(InsideOutside &inside_outside, std::ostream &out, const SuperCat *sc,
			       const Words &words, const Words &tags);

      void calc_root_canonical(SuperCat *sc, const Words &words, const Words &tags);
      void calc_score_canonical(SuperCat *sc, const Words &words, const Words &tags);

      void calc_beam_scores(Cell &cell, const Words &words, const Words &tags);
      void apply_beam(Cell &cell, double beam);
      double calc_beam_score(SuperCat *sc, const Words &words, const Words &tags);

      bool calc_scores(void);
      void raws2words(const Raws &raw, Words &words) const;
      void calc_score(SuperCat *sc, const Words &words, const Words &tags);

      void calc_score_unary(SuperCat *sc, const Words &words, const Words &tags);
      double calc_beam_score_unary(SuperCat *sc, const Words &words, const Words &tags);

      double score_binary_feats(SuperCat *sc, const Words &words, const Words &tags);
      void calc_score_binary(SuperCat *sc, const Words &words, const Words &tags);
      double calc_beam_score_binary(SuperCat *sc, const Words &words, const Words &tags);

      void calc_score_leaf(SuperCat *sc, const Words &words, const Words &tags);
      double calc_beam_score_leaf(SuperCat *sc, const Words &words, const Words &tags);

      ulong dependency_marker(const Filled *filled) const;

      ulong get_feature(const std::string &filename, const std::string &line, std::vector<long> &rules) const;

      const SuperCat *best(Decoder &decoder);
    };
  }
}
