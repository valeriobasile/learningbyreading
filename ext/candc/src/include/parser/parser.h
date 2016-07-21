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

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"

#include "parser/markedup.h"
#include "parser/gr_constraints.h"
#include "parser/gr.h"
#include "parser/relation.h"
#include "parser/relations.h"
#include "parser/canonical.h"
#include "parser/categories.h"

#include "parser/decoder.h"

#include "parser/statistics.h"

namespace NLP {

  using namespace Config;

  namespace CCG {

    class InsideOutside;
    class Chart;
    class Rules;

    class Parser {
    public:

      class Config: public Directory {
      public:
	OpPath cats;
	OpPath markedup;
	OpPath weights;
	OpPath rules;

	std::string lexicon(void) const { return derived_path(path, "lexicon"); }
	std::string features(void) const { return derived_path(path, "features"); }

	Op<ulong> maxwords;
	Op<ulong> maxsupercats;

	Op<bool> alt_markedup;
	Op<bool> seen_rules;
	Op<bool> extra_rules;
	Op<bool> question_rules;
	Op<bool> noisy_rules;
	Op<bool> eisner_nf;
	Op<bool> partial_gold;
	Op<double> beam;

	Config(const OpPath *base = 0, const std::string &name = "parser",
	       const std::string &desc = "parser config");
      };
    public:
      static const ulong LOAD_WEIGHTS = 2;
      static const ulong LOAD_FEATURES = 1;
      static const ulong LOAD_NONE = 0;

      Parser(const Config &cfg, Sentence &sent,
	     Categories &cats, ulong load = LOAD_WEIGHTS);
      ~Parser(void);

      bool parse(double BETA, bool repair);
      void reset(void);

      bool deps_count(std::ostream &out);
      bool count_rules(void);
      void print_rules(std::ostream &out) const;
      bool print_forest(InsideOutside &inside_outside, std::ostream &out, ulong id,
			const std::vector<ulong> &correct, const std::vector<long> &rules);
      ulong get_feature(const std::string &filename, const std::string &line,
			std::vector<long> &rules) const;

      bool is_partial_gold(void) const;

      bool calc_scores(void);
      const SuperCat *best(Decoder &decoder);
      void calc_stats(Statistics &stats);

      void dump_chart(std::ostream &out) const;

      Sentence &sentence(void);
      Chart &chart(void);
      Rules &rules(void);
    private:
      class _Impl;
      _Impl *_impl;
    };
    
  }
}
