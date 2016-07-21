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
  namespace CCG {

    typedef std::vector<RawTags> SuperTags;
    typedef std::vector<Position> Positions;
    typedef uchar DistType;

    class Generator {
    public:
      static const DistType WORDS = 0;
      static const DistType VERBS = 1;
      static const DistType PUNCT = 2;

      const ulong MAXWORDS;
      ulong nsentences;
      Categories &cats;
      Chart TBchart;
      Chart chart;
      Rules rules;
      SuperCats results;

      Generator(Categories &cats, bool EXTRA_RULES, ulong MAXWORDS)
	: MAXWORDS(MAXWORDS), nsentences(0), cats(cats),
	  TBchart(cats, EXTRA_RULES, MAXWORDS),
	  chart(cats, EXTRA_RULES, MAXWORDS),
	  rules(chart.pool, cats.markedup, EXTRA_RULES, true){}
      ~Generator(void){}

      void convertTBsent(const TBSentence &TBsentence, Sentence &sent) const {
	sent.reset();
	for(ulong i = 0; i < TBsentence.size(); ++i){
	  if(TBsentence[i].is_leaf()){
	    sent.words.push_back(TBsentence[i].word);
	    sent.pos.push_back(TBsentence[i].pos);
	    MultiRaw mraw;
	    mraw.push_back(ScoredRaw(TBsentence[i].catNoBrack, 0.0));
	    sent.msuper.push_back(mraw);
	  }
	}
      }

      bool combine(const long j, const long i, const long k, std::ostream &log);
      bool unary(Cell &TBcell, Cell &cell);
      void print_sentence(std::ostream &out, const Sentence &sent);
      void print_deps(std::ostream &out, const SuperCat *sc, char type, const Raws &heads, const Raws &words);
      void print_deps(std::ostream &out, const SuperCat *sc, const Markedup &markedup,
		      const Relations &rel, const Raws &heads, const Raws &words,
		      bool julia_slots);
      void print_root(std::ostream &out, const Cat *cat, char type, const Raws &values, const Variable *var);
      void print_deps_dist(std::ostream &out, const SuperCat *sc, char type, const Raws &values, 
			   const std::vector<std::string> &tags, DistType dist);
      void print_rules(std::ostream &out, const SuperCat *sc, char utype, char btype);
      void print_rheads(std::ostream &out, const Cat *cat1, const Cat *cat2, const Cat *cat3, char type, 
			const Raws &values, const Variable *var);
      void print_rules_deps(std::ostream &out, const SuperCat *sc, char type, const Raws &values1, 
			    const Raws &values2);
      void _print_rdeps(std::ostream &out, const Cat *cat1, const Cat *cat2, const Cat *cat3, char type, 
			const Variable *var1, const Variable *var2,
			const Raws &values1, const Raws &values2);

      void _print_rdeps_dist(std::ostream &out, const Cat *cat1, const Cat *cat2, const Cat *cat3, char type, 
			     const Variable *hvar, const Variable *lvar, const Variable *rvar, 
			     const Raws &values, const Raws &tags, DistType dist);
      void _print_rdeps_dist(std::ostream &out, char type, Position lpos, Position rpos, const Raws &tags, 
			     DistType dist);
      void print_ruledeps_dist(std::ostream &out, const SuperCat *sc, char type,
			       const Raws &values, const Raws &tags, DistType dist);

      void print_rules(std::ostream &out, const SuperCat *sc, char utype, char btype, const Raws &values);
      void print_rule_types(std::ostream &out, const SuperCat *sc, char type1);
      void print_rule_types(ushort flags);
      void print_filled(std::ostream &out, const SuperCat *sc);
      void print_filled(std::ostream &out, const SuperCat *sc,
			const Raws &words, bool julia_slots);

      bool parse(const TBSentence &TBsentence);
      bool parse(const TBSentence &tb, const Sentence &sent, std::ostream &log); 
    };
  }  
}
