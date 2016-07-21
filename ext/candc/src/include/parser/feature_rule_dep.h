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

    class RuleDepFeature: public FeatureType {
    private:
      Categories &cats;
      Lexicon lexicon;
      RuleAttributes rule_attrs;

      void _add(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var1,
		const Variable *var2, Type type, const Words &words1, const Words &words2,
		std::vector<ulong> &ids) const;
      void _add(const Cat *cat1, const Cat *cat2, const Cat *cat3, const SuperCat *sc1,
		const SuperCat *sc2, Type type, const Words &words1, const Words &words2,
		std::vector<ulong> &ids) const;
      double _score(const Cat *cat1, const Cat *cat2, const Cat *cat3, Type type,
		    const Word value1, const Word value2) const;
      double _score(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var1,
		    const Variable *var2, Type type, const Words &words1, 
		    const Words &words2) const;
      double _score(const Cat *cat1, const Cat *cat2, const Cat *cat3, const SuperCat *sc1,
		    const SuperCat *sc2, Type type, const Words &words1,
		    const Words &words2) const;

    public:

      RuleDepFeature(Categories &cats, const Lexicon &lexicon):
	cats(cats), lexicon(lexicon){}
      virtual ~RuleDepFeature(void){ /* do nothing */ }
      
      void load(std::istream &in, const std::string &filename, ulong id, Type type);
      ulong get_id(std::istream &in, const std::string &filename, Type type,
		   std::vector<long> &rules) const;
      void add(const SuperCat *sc, const Words &words, const Words &tags, 
	       Type type, std::vector<ulong> &ids) const;
      double score(const SuperCat *sc, const Words &words, const Words &tags, 
		   Type type) const;
      void set_weights(const double *weights){
	rule_attrs.set_weights(weights);
      }
    };
  }
}
