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

    class RuleDepDistFeature: public DistFeatureType {
    private:
      Categories &cats;
      Lexicon lexicon;
      DepDistAttributes depdist_attrs;
      
      void _add(const Cat *catl, const Cat *catr, const Cat *cat,
		const Variable *varl, const Variable *varr, const Variable *var,
		Type type, const Words &values, const Words &tags,
		std::vector<ulong> &ids) const;
      void _add(const SuperCat *scl, const SuperCat *scr, const SuperCat *sc, 
		Type type, const Words &values, const Words &tags, 
		std::vector<ulong> &ids) const;
      double _score(const Cat *catl, const Cat *catr, const Cat *cat, Type type, 
		    const Word value, ushort dist) const;
      double _score(const Cat *catl, const Cat *catr, const Cat *cat, 
		    const Variable *varl, const Variable *varr, const Variable *var,
		    Type type, const Words &values, const Words &tags) const;
      double _score(const SuperCat *scl, const SuperCat *scr, const SuperCat *sc,
		    Type type, const Words &values, const Words &tags) const;

    public:

      RuleDepDistFeature(Categories &cats, const Lexicon &lexicon):
	cats(cats), lexicon(lexicon){}
      virtual ~RuleDepDistFeature(void){ /* do nothing */ }
      
      void load(std::istream &in, const std::string &filename, ulong id, Type type);
      ulong get_id(std::istream &in, const std::string &filename, Type type,
		   std::vector<long> &rules) const;
      void add(const SuperCat *sc, const Words &words, const Words &tags, 
	       Type type, std::vector<ulong> &ids) const;
      double score(const SuperCat *sc, const Words &words, const Words &tags, 
		   Type type) const;
      void set_weights(const double *weights){
	depdist_attrs.set_weights(weights);
      }
    };
  }
}
