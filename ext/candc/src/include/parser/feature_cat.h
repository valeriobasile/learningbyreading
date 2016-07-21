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

    class CatFeature: public FeatureType {
    private:
      Categories &cats;
      const Lexicon lexicon;
      CatValueAttributes cv_attrs;

      void _add_cv(const SuperCat *sc, const Variable *var, Type type, const Words &values,
		   std::vector<ulong> &ids) const;
      void _add_c(const SuperCat *sc, Type type, std::vector<ulong> &ids) const;
      void _add_cv(const SuperCat *sc, Type type, const Words &values,
	      std::vector<ulong> &ids) const;
      double _score_c(const SuperCat *sc, Type type) const;
      double _score_cval(const SuperCat *sc, Type type, const Word value) const;
      double _score_cvar(const SuperCat *sc, const Variable *var, Type type,
			 const Words &values) const;
      double _score_cvar(const SuperCat *sc, Type type, const Words &values) const;
    public:

      CatFeature(Categories &cats, const Lexicon &lexicon):
	cats(cats), lexicon(lexicon){}
      virtual ~CatFeature(void){ /* do nothing */ }
      
      void load(std::istream &in, const std::string &filename, ulong id, Type type);
      ulong get_id(std::istream &in, const std::string &filename, Type type,
		   std::vector<long> &rules) const;
      void add(const SuperCat *sc, const Words &words, const Words &tags, 
	       Type type, std::vector<ulong> &ids) const;
      double score(const SuperCat *sc, const Words &words, const Words &tags, 
		   Type type) const;
      void set_weights(const double *w){
	cv_attrs.set_weights(w);
      }
    };
  }
}
