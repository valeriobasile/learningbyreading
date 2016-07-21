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

    class DepFeature: public FeatureType {
    private:
      Lexicon lexicon;
      DependencyAttributes dep_attrs;

      void _add(const Filled *filled, Type type, const Words &heads, const Words &words,
	   std::vector<ulong> &ids) const;
      double _score(const Filled *filled, Type type, const Words &heads,
		    const Words &words) const;

    public:
      DepFeature(const Lexicon &lexicon) : lexicon(lexicon) { }
      virtual ~DepFeature(void){ /* do nothing */ }
      
      void load(std::istream &in, const std::string &filename, ulong id, Type type);
      ulong get_id(std::istream &in, const std::string &filename, Type type,
		   std::vector<long> &rules) const;
      void add(const SuperCat *sc, const Words &words, const Words &tags, 
	       Type type, std::vector<ulong> &ids) const;
      double score(const SuperCat *sc, const Words &words, const Words &tags, 
		   Type type) const;
      void set_weights(const double *weights){
	dep_attrs.set_weights(weights);
      }
    };
  }
}
