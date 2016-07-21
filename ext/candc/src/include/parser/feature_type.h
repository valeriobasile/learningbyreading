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

    using namespace NLP::Tree;

    class FeatureType {
    public:
      const double *weights;

      FeatureType(void): weights(0) {}
      virtual ~FeatureType(void){ /* do nothing */ }
      
      virtual void load(std::istream &in, const std::string &filename, ulong id, Type type) = 0;
      virtual ulong get_id(std::istream &in, const std::string &filename, Type type,
			   std::vector<long> &rules) const = 0;
      virtual void add(const SuperCat *sc, const Words &words, const Words &tags, 
		       Type type, std::vector<ulong> &ids) const = 0;
      virtual double score(const SuperCat *sc, const Words &words, const Words &tags, 
			   Type type) const = 0;
      virtual void set_weights(const double *w){
	weights = w;
      }
    };
  }
}
