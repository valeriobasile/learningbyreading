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

// NLP::Extract::Feature
// feature representation in the extraction classes
// only need to store class and frequency because the attribute
// is implicitly represented by storing features with the same
// attribute together in NLP::Extract::Attributes

namespace NLP {
  namespace Extract {

    // extraction feature representation
    class Feature {
    public:
      Tag tag;		// tag or class
      ulong freq;	// feature count

      Feature(Tag tag, ulong freq): tag(tag), freq(freq) {};
      Feature(const Feature &other): tag(other.tag), freq(other.freq) {};
    };

    // vector of features stored in each NLP::Extract::Attributes entry
    typedef std::vector<Feature> Features;

    // used for sorting attributes by their class
    class FeatureCmp {
    public:
      bool operator ()(const Feature &f1, const Feature &f2){
        return f1.tag < f2.tag;
      };
    };

  }
}
