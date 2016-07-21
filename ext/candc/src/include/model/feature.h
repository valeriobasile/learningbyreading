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

// NLP::Model::Feature
// feature representation for the taggers
// we only need to store the feature weight and the class associated
// with the feature because the features are collected together that
// share the same attribute value

// NLP::Model::Features is a vector of features that is used to
// store the features in the tagger
// it is important that the capacity of this vector is larger than
// the number of attributes because NLP::Model::Attribute objects
// point into this vector, so it cannot be resized without invalidating
// the Attribute pointers

namespace NLP {
  namespace Model {

    class Feature {
    public:
      ulong klass;	// class index
      float lambda;	// feature weight (sum form)

      Feature(void): klass(0), lambda(0.0) {};
      Feature(ulong klass, float lambda):
        klass(klass), lambda(lambda) {};
    };

    typedef std::vector<Feature> Features;

  }
}
