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

// NLP::Model::Attribute

// Attributes are not represented explicity by the system
// instead, they are represented by a pair of pointers into
// attribute ordered vector of Feature objects

// In the tagger Attribute objects are returned by the
// various attribute hash tables which convert between
// the instatiated feature types and the features themselves

namespace NLP {
  namespace Model {

    class Attribute {
    public:
      const Feature *begin;
      const Feature *end;

      Attribute(void): begin(0), end(0){}
      Attribute(None): begin(0), end(0){}
      Attribute(const Feature *begin, const Feature *end): begin(begin), end(end){}
    };

  }
}
