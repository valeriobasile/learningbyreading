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

// NLP::MaxEnt::Attribute and NLP::MaxEnt::Attributes

// Attributes are not represented explicity by the system
// instead, they are represented by a pair of pointers into
// attribute ordered vector of Feature objects, i.e. all of
// the features for curr_word == 'the' are stored together
// and the begin and end pointers indicate their position in
// the vector of Feature objects

// for attribute number x, the value Attributes[x] points to
// the first feature in Features, and Attributes[x+1] points
// to one past the end of the sequence of Features

// Attributes is used to translate attribute id's from the
// model/contexts file into Attribute objects for each Context

namespace NLP {
  namespace MaxEnt {

    // delimits a series of Features with the same attribute
    struct Attribute {
      Feature *begin;	// first feature with attribute
      Feature *end;	// last feature + 1 with attribute
    };

    // used to translate attribute id's to Attribute objects
    typedef std::vector<Feature *> Attributes;

  }
}
