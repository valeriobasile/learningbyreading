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
  namespace Thesaurus {

    class WChi2: public Weight {
    public:
      WChi2(void): Weight("chi2.cc", "Chi2", "see code (manually constructed)") {};
      ~WChi2(void) {};

      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}
