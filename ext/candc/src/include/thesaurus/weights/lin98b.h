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

    class WLin98b: public Weight {
    private:
      std::vector<float> _ftypes;
    public:
      WLin98b(void): Weight("lin98b.cc", "Lin98b", "see code (manually constructed)") {};
      ~WLin98b(void) {};
    
      void init(const Object *o);
      float operator()(const Object *o, const Attribute *a, const Relation *r) const;
    };

  }
}
