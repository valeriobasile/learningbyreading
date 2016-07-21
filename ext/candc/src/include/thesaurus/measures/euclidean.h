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

    class MEuclidean: public Measure {
    public:
      MEuclidean(void): Measure("Euclidean") {};
      ~MEuclidean(void) {};
      float measure(const Object *obj1, const Object *obj2) const;
      bool heuristic(const Object *obj1, const Object *obj2) const;
      void explain(std::ostream &out, const Object *obj1, const Object *obj2) const;
    };

  }
}
