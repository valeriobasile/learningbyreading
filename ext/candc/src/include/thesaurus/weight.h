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

    class Attribute;
    class Relation;
    class Object;

    class Weight {
    public:
      const std::string FILENAME;
      const std::string NAME;
      const std::string FORMULA;

      Weight(char *filename, char *name, char *formula):
        FILENAME(filename), NAME(name), FORMULA(formula) {};
      ~Weight(void) {};

      virtual void init(const Object *o) {};
      virtual float operator()(const Object *o, const Attribute *a, const Relation *r) const = 0;
    };

  }
}
