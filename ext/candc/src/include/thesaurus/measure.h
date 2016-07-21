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

    class Measure {
    public:
      const std::string NAME;

      bool verbose;
      bool use_heuristic;

      Object *previous;
      Matches matches;

      Measure(const char *name):
          NAME(name), verbose(false), use_heuristic(false), previous(0) {};
      virtual ~Measure(void) {};

      virtual float measure(const Object *obj1, const Object *obj2) const = 0;
      virtual bool heuristic(const Object *obj1, const Object *obj2) const = 0;
      virtual float match(const Object *obj1, const Object *obj2) const {
        if(heuristic(obj1, obj2))
          return measure(obj1, obj2);
        else
          return 0.0;
      };
      virtual void explain(std::ostream &out, const Object *obj1, const Object *obj2) const = 0;

      void best(Objects objects, std::ostream &out, Object *obj, Match &match);
      void all(Objects objects, std::ostream &out, Object *obj);
      void thesaurus(Objects objects, std::ostream &out);
      void thesaurus(Objects objects, const std::string &input, ulong print_limit);
      void pairs(Objects objects, std::ostream &out);
    };

    extern Measure *measure;

  }
}
