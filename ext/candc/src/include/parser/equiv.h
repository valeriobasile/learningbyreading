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

    class Equivalence {
    private:
      class _Impl;
      _Impl *_impl;
    public:
      Equivalence(const std::string &name);
      Equivalence(const Equivalence &other);

      ~Equivalence(void);

      Equivalence &operator=(Equivalence &other);

      const std::string name(void) const;
      size_t size(void) const;

      bool add(Position pos, Position span, SuperCat *sc);
      void clear(void);
    };

  }
}
