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

    class Canonical {
    private:
      class _Impl;
      _Impl *_impl;
    public:
      Canonical(void);
      Canonical(const Canonical &other);

      ~Canonical(void);

      Canonical &operator=(Canonical &other);

      size_t size(void) const;

      const Cat *get(const Cat *cat) const;
      const Cat *operator[](const Cat *cat) const { return get(cat); }

      const Cat *add(const Cat *cat);
      
    };

  }
}
