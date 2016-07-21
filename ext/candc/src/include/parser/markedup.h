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

    class Markedup {
    private:
      class _Impl;
      _Impl *_impl;
    public:
      Markedup(const std::string &name);
      Markedup(const Markedup &other);

      ~Markedup(void);

      Markedup &operator=(Markedup &other);

      const std::string name(void) const;
      size_t size(void) const;
  
      const Cat *cat(const std::string &str) const;
      const Cat *cat(CatID id) const;

      const Cat *operator[](const std::string &str) const { return cat(str); }
      const Cat *operator[](CatID id) const { return cat(id); }

      const std::string &markedup(const std::string &str) const;

      void add(const std::string &plain, const std::string &markedup, const Cat *cat);
    };

  }
}
