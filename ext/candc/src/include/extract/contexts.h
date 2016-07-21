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
  namespace Extract {
    // a vector of Attribute identifiers
    // after each context vector has been created it is
    // sorted and dumped out to the models/context file
    typedef std::vector<ulong> Context;

    class Contexts {
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    public:
      const std::string &PREFACE;

      Contexts(bool MERGE, bool SORT);
      Contexts(const Contexts &other);
      Contexts &operator=(const Contexts &other);

      ~Contexts(void);

      size_t size(void) const;

      void add(Tag klass, const Context &context);
      void sort_by_attributes(void);
      void save(const std::string &filename, const std::string &preface) const;
    };

  }
}
