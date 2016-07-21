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

    class DepScore {
    private:
      class _Impl;
      _Impl *_impl;
    public:
      DepScore(const std::string &name);
      DepScore(const DepScore &other);

      ~DepScore(void);

      DepScore &operator=(DepScore &other);

      const std::string name(void) const;
      size_t size(void) const;

      void add(const Filled *filled, double score);
      double get(const Filled *filled) const;
      double operator[](const Filled *filled) const { return get(filled); };

      void dump(std::ostream &out);
      void dump(std::ostream &out, const Markedup &markedup, const Relations &rels, const Raws &words);

      void clear(void);
    };

  }
}
