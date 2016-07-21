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

    class Types {
    private:
      class _Impl;
      _Impl *_impl;
    public:
      Types(Pool *pool = 0, Pool *lpool = 0);
      Types(const Types &other);
      ~Types(void);

      Types &operator=(const Types &other);

      ulong size(void) const;

      Type *find(std::string s) const;
      Type *get(ulong id) const;
      Type *operator[](ulong id) const { return get(id); };

      Type *add(const char *word);
      Type *add(const char *word, ulong freq);

      Type *extract(const char *word);

      // serializing interface
      void load_1(std::istream &in, char *fname);
      void dump_1(std::ostream &out) const;

      void load_2(std::istream &in, char *fname);
      void dump_2(std::ostream &out) const;
    };

  }
}
