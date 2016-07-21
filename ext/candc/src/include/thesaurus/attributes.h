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

    class Attributes {
    private:
      class _Impl;
      _Impl *_impl;
    public:
      Attributes(Types types, Pool *pool = 0, Pool *lpool = 0);
      Attributes(const Attributes &other);
      ~Attributes(void);

      Attributes &operator=(const Attributes &other);

      ulong size(void) const;

      Attribute *add(char *word);
      Attribute *add(char *word, float freq);

      Attribute *get(ulong id) const;
      Attribute *operator[](ulong id) const { return get(id); };

      Attribute *find(const char *s) const;
      Attribute *find(const std::string &s) const { return find(s.c_str()); };
      Attribute *operator[](const char *s) const { return find(s); };

      // serializing interface
      void load_1(std::istream &in, const std::string &fname);
      void dump_1(std::ostream &out) const;

      void load_2(std::istream &in, const std::string &fname);
      void dump_2(std::ostream &out) const;

      void printstats(std::ostream &os) const;

      void sort_by_alpha(void);
    };

  }
}
