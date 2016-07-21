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

    class Objects {
    private:
      class _Impl;
      _Impl *_impl;
    public:
      Objects(Attributes attributes, Pool *pool = 0, Pool *lpool = 0);
      Objects(const Objects &other);
      ~Objects(void);

      Objects &operator=(const Objects &other);

      ulong size(void) const;

      Object *add(const char *s);
      Object *add(const std::string &s) { return add(s.c_str()); };

      Object *copy(const Object *obj);

      Object *find(const char *s) const;
      Object *find(const std::string &s) const { return find(s.c_str()); };

      Object *get(ulong id) const;
      Object *operator[](ulong id) const { return get(id); };

      // serializing interface
      void load_1(std::istream &in, const std::string &fname);
      void dump_1(std::ostream &out) const;

      void load_2(std::istream &in, const std::string &fname);
      void dump_2(std::ostream &out) const;

      void score(Weight &weight);
      void cutoff(const Options &op);
      void optimize(const Options &op);
      void heuristic(const Options &op);

      Object *pseudo(std::istream &in, const std::string &filename, const std::string &name);

      void split(Object *obj, float c, const std::string &newstr);
      Object *diff(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr);
      Object *sum(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr);
      Object *intersect(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr);

      void add(Object *target, const Object *src, float c);
      void sub(Object *target, const Object *src, float c);
      void intersect(Object *target, const Object *src, float c);

      void printstats(std::ostream &os) const;
      void printvectorstats(std::ostream &out);
    };

  }
}
