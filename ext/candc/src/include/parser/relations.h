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

    class Categories;

    class Relations {
    public:
      static RelID conj1;
    private:
      class _Impl;
      _Impl *_impl;
    public:
      Relations(const std::string &name);
      Relations(const Relations &other);

      ~Relations(void);

      Relations &operator=(const Relations &other);

      const std::string name(void) const;
      size_t size(void) const;

      void init_conj(const Categories &categories);

      RelID get(const std::string &cat, ulong slot) const;

      RelID add(const std::string &cat, ulong slot, ulong jslot) const;
      RelID operator()(const std::string &cat, ulong slot, ulong jslot) const {
	return add(cat, slot, jslot);
      };

      void add_gr(const Categories &cats, const std::string &markedup,
		  ulong slot, const std::string &fmt);

      void set_constraints(const Categories &cats);
      void set_cats(const Categories &cats);

      const Relation &rel(RelID id) const;
      const Relation &rel_checked(RelID id) const;
      const Relation &operator[](RelID id) const { return rel(id); };
    };

  }
}
