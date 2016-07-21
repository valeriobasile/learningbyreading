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

    class Filled;
    typedef std::vector<const Filled *> FilledDeps;

    class SuperCat;
    class Categories;

    struct Argument {
      Raw raw;
      int pos;

      Argument(void): pos(-1){}
      Argument(Raw raw, int pos): raw(raw), pos(pos){}
    };

    typedef std::vector<Argument> Arguments;

    struct GR {
      Raw label;
      Arguments args;
    };

    typedef std::vector<GR> GRs;

    class GRTemplate {
    protected:
      std::string _tmp_cat; // temporary storage for the category constraint

      void get(GRs &grs, const std::string &format, const Sentence &sent,
	       const SuperCat *sc, const Filled *dep, const Filled *other,
	       const Filled *constraint) const;
    public:
      const std::string markedup;
      bool ignore;
      std::string fmt;
      RelID other_rel;

      bool constrained;    // are there any constraints on
      const GRConstraints groups; // lexical constraint groups 
      std::string con_lex; // lexical constraint label
      const Cat *con_cat;  // category constraint
      RelID con_rel;       // relation that the category constraint applies to
      GRTemplate *next;

      GRTemplate(const Categories &cats, const std::string &cat,
		 ulong slot, const std::string &fmt);

      void set_cat(const Categories &cats);

      bool satisfy(const Sentence &sent, const SuperCat *sc, const Filled *filled) const;
      void get(GRs &grs, const Sentence &sent, const SuperCat *sc,
	       const FilledDeps &seen, const Filled *filled) const;
    };

  }
}
