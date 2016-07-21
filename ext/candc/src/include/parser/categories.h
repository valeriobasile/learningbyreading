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

    typedef std::vector<TRCat> TRCats;

    class Categories {
    public:
      TRCats trNP;
      TRCats trPP;
      TRCats trAP;
      TRCats trVP_to;

      Pool pool;

      Markedup markedup;
      GRConstraints gr_constraints;
      Relations relations;
      Markedup seen;
      Canonical canonical;

      CatID catid;

      std::string PREFACE;
    protected:
      void _read_markedup(const std::string &filename, bool alt_markedup);
      void _read_typeraise(const std::string &filename, TRCats &tr);

      Cat *_parse(const char *current, const char *orig);
      Cat *_consume_category(const char *&current, const char *orig,
			     bool in_result, ulong &njulia_slots);
      Cat *_consume_basic(const char *&current, const char *orig, ulong njulia_slots);
      Cat *_consume_complex(const char *&current, const char *orig,
			    bool in_result, ulong &njulia_slots);

      Feature _consume_feature(const char *&current, const char *orig);
      VarID _consume_var(const char *&current, const char *orig, CatID &lrange);
      RelID _consume_slot(const char *&current, const char *orig, ulong jslot);
    public:
      Categories(const std::string &dir, const std::string &markedup,
		 const bool ALT_MARKEDUP);
      ~Categories(void);

      Cat *parse(const char *s){ return _parse(s, s); }
      Cat *parse(const std::string &s){ return _parse(s.c_str(), s.c_str()); }

			const Constraint *constraint(const std::string &s);

      const Cat *canonize(const char *s){
				const Cat *cat = seen[s];
				if(!cat){
					cat = parse(s);
					seen.add(s, "", cat);
					canonical.add(cat);
				}
				return cat;
      }

      // used to create new canonical categories from a cat pointer
      const Cat *canonize(const Cat *cat);
    };

  }
}
