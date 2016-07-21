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

    class InsideOutside {
    public:
      Pool *pool;
      DepScore depscores;
      const bool PARTIAL_GOLD;

      InsideOutside(bool PARTIAL_GOLD = false):
	pool(new Pool(1 << 20)), depscores("depscore"), PARTIAL_GOLD(PARTIAL_GOLD){}
      ~InsideOutside(void){
	delete pool;
      }
      
      Filled *load_dependency(const std::string &filename, const std::string &line) const;
      bool read_dependencies(const std::string filename, std::istream &in);
      int count_gold_deps(const SuperCat *sc) const;
      double conj_calc_inside(SuperCat *conj);
      void conj_calc_outside(SuperCat *conj, double outside, double invZ);
      void disj_calc_inside(SuperCat *disj);
      void disj_calc_outside(SuperCat *disj, double invZ);

      double calc_inside(Chart &chart);
      void calc_outside(Chart &chart, double invZ);
      void calc(Chart &chart){
	double Z = calc_inside(chart);
	calc_outside(chart, -Z);
      }
      double calc_stats(Chart &chart, ulong &nequiv, ulong &ntotal);
    };
  }
}
