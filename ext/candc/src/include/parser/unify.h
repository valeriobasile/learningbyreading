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

    class Unify {
    public:
      uchar heads[Vars::NVARS][Vars::NVARS];
      Feature feature;

      ulong nvariables;
      VarID order;
      VarID trans1[Vars::NVARS];
      VarID trans2[Vars::NVARS];
      VarID old1[Vars::NVARS];
      VarID old2[Vars::NVARS];
      VarID seen[Vars::NVARS];

      CatID lrange1[Vars::NVARS];
      CatID lrange2[Vars::NVARS];

      VarID max1;
      VarID max2;

      Unify(void){};
      ~Unify(void){};

      bool _unify(const Cat *c1, const Cat *c2);

      void _matrix2trans(void);

      void add_var(VarID var, VarID trans[], VarID old[]);
      void add_var1(VarID var1){ add_var(var1, trans1, old1); };
      void add_var2(VarID var2){ add_var(var2, trans2, old2); };

      void add_vars(const Cat *c1, VarID trans[], VarID old[]);
      void add_vars1(const Cat *c1){ add_vars(c1, trans1, old1); };
      void add_vars2(const Cat *c2){ add_vars(c2, trans2, old2); };

      void reorder(const SuperCat *sc1, const SuperCat *sc2);

      bool operator()(const Cat *){
        nvariables = 1;
        return true;
      }

      bool operator()(const Cat *c1, const Cat *c2){
        if(c1->uhash != c2->uhash)
          return false;

        memset(heads, 0, sizeof(heads));
        memset(lrange1, 0, sizeof(lrange1));
        memset(lrange2, 0, sizeof(lrange2));
        memset(seen, 0, sizeof(seen));
        feature = Features::NONE;
        order = 0;
	max1 = max2 = 0;

        if(_unify(c1, c2)){
          _matrix2trans();
          return true;
        }else
          return false;
      }

      bool operator()(const SuperCat *sc1, const SuperCat *sc2){
        for(VarID i = 0; i < nvariables; ++i)
          if(sc1->vars[old1[i]].is_filled() && sc2->vars[old2[i]].is_filled())
            return false;
        return true;
      }
    };

  }
}
