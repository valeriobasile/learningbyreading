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

    typedef uchar Position;
    typedef ushort RelID;
    typedef ushort CatID;
    typedef uchar RuleID;
   
    typedef uchar CatFlags;
    typedef uchar Slash;
    const static Slash BWD = 0;
    const static Slash FWD = 1;

    class Unify;
    class SuperCat;

    class Cat {
    private:
      Cat(Atom atom, Feature feature, VarID var, RelID rel, CatID lrange);
      Cat(const Cat *res, Slash slash, const Cat *arg, VarID var);
      Cat(const Cat *res, Slash slash, const Cat *arg, VarID var, RelID rel, CatID lrange);
      Cat(Pool *pool, const Cat *other);
      Cat(Pool *pool, const Cat *other, VarID trans[], Feature X);

      static Cat *do_insert12(Pool *pool, const Cat *replace, const Cat *at,
				    const Cat *current, Unify &unify);
      static Cat *do_insert21(Pool *pool, const Cat *replace, const Cat *at,
				    const Cat *current, Unify &unify);

    public:
      static const uchar SLASH = 1 << 0;
      static const uchar RES_S = 1 << 1;
      static const uchar ARG_S = 1 << 2;
      static const uchar RES_NP = 1 << 3;
      static const uchar ARG_NP = 1 << 4;
      static const uchar RES_N = 1 << 5;
      static const uchar ARG_N = 1 << 6;
      static const uchar RES_SbNP = 1 << 7;

      static Hash uhash_basic(Atom atom);
      static Hash rhash_basic(Atom atom, Feature feature);
      static Hash uhash_complex(const Cat *res, Slash slash, const Cat *arg);
      static Hash rhash_complex(const Cat *res, Slash slash, const Cat *arg);
      Hash rhash_rebuild(void) const;

      const Hash uhash;    // unification hash (no feature, [dcl] etc.)

      const Atom atom;
      const Feature feature;
      const CatFlags flags;
      VarID var;

      const RelID rel;
      const CatID lrange;

      const Cat *const res;
      const Cat *const arg;

      const Hash rhash;    // used by RuleInstances and Equivalence

      static Cat *Basic(Pool *pool, Atom atom, Feature feature, VarID var, RelID rel, CatID lrange);
      static Cat *Complex(Pool *pool, const Cat *res, Slash slash, const Cat *arg, VarID var);
      static Cat *Complex(Pool *pool, const Cat *res, Slash slash, const Cat *arg, VarID var, 
			  RelID rel, CatID lrange);
      static Cat *Clone(Pool *pool, const Cat *other);
      static Cat *Trans(Pool *pool, const Cat *other, VarID trans[], Feature X);

      static Cat *Join12(Pool *pool, const Cat *c1, Slash slash, const Cat *c2,
                         VarID var1, const SuperCat *sc1, const SuperCat *sc2, Unify &unify);
      static Cat *Join21(Pool *pool, const Cat *c2, Slash slash, const Cat *c1,
                         VarID var1, const SuperCat *sc1, const SuperCat *sc2, Unify &unify);
      static Cat *Insert12(Pool *pool, const Cat *replace, const Cat *at, const Cat *root,
				 VarID var1, const SuperCat *sc1, const SuperCat *sc2, Unify &unify);
      static Cat *Insert21(Pool *pool, const Cat *replace, const Cat *at, const Cat *root,
				 VarID var1, const SuperCat *sc1, const SuperCat *sc2, Unify &unify);

      ~Cat(void) { /* do nothing */ };

      void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); };
      void operator delete(void *, Pool *) { /* do nothing */ }

      bool is_atomic(void) const { return atom != Atoms::NONE; }
      bool is_complex(void) const { return atom == Atoms::NONE; }

      Slash slash() const{ return flags & SLASH; }
      bool is_fwd(void) const { return slash(); }
      bool is_bwd(void) const { return is_complex() && !is_fwd(); }
      bool not_fwd(void) const { return !is_fwd(); }
      bool not_bwd(void) const { return is_atomic() || is_fwd(); }

      bool is_NP(void) const { return atom.is_NP(); }
      bool is_N(void) const { return atom.is_N(); }
      bool is_NorNP(void) const { return atom.is_NorNP(); }
      bool is_PP(void) const { return atom.is_PP(); }
      bool is_conj(void) const { return atom.is_conj(); }
      bool is_S(void) const { return atom.is_S(); }
      bool is_comma(void) const { return atom.is_comma(); }
      bool is_commaORperiod(void) const { return atom.is_commaORperiod(); }
      bool is_period(void) const { return atom.is_period(); }
      bool is_colon(void) const { return atom.is_colon(); }
      bool is_semicolon(void) const { return atom.is_semicolon(); }
      bool is_semiORcolon(void) const { return atom.is_semiORcolon(); }
      bool is_LRB(void) const { return atom.is_LRB(); }
      bool is_RRB(void) const { return atom.is_RRB(); }
      bool is_LQU(void) const { return atom.is_LQU(); }
      bool is_RQU(void) const { return atom.is_RQU(); }
      bool is_punct(void) const { return atom.is_punct(); }

      bool has_none(void) const { return feature.is_none(); }
      bool has_var(void) const { return feature.is_var(); }
      bool is_free(void) const { return feature.is_free(); }
      bool has_adj(void) const { return feature.is_adj(); }
      bool has_pss(void) const { return feature.is_pss(); }
      bool has_to(void) const { return feature.is_to(); }
      bool has_ng(void) const { return feature.is_ng(); }
      bool has_dcl(void) const { return feature.is_dcl(); }
      bool has_b(void) const { return feature.is_b(); }

      static CatFlags build_flags(const Cat *res, const Slash slash, const Cat *arg);

      bool check_flags(const CatFlags mask, const CatFlags values) const{
	return (flags & mask) == values;
      }

      bool is_SbNP(void) const { return check_flags(SLASH | RES_S | ARG_NP, BWD | RES_S | ARG_NP); }
      bool is_SdclbNP(void) const { return is_SbNP() && res->has_dcl(); }
      bool is_StobNP(void) const { return is_SbNP() && res->has_to(); }
      bool is_SfNP(void) const { return check_flags(SLASH | RES_S | ARG_NP, FWD | RES_S | ARG_NP); }
      bool is_SbS(void) const { return check_flags(SLASH | RES_S | ARG_S, BWD | RES_S | ARG_S); }
      bool is_SbPP(void) const { return check_flags(SLASH | RES_S, BWD | RES_S) && arg->is_PP(); }
      bool is_NPbNP(void) const { return check_flags(SLASH | RES_NP | ARG_NP, BWD | RES_NP | ARG_NP); }
      bool is_NbN(void) const { return check_flags(SLASH | RES_N | ARG_N, BWD | RES_N | ARG_N); }
      bool is_SfS(void) const { return check_flags(SLASH | RES_S | ARG_S, FWD | RES_S | ARG_S); }
      bool is_NfN(void) const { return check_flags(SLASH | RES_N | ARG_N, FWD | RES_N | ARG_N); }
      bool is_NfNbNfN(void) const { return is_fwd() && arg->is_NfN() && res->is_NfN(); }
      bool is_NPfNPbNP(void) const {
	return check_flags(SLASH | RES_NP, FWD | RES_NP) && arg->is_NPbNP();
      }
      bool is_SbNPbSbNP(void) const {
        return check_flags(SLASH | RES_SbNP, BWD | RES_SbNP) && arg->is_SbNP(); 
      };
      bool is_VPbVPbVPbVP(void) const {
        return is_bwd() && arg->is_SbNPbSbNP() && res->is_SbNPbSbNP(); 
      }
      bool is_SbNPfSbNP(void) const {
	return check_flags(SLASH | RES_SbNP, FWD | RES_SbNP) && arg->is_SbNP(); 
      }
      bool is_Sdcl(void) const { return is_S() && has_dcl(); }
      bool is_Sb(void) const { return is_S() && has_b(); }
      bool is_SdclbNPfSdcl(void) const {
	return check_flags(SLASH | RES_SbNP | ARG_S, FWD | RES_SbNP | ARG_S) && 
	  res->is_SdclbNP() && arg->has_dcl();
      }
      bool is_SdclbNPfS(void) const {
	return check_flags(SLASH | RES_SbNP | ARG_S, FWD | RES_SbNP | ARG_S) && 
	  res->is_SdclbNP();
      }
      bool is_SdclbNPfNP(void) const {
	return check_flags(SLASH | RES_SbNP | ARG_NP, FWD | RES_SbNP | ARG_NP) && 
	  res->is_SdclbNP();
      }
      bool is_SdclbNPfPP(void) const {
	return check_flags(SLASH | RES_SbNP, FWD | RES_SbNP) && 
	  res->is_SdclbNP() && arg->is_PP();
      }
      bool is_SdclbSbNP(void) const {
	return check_flags(SLASH | RES_S, BWD | RES_S) && 
	  arg->is_SbNP() && res->is_Sdcl();
      }
      bool is_SdclbSdcl(void) const {
	return check_flags(SLASH | RES_S | ARG_S, BWD | RES_S | ARG_S) && 
	  res->is_Sdcl() && arg->is_Sdcl();
      }
      bool is_SdclfSdcl(void) const {
	return check_flags(SLASH | RES_S | ARG_S, FWD | RES_S | ARG_S) && 
	  res->is_Sdcl() && arg->is_Sdcl();
      }
      bool is_SdclbSdclbNP(void) const {
	return check_flags(SLASH | ARG_NP, BWD | ARG_NP) && 
	  res->is_SdclbSdcl();
      }
      bool is_StobNPfNP(void) const {
	return check_flags(SLASH | RES_SbNP | ARG_NP, FWD | RES_SbNP | ARG_NP) &&
	  res->is_StobNP();
      }
      bool is_NPbNPfSdclbNP(void) const {
	return is_fwd() && res->is_NPbNP() && arg->is_SdclbNP();
      }
      bool is_PPbPP(void) const {
	return is_bwd() && res->is_PP() && arg->is_PP();
      }
      bool is_AP(void) const { return is_SbNP() && res->has_adj(); }
      bool is_argNorNP(void) const {
	return flags & (ARG_N | ARG_NP);
      }

      uchar nvars(void) const {
	uchar current = var;
	++current;
        if(atom)
          return current;
        else{
          uchar max = res->nvars();
          uchar maxarg = arg->nvars();
          if(max < maxarg)
            max = maxarg;
          if(max < current)
            max = current;
          return max;
        }
      }

      VarID has_lrange(void) const {
        if(atom)
          return lrange != 0;
        else
          return arg->has_lrange() || res->has_lrange();
      }

      void reorder(VarID seen[], VarID &order){
        if(res){
          const_cast<Cat *>(res)->reorder(seen, order);
          const_cast<Cat *>(arg)->reorder(seen, order);
        }

        if(var && !seen[var])
          seen[var] = ++order;
        var = seen[var];
      }

      void order(VarID trans[], VarID seen[], VarID &order) const {
        if(atom){
          if(var && !seen[trans[var]])
            seen[trans[var]] = ++order;
        }else{
          res->order(trans, seen, order);
          arg->order(trans, seen, order);          
          if(var && !seen[trans[var]])
            seen[trans[var]] = ++order;
        }
      }

      std::ostream &out(std::ostream &stream) const;
      std::ostream &out_novar(std::ostream &stream, const bool brack) const;
      std::ostream &out_novar_noX(std::ostream &stream, const bool brack) const;
      std::ostream &out_boxer(std::ostream &stream, Feature parent,
			      const bool brack = false) const;
      std::ostream &out_short(std::ostream &stream, const bool brack = false) const;
      std::ostream &out_js(std::ostream &stream) const;
    };

    inline std::ostream &operator <<(std::ostream &out, const Cat &cat){ return cat.out(out); }

    bool _unequal(const Cat &c1, const Cat &c2);
    bool _uneq(const Cat *c1, const Cat *c2);

    inline bool operator!=(const Cat &c1, const Cat &c2){
      if(c1.rhash != c2.rhash || c1.uhash != c2.uhash)
	return true;

      return _unequal(c1, c2);
    }

    inline bool operator==(const Cat &c1, const Cat &c2){
      if(c1.rhash != c2.rhash || c1.uhash != c2.uhash)
	return false;

      return !_unequal(c1, c2);
    }

    inline bool eq(const Cat *c1, const Cat *c2){
      if(c1->rhash != c2->rhash || c1->uhash != c2->uhash)
	return false;

      return !_uneq(c1, c2);
    }

    inline bool rule_eq(const Cat *c1, const Cat *c2){
      if(c1->rhash != c2->rhash)
	return false;

      return true;
    }

    struct TRCat {
    public:
      const Cat *cat;
      VarID lex;
      VarID dep;

      TRCat(const Cat *cat, VarID lex, VarID dep):
          cat(cat), lex(lex), dep(dep) {};
      TRCat(const TRCat &other):
          cat(other.cat), lex(other.lex), dep(other.dep) {};
      ~TRCat(void){};
    };
  }
}
