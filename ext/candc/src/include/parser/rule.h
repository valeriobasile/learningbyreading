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

    class Rules {
    public:
      const static ulong NONE = 0;
      const static ulong FORWARD = 1;
      const static ulong BACKWARD = 2;
      const static ulong CONJ = 3;
      const static ulong PERIOD = 4;
      const static ulong COLON = 5;
      const static ulong COMMA = 6;
      const static ulong BRACKET = 7;
      const static ulong NFLAGS = 8;

      const bool EXTRA_RULES;
      const bool NOISY_RULES;

      Pool *const pool;
      const Markedup &markedup;
      const Cat *const SbNPbSbNP;
      const Cat *const SbNPfSbNP;
      const Cat *const SbS;
      const Cat *const SfS;
      Unify unify;

      Rules(Pool *pool, const Markedup &markedup, const bool EXTRA_RULES, const bool NOISY_RULES)
	: EXTRA_RULES(EXTRA_RULES), NOISY_RULES(NOISY_RULES),
	  pool(pool), markedup(markedup),
	  SbNPbSbNP(markedup["(S\\NP)\\(S\\NP)"]),
  	  SbNPfSbNP(markedup["(S\\NP)/(S\\NP)"]),
	  SbS(markedup["S\\S"]), SfS(markedup["S/S"]) {};

      bool operator()(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, 
		      bool tb_rules, bool questions, SuperCats &results);
      bool operator()(const SuperCat *left, const SuperCat *right,
		      const SuperCat *TBleft, const SuperCat *TBright, const SuperCat *TBres,
		      const bool eisner_nf, SuperCats &results);

      static ulong flags(const SuperCat *sc){
	switch(sc->cat->atom){
	  case Atoms::NONE:
	    if(sc->cat->is_fwd())
	      return FORWARD;
	    else
	      return BACKWARD;
       	  case Atoms::CONJ: return CONJ;
	  case Atoms::PERIOD: return PERIOD;
	  case Atoms::COLON: return COLON;
     	  case Atoms::SEMICOLON: return COLON;  // sc: added this for new treebank
	  case Atoms::COMMA: return COMMA;
      	  case Atoms::LQU: /* fall through */   //sc: quotes appear in TREC questions;
	  case Atoms::RQU: /* fall through */   //just use BRACKET flag
	  case Atoms::LRB: /* fall through */ 
	  case Atoms::RRB: return BRACKET;
	  default: return NONE;
	}
      }

      // sc: added Eisner's constraint
      bool forward_app_reject(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf){
        return sc1->cat->not_fwd() || sc2->conj_or_tr() ||
	  (eisner_nf && (sc1->fcomp() || sc1->tr()));
      }

      bool forward_app(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res);
      bool gen_forward_app(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, const bool eisner_nf,
			   SuperCats &res);

      // sc: added Eisner's constraint (note we do allow sc2->bxcomp)
      bool backward_app_reject(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf){
        return sc2->cat->not_bwd() || sc1->conj_or_tr() ||
	  (eisner_nf && (sc2->bcomp() || sc2->tr()));
      }

      bool backward_app(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res);
      bool gen_backward_app(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, const bool eisner_nf,
			    SuperCats &res);

      const Cat *unify_result(const Cat *fixed, const Cat *current);

      bool forward_comp_recursive(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf,
				  SuperCats &res);
      
      bool forward_comp_reject(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf){
        return sc1->cat->not_fwd() ||
               sc2->cat->not_fwd() ||
	  //Eisner's constraint
	  (eisner_nf && sc1->fcomp()) ||
	  //TODO do we need this - conj cats are always X\X?
	  sc1->conj() || sc2->conj();
      }

      bool forward_comp(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res);
      bool gen_forward_comp(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, const bool eisner_nf,
			    SuperCats &res);

      //sc: putting the sc2->cat->is_argNorNP() restriction on loses a
      //small amount coverage (0.1\% on 00) and small accuracy (0.1
      //f-score); should find out why, since this speeds up the parser
      //a lot
      bool backward_comp_reject(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf){
        return sc1->cat->not_bwd() || sc2->cat->not_bwd() ||
	  //Eisner's constraint
	  (eisner_nf && (sc2->bcomp() || sc2->bxcomp() || (sc1->tr() && !sc2->tr()))) ||
	  sc1->conj() || sc2->conj() || sc2->cat->is_argNorNP();
      }

      bool backward_comp(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res);
      bool gen_backward_comp(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb,
			     const bool eisner_nf, SuperCats &res);

      bool backward_comp_recursive(const SuperCat *sc1, const SuperCat *sc2,
				    const bool eisner_nf, SuperCats &res);

      bool backward_cross_reject(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf){
        return sc1->cat->not_fwd() || sc2->cat->not_bwd() ||
	  //Eisner's constraint
	  (eisner_nf && (sc2->bcomp() || sc2->bxcomp())) ||
	  sc1->conj() || sc2->conj() || sc2->cat->is_argNorNP();
      }

      bool backward_cross_recursive(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf,
				    SuperCats &res);

      bool backward_cross(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf,
			  SuperCats &res);
      bool gen_backward_cross(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb,
			      const bool eisner_nf, SuperCats &res);

      bool conj_reject(const SuperCat *sc1, const SuperCat *sc2){
        return !sc1->cat->is_conj() || sc2->conj_or_tr();
      }

      bool conj(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res);

      bool left_comma_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc2->cat;
	return sc1->cat->is_comma() && 
	  (cat->is_NorNP() || cat->is_Sdcl() || cat->is_SfS() ||
	   cat->is_SbNP() ||
	   cat->is_SbNPbSbNP() || cat->is_SbNPfSbNP() ||
	   cat->is_NPbNP() || cat->is_NfN() || cat->is_SbS() ||
	   (EXTRA_RULES && (cat->is_S() || cat->is_PPbPP() || cat->is_VPbVPbVPbVP())));
      }
      
      bool left_colon_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc2->cat;
	return sc1->cat->is_semiORcolon() && //sc: changed this to handle semicolon too 
	  (cat->is_NorNP() || cat->is_Sdcl() || cat->is_NPbNP() ||
	   cat->is_SbNPbSbNP() || cat->is_SbNP());
      }

      bool left_LRB_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc2->cat;
	return sc1->cat->is_LRB() && 
	  (cat->is_NorNP() || cat->is_Sdcl() || cat->is_NPbNP() ||
	   cat->is_SbNPbSbNP());
      }

      bool left_LQU_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc2->cat;
	return sc1->cat->is_LQU() && 
	  (cat->is_NorNP() || cat->is_Sdcl() || cat->is_SbNP());
      }

      //sc: added some rules for quotes (represented as LQU and RQU);
      //these are used for TREC question parsing
      bool left_punct_accept(const SuperCat *sc1, const SuperCat *sc2){
	return left_comma_accept(sc1, sc2) || left_colon_accept(sc1, sc2) ||
	  left_LRB_accept(sc1, sc2) || left_LQU_accept(sc1, sc2);
      }

      bool left_punct(const SuperCat *sc1, const SuperCat *sc2, bool tb_rules,
		      SuperCats &res){
	if(tb_rules){
	  if(!sc1->cat->is_punct())
	    return false;
	}
        else if(!left_punct_accept(sc1, sc2))
          return false;

	// inherit the CONJ from the right supercat
        SCatFlags inherited = sc2->flags & SuperCat::CONJ;
	res.push_back(SuperCat::Punct(pool, sc2->cat, SuperCat::LEFT_PUNCT | inherited, sc1, sc2, sc2));
	
        return true;
      };

      bool left_comma_typechange(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res);

      bool right_period_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc1->cat;
	return sc2->cat->is_period() && 
	  (cat->is_NorNP() || cat->is_S() || cat->is_SbNP() ||
	   cat->is_SbS() || cat->is_SbNPbSbNP() ||
	   (EXTRA_RULES && (cat->is_NPbNP() || cat->is_SdclbSbNP() ||
			 cat->is_PP() || cat->is_SbPP())));
      }

      bool right_comma_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc1->cat;
	return sc2->cat->is_comma() && 
	  (cat->is_NorNP() || cat->is_SbNP() || cat->is_SfS() ||
	   cat->is_SbNPbSbNP() || cat->is_NfN() || cat->is_SdclbNPfS() ||
	   cat->is_NPbNP() || cat->is_Sdcl() || 
	   (EXTRA_RULES && (cat->is_SdclbSdclbNP() || cat->is_SbS() ||
			  cat->is_SbNPfSbNP() || cat->is_SdclbNPfNP() ||
			  cat->is_NPbNPfSdclbNP() || cat->is_SdclbNPfPP() ||
			  cat->is_PP())));
      }

      bool right_colon_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc1->cat;
	return sc2->cat->is_semiORcolon() && 
	  (cat->is_NorNP() || cat->is_NPbNP() || cat->is_Sdcl() || cat->is_SdclbNP() || 
	   cat->is_SdclbNPfSdcl() || cat->is_SbNPbSbNP() || 
	   (EXTRA_RULES && (cat->is_SfS() || cat->is_PP() || cat->is_SbNPfSbNP() ||
			 cat->is_SbNP())));
      }

      bool right_RRB_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc1->cat;
	return sc2->cat->is_RRB() && 
	  (cat->is_NorNP() || cat->is_Sdcl() || cat->is_SdclbNP() || 
	   cat->is_SbNPbSbNP() || cat->is_NPbNP() || cat->is_NbN() || cat->is_NfN() ||
	   (EXTRA_RULES && (cat->is_SfS() || cat->is_NfNbNfN() ||
			 cat->is_SbS() || cat->is_SbNPfSbNP())));
      }

      bool right_RQU_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc1->cat;
	return sc2->cat->is_RQU() &&
	  (cat->is_NorNP() || cat->is_Sdcl() || cat->is_SbNP());
      }

      bool right_punct_accept(const SuperCat *sc1, const SuperCat *sc2){
	return right_comma_accept(sc1, sc2) || right_period_accept(sc1, sc2) ||
	       right_colon_accept(sc1, sc2) || right_RRB_accept(sc1, sc2) ||
	  right_RQU_accept(sc1, sc2);
      }

      bool right_punct(const SuperCat *sc1, const SuperCat *sc2, bool tb_rules,
		       SuperCats &res){
	if(tb_rules){
	  if(!sc2->cat->is_punct())
	    return false;
	}
	else if(!right_punct_accept(sc1, sc2))
          return false;

        // inherit the CONJ from the left supercat
	SCatFlags inherited = sc1->flags & SuperCat::CONJ;
	res.push_back(SuperCat::Punct(pool, sc1->cat, SuperCat::RIGHT_PUNCT | inherited, sc1, sc2, sc1));

        return true;
      };

      bool right_comma_typechange(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res);

      bool left_punct_conj_accept(const SuperCat *sc1, const SuperCat *sc2){
	const Cat *cat = sc2->cat;
	switch(sc1->cat->atom){
	  case Atoms::COMMA:
	    return cat->is_NorNP() || cat->is_S() || cat->is_SbNP() ||
	      (EXTRA_RULES && !sc2->conj() && (cat->is_NPbNP() || cat->is_NfN() || cat->is_SbNPbSbNP()));
	  case Atoms::SEMICOLON:
	      return cat->is_NP() || cat->is_S() || cat->is_SbNP();
	  default:
            return false;
        }
      }
      
      bool left_punct_conj(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res){
        if(!left_punct_conj_accept(sc1, sc2))
          return false;

        Cat *cat = Cat::Complex(pool, sc2->cat, BWD, sc2->cat, Vars::NONE);
        res.push_back(SuperCat::Conj(pool, cat, SuperCat::LEFT_PUNCT | SuperCat::CONJ, sc1, sc2));
        return true;
      };

      bool appositions(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res);

      bool funny_conj(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res);
      bool gen_conj(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res);
      bool gen_left_punct(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res);
      bool gen_right_punct(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res);
      bool gen_misc(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res);
      bool gen_right_comma_typechange(const SuperCat *sc1, const SuperCat *sc2,
				      const Cat *tb, SuperCats &res);

      bool gen_appositions(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res);
    };

  }
}
