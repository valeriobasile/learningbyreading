// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "utils.h"

#include "pool.h"

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"
#include "parser/markedup.h"
#include "parser/variable.h"
#include "parser/gr_constraints.h"
#include "parser/gr.h"
#include "parser/relation.h"
#include "parser/dependency.h"
#include "parser/filled.h"
#include "parser/relations.h"
#include "parser/canonical.h"
#include "parser/categories.h"
#include "parser/supercat.h"
#include "parser/unify.h"
#include "parser/rule.h"

using namespace std;

namespace NLP { namespace CCG {

const static ulong MAX_DEPTH = 6;

ulong rule_calls = 0;
ulong rule_nvars = 0;  
ulong rule_fcomp = 0;  
ulong rule_fapp = 0; 
ulong rule_bcross = 0; 
ulong rule_bapp = 0; 
ulong rule_bcomp = 0;  
ulong rule_conj = 0;  
ulong rule_funny = 0;  
ulong rule_leftpunct = 0;
ulong rule_leftpunctconj = 0;
ulong rule_rightpunct = 0;  
ulong rule_fails = 0;  


bool Rules::operator()(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, 
		       bool tb_rules, bool, SuperCats &results){
  assert(results.size() <= results.capacity());

  ulong f1 = flags(sc1);
  ulong f2 = flags(sc2);

  ++rule_calls;

  if(sc1->nvars > 9 || sc2->nvars > 9){
    ++rule_nvars;
    return false;
  }

  switch(f1*NFLAGS + f2){
  case NONE*NFLAGS + NONE:
    return appositions(sc1, sc2, results);
  case NONE*NFLAGS + BACKWARD:
    return backward_app(sc1, sc2, eisner_nf, results);
  case NONE*NFLAGS + PERIOD:   /* fall through */
  case NONE*NFLAGS + COLON:    /* fall through */
  case NONE*NFLAGS + BRACKET:  /* fall through */
  case FORWARD*NFLAGS + PERIOD: /* fall through */
  case FORWARD*NFLAGS + COLON:  /* fall through */
  case FORWARD*NFLAGS + BRACKET:  /* fall through */
  case BACKWARD*NFLAGS + PERIOD:   /* fall through */
  case BACKWARD*NFLAGS + COLON:    /* fall through */
  case BACKWARD*NFLAGS + BRACKET:
    return right_punct(sc1, sc2, tb_rules, results);

  case NONE*NFLAGS + COMMA:    /* fall through */
  case FORWARD*NFLAGS + COMMA:  /* fall through */
  case BACKWARD*NFLAGS + COMMA:    /* fall through */
    right_comma_typechange(sc1, sc2, results);
    return right_punct(sc1, sc2, tb_rules, results);

  case FORWARD*NFLAGS + NONE: /* fall through */
  case FORWARD*NFLAGS + CONJ:
    return forward_app(sc1, sc2, eisner_nf, results);
  case FORWARD*NFLAGS + FORWARD:
    return forward_comp(sc1, sc2, eisner_nf, results) || forward_app(sc1, sc2, eisner_nf, results);
  case FORWARD*NFLAGS + BACKWARD:
    return backward_app(sc1, sc2, eisner_nf, results) || forward_app(sc1, sc2, eisner_nf, results)
      || backward_cross(sc1, sc2, eisner_nf, results);
  case BACKWARD*NFLAGS + BACKWARD:
    return backward_comp(sc1, sc2, eisner_nf, results) || backward_app(sc1, sc2, eisner_nf, results);
  case CONJ*NFLAGS + NONE:
    return conj(sc1, sc2, results) && funny_conj(sc1, sc2, results);
  case CONJ*NFLAGS + FORWARD:  /* fall through */
  case CONJ*NFLAGS + BACKWARD: // sc: added the second condition to deal with conj conj\conj
    return conj(sc1, sc2, results) || backward_app(sc1, sc2, eisner_nf, results);
  case COLON*NFLAGS + NONE:  /* fall through */
  case COLON*NFLAGS + FORWARD: /* fall through */
  case COLON*NFLAGS + BACKWARD:  /* fall through */
  case BRACKET*NFLAGS + NONE:  /* fall through */
  case BRACKET*NFLAGS + FORWARD: /* fall through */
  case BRACKET*NFLAGS + BACKWARD:
    return left_punct(sc1, sc2, tb_rules, results) && left_punct_conj(sc1, sc2, results);
  case COMMA*NFLAGS + NONE:  /* fall through */
  case COMMA*NFLAGS + FORWARD: /* fall through */
  case COMMA*NFLAGS + BACKWARD:  /* fall through */
    left_comma_typechange(sc1, sc2, results); 
    return left_punct(sc1, sc2, tb_rules, results) && left_punct_conj(sc1, sc2, results);
  default: return false;
  }
}

bool Rules::operator()(const SuperCat *sc1, const SuperCat *sc2, const SuperCat *, const SuperCat *,
		       const SuperCat *TBres, const bool eisner_nf, SuperCats &results){
  ulong f1 = flags(sc1);
  ulong f2 = flags(sc2);

  switch(f1*NFLAGS + f2){
  case NONE*NONE + NONE:
    if(gen_appositions(sc1, sc2, TBres->cat, results))
      return true;
    break;
  case NONE*NFLAGS + BACKWARD:
    if(gen_backward_app(sc1, sc2, TBres->cat, eisner_nf, results))
      return true;
    break;
  case NONE*NFLAGS + PERIOD:   /* fall through */
  case NONE*NFLAGS + COLON:    /* fall through */
  case NONE*NFLAGS + BRACKET:  /* fall through */
  case FORWARD*NFLAGS + PERIOD: /* fall through */
  case FORWARD*NFLAGS + COLON:  /* fall through */
  case FORWARD*NFLAGS + BRACKET:  /* fall through */
  case BACKWARD*NFLAGS + PERIOD:   /* fall through */
  case BACKWARD*NFLAGS + COLON:    /* fall through */
  case BACKWARD*NFLAGS + BRACKET:
    if(gen_right_punct(sc1, sc2, TBres->cat, results))
      return true;
    break;

  case NONE*NFLAGS + COMMA:    /* fall through */
  case FORWARD*NFLAGS + COMMA:  /* fall through */
  case BACKWARD*NFLAGS + COMMA:    /* fall through */
    if(gen_right_comma_typechange(sc1, sc2, TBres->cat, results) ||
       gen_right_punct(sc1, sc2, TBres->cat, results))
      return true;
    break;
  case FORWARD*NFLAGS + NONE: /* fall through */
  case FORWARD*NFLAGS + CONJ:
    if(gen_forward_app(sc1, sc2, TBres->cat, eisner_nf, results))
      return true;
    break;
  case FORWARD*NFLAGS + FORWARD:
    if(gen_forward_comp(sc1, sc2, TBres->cat, eisner_nf, results) || 
       gen_forward_app(sc1, sc2, TBres->cat, eisner_nf, results))
      return true;
    break;
  case FORWARD*NFLAGS + BACKWARD:
    if(gen_backward_app(sc1, sc2, TBres->cat, eisner_nf, results) || 
       gen_forward_app(sc1, sc2, TBres->cat, eisner_nf, results) ||
       gen_backward_cross(sc1, sc2, TBres->cat, eisner_nf, results))
      return true;
    break;
  case BACKWARD*NFLAGS + BACKWARD:
    if(gen_backward_comp(sc1, sc2, TBres->cat, eisner_nf, results) ||
       gen_backward_app(sc1, sc2, TBres->cat, eisner_nf, results))
      return true;
    break;
  case CONJ*NFLAGS + NONE:
    if(gen_conj(sc1, sc2, TBres->cat, results))
      return true;
    break;
  case CONJ*NFLAGS + FORWARD:  /* fall through */
  case CONJ*NFLAGS + BACKWARD: // sc: added the second condition to deal with conj conj\conj
    if(gen_conj(sc1, sc2, TBres->cat, results) || gen_backward_app(sc1, sc2, TBres->cat, eisner_nf, results))
      return true;
    break;
  case COLON*NFLAGS + NONE:  /* fall through */
  case COLON*NFLAGS + FORWARD: /* fall through */
  case COLON*NFLAGS + BACKWARD:  /* fall through */
  case COMMA*NFLAGS + NONE:  /* fall through */
  case COMMA*NFLAGS + FORWARD: /* fall through */
  case COMMA*NFLAGS + BACKWARD:  /* fall through */ 
  case BRACKET*NFLAGS + NONE:  /* fall through */
  case BRACKET*NFLAGS + FORWARD: /* fall through */
  case BRACKET*NFLAGS + BACKWARD:
    if(gen_left_punct(sc1, sc2, TBres->cat, results))
      return true;
    break;
  }

  if(gen_conj(sc1, sc2, TBres->cat, results))
    return true;

  if(gen_left_punct(sc1, sc2, TBres->cat, results))
    return true;

  if(gen_right_punct(sc1, sc2, TBres->cat, results))
    return true;

  return gen_misc(sc1, sc2, TBres->cat, results);
}

bool
Rules::forward_app(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res){
  if(forward_app_reject(sc1, sc2, eisner_nf))
    return false;

  if(!unify(sc1->cat->arg, sc2->cat) || !unify(sc1, sc2))
    return false;

  unify.add_vars1(sc1->cat->res);

  sc1->cat->res->order(unify.trans1, unify.seen, unify.order);
  unify.reorder(sc1, sc2);

  Cat *rescat = Cat::Trans(pool, sc1->cat->res, unify.trans1, unify.feature);
  SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::FWD_APP, 0, sc1, sc2, unify);
  if(ressc)
    res.push_back(ressc);
  return true;
} 

bool
Rules::gen_forward_app(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, const bool eisner_nf, 
		       SuperCats &res){
  if(forward_app(sc1, sc2, eisner_nf, res) && res[0]->cat->uhash == tb->uhash)
    return true;
  if(!res.empty())
    res.pop_back();
  return false;
}

bool Rules::backward_app(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res){
  if(backward_app_reject(sc1, sc2, eisner_nf))
    return false;
  if(!unify(sc1->cat, sc2->cat->arg) || !unify(sc1, sc2))
    return false;

  unify.add_vars2(sc2->cat->res);
  sc2->cat->res->order(unify.trans2, unify.seen, unify.order);
  unify.reorder(sc1, sc2);

  Cat *rescat = Cat::Trans(pool, sc2->cat->res, unify.trans2, unify.feature);
  SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::BWD_APP, 0, sc1, sc2, unify);
  if(ressc)
    res.push_back(ressc);
  return true;
}

bool
Rules::gen_backward_app(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, const bool eisner_nf, 
			SuperCats &res){
  if(backward_app(sc1, sc2, eisner_nf, res) && res[0]->cat->uhash == tb->uhash)
    return true;
  if(!res.empty())
    res.pop_back();
  return false;
}

// TODO could put a depth limit on this
bool
Rules::forward_comp_recursive(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, 
			      SuperCats &res){
  const Cat *cat1 = sc1->cat;
  const Cat *cat2 = sc2->cat;

  if(!cat1->arg->is_SbNP() || cat2->res->not_fwd() || (eisner_nf && sc1->fcomp()))
    return false;

  if(cat2->res->res->uhash == cat1->arg->uhash){
    const Cat *at = cat2->res->res;
    if(at->res->has_var())
      return false;

    if(!unify(cat1->arg, at) || !unify(sc1, sc2))
      return false;

    const Cat *rescat = Cat::Insert12(pool, cat1->res, at, cat2, sc1->cat->var, sc1, sc2, unify);
    if(rescat){
      SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::FWD_COMP | SuperCat::RECURSIVE, 2, sc1, sc2, unify);
      if(ressc)
	res.push_back(ressc);
      return true;
    }else
      return false;
  }else if(cat2->res->res->is_fwd() && 
	   cat2->res->res->res->uhash == cat1->arg->uhash){
    const Cat *at = cat2->res->res->res;
    if(at->res->has_var())
      return false;

    if(!unify(cat1->arg, at) || !unify(sc1, sc2))
      return false;

    const Cat *rescat = Cat::Insert12(pool, cat1->res, at, cat2, sc1->cat->var, sc1, sc2, unify);
    if(rescat){
      SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::FWD_COMP | SuperCat::RECURSIVE, 3, sc1, sc2, unify);
      if(ressc)
	res.push_back(ressc);
      return true;
    }else
      return false;
  }

  return true;
}

bool
Rules::forward_comp(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res){
  if(forward_comp_reject(sc1, sc2, eisner_nf))
    return false;
  if(!unify(sc1->cat->arg, sc2->cat->res))
    return forward_comp_recursive(sc1, sc2, eisner_nf, res);
  if(!unify(sc1, sc2))
    return false;

  Cat *rescat = Cat::Join12(pool, sc1->cat->res, FWD, sc2->cat->arg, sc1->cat->var, sc1, sc2, unify);
  if(rescat){
    SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::FWD_COMP, 0, sc1, sc2, unify);
    if(ressc)
      res.push_back(ressc);
    return true;
  }else
    return false;
}

bool
Rules::gen_forward_comp(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, const bool eisner_nf, 
			SuperCats &res){
  if(forward_comp(sc1, sc2, eisner_nf, res) && res[0]->cat->uhash == tb->uhash)
    return true;
  if(!res.empty())
    res.pop_back();
  return false;
}

bool
Rules::backward_comp(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res){
  if(backward_comp_reject(sc1, sc2, eisner_nf))
    return false;
  if(!unify(sc1->cat->res, sc2->cat->arg))
    return backward_comp_recursive(sc1, sc2, eisner_nf, res);
  if(!unify(sc1, sc2))
    return false;

  // outer variable is taken from left
  // this is an arbitrary decision -- we haven't developed a rationale for left over right
  Cat *rescat = Cat::Join21(pool, sc2->cat->res, BWD, sc1->cat->arg, sc1->cat->var, sc1, sc2, unify);
  if(rescat){
    SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::BWD_COMP, 0, sc1, sc2, unify);
    if(ressc)
      res.push_back(ressc);
    return true;
  }else
    return false;
}

bool
Rules::gen_backward_comp(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, const bool eisner_nf, 
			 SuperCats &res){
  if(backward_comp(sc1, sc2, eisner_nf, res) && res[0]->cat->uhash == tb->uhash)
    return true;
  if(!res.empty())
    res.pop_back();
  return false;
}

bool
Rules::backward_cross_recursive(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res){
  const Cat *cat1 = sc1->cat;
  const Cat *cat2 = sc2->cat;

  if(!cat2->arg->is_SbNP() || cat1->res->not_fwd() || (eisner_nf && (sc2->bcomp() || sc2->bxcomp())))
    return false;

  if(cat1->res->res->uhash == cat2->arg->uhash){
    const Cat *at = cat1->res->res;
    if(at->res->has_var())
      return false;

    if(!unify(at, cat2->arg) || !unify(sc1, sc2))
      return false;

    // arbitrary decision to take variable from left rather than right
    const Cat *rescat = Cat::Insert21(pool, cat2->res, at, cat1, sc1->cat->var, sc1, sc2, unify);
    if(rescat){
      SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::BWD_CROSS | SuperCat::RECURSIVE, 2, sc1, sc2, unify);
      if(ressc)
	res.push_back(ressc);
      return true;
    }else
      return false;
  }else if(cat1->res->res->is_fwd() && 
	   cat1->res->res->res->uhash == cat2->arg->uhash){
    const Cat *at = cat1->res->res->res;
    if(at->res->has_var())
      return false;

    if(!unify(at, cat2->arg) || !unify(sc1, sc2))
      return false;

    const Cat *rescat = Cat::Insert21(pool, cat2->res, at, cat1, sc1->cat->var, sc1, sc2, unify);
    if(rescat){
      SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::BWD_CROSS | SuperCat::RECURSIVE, 3, sc1, sc2, unify);
      if(ressc)
	res.push_back(ressc);
      return true;
    }else
      return false;
  }

  return false;
}

bool
Rules::backward_cross(const SuperCat *sc1, const SuperCat *sc2, const bool eisner_nf, SuperCats &res){
  if(backward_cross_reject(sc1, sc2, eisner_nf))
    return false;
  if(!unify(sc1->cat->res, sc2->cat->arg))
    return backward_cross_recursive(sc1, sc2, eisner_nf, res);
  if(!unify(sc1, sc2))
    return false;
  
  // outer variable from left
  Cat *rescat = Cat::Join21(pool, sc2->cat->res, FWD, sc1->cat->arg, sc1->cat->var, sc1, sc2, unify);
  if(rescat){
    SuperCat *ressc = SuperCat::Rule(pool, rescat, SuperCat::BWD_CROSS, 0, sc1, sc2, unify);
    if(ressc)
      res.push_back(ressc);
    return true;
  }else
    return false;
}

bool
Rules::gen_backward_cross(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, const bool eisner_nf, 
			  SuperCats &res){
  if(backward_cross(sc1, sc2, eisner_nf, res) && res[0]->cat->uhash == tb->uhash)
    return true;
  if(!res.empty())
    res.pop_back();
  return false;
}

bool
Rules::backward_comp_recursive(const SuperCat *sc1, const SuperCat *sc2,
			       const bool, SuperCats &res){
  if(!EXTRA_RULES)
    return false;

  const Cat *cat1 = sc1->cat;

  // this rule is hard coded for only one case
  // (S[dcl]\S[dcl])\NP S\S --> (S[dcl]\S[dcl])\NP
  if(!cat1->res->is_SdclbSdcl())
    return false;

  // only want to apply this rule when the S\S dependency
  // can be filled, i.e. when the inner result S[dcl] has a head
  if(sc1->vars[cat1->res->res->var].is_unfilled())
    return false;

  if(!unify(cat1->res->res, sc2->cat->arg) || !unify(sc1, sc2))
    return false;

  unify.add_vars1(cat1);

  SuperCat *ressc = SuperCat::Rule(pool, sc1->cat, SuperCat::BWD_COMP | SuperCat::RECURSIVE, 2, sc1, sc2, unify);
  if(ressc)
    res.push_back(ressc);

  return true;
}

bool
Rules::conj(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res){
  if(conj_reject(sc1, sc2))
    return false;

  // TODO put this in conj_reject
  if(sc2->cat->is_punct())
    return false;

  Cat *cat = Cat::Complex(pool, sc2->cat, BWD, sc2->cat, Vars::NONE);
  res.push_back(SuperCat::Conj(pool, cat, SuperCat::CONJ, sc1, sc2));
  return true;
}

bool
Rules::left_comma_typechange(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res){
  if(!EXTRA_RULES)
    return false;
  
  if(!sc1->cat->is_comma())
    return false;

  if(sc2->cat->is_NP()){
    res.push_back(SuperCat::Special(pool, SbNPbSbNP, SuperCat::LEFT_TC, sc1, sc2, sc2, 56));
    return true;
  }

  return false;
}

// TODO: funny conj allows cases like this through:
// John likes and cars - need to block this
// but funny conj needed because of the way some coordinations
// analysed in the Penn Treebank (and hence CCGBank)
bool
Rules::funny_conj(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res){
  if(!NOISY_RULES)
    return false;

  if(!sc1->cat->is_conj() || !sc2->cat->is_N())
    return false;

  res.push_back(SuperCat::Punct(pool, sc2->cat, SuperCat::FUNNY_CONJ, sc1, sc2, sc2));
  return true;
}

bool
Rules::gen_conj(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res){
  if(conj_reject(sc1, sc2))
    return false;

  if(tb->is_bwd() &&
     tb->res->uhash == sc2->cat->uhash &&
     tb->arg->uhash == sc2->cat->uhash){
    Cat *cat = Cat::Complex(pool, sc2->cat, BWD, sc2->cat, Vars::NONE);
    res.push_back(SuperCat::Conj(pool, cat, SuperCat::CONJ, sc1, sc2));
    return true;
  }

  // funny conj rule for generator
  if(sc2->cat->uhash == tb->uhash){
    res.push_back(SuperCat::Punct(pool, sc2->cat, SuperCat::FUNNY_CONJ, sc1, sc2, sc2));
    return true;
  }

  return false;
}

bool
Rules::gen_left_punct(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res){
  if(sc1->cat->is_punct()){

    // inherit conf flag from right child
    SCatFlags inherited = sc2->flags & SuperCat::CONJ;

    if(sc2->cat->uhash == tb->uhash){
      res.push_back(SuperCat::Punct(pool, sc2->cat, SuperCat::LEFT_PUNCT | inherited, sc1, sc2, sc2));
      return true;
    }else if(tb->is_bwd() &&
	     sc2->cat->uhash == tb->res->uhash &&
	     sc2->cat->uhash == tb->arg->uhash){
      Cat *cat = Cat::Complex(pool, sc2->cat, BWD, sc2->cat, Vars::NONE);
      res.push_back(SuperCat::Conj(pool, cat, SuperCat::LEFT_PUNCT | SuperCat::CONJ, sc1, sc2));
      return true;
    }else if(sc1->cat->is_comma() && sc2->cat->is_NP() && tb->is_SbNPbSbNP()){
      // paired with left_comma_typechange for the comma cases
      res.push_back(SuperCat::Special(pool, SbNPbSbNP, SuperCat::LEFT_TC, sc1, sc2, sc2, 56));
      return true;
    }

    return false;
  }

  return false;
}

bool
Rules::right_comma_typechange(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res){
  if(!EXTRA_RULES)
    return false;

  if(!sc2->cat->is_comma())
    return false;

  const Cat *cat1 = sc1->cat;
  if(cat1->is_NP()){
    res.push_back(SuperCat::TypeChange(pool, SfS, SuperCat::RIGHT_TC, sc1, sc2, false, 55));
    return true;
  }

  if(cat1->is_SdclfSdcl()){
    res.push_back(SuperCat::TypeChange(pool, SfS, SuperCat::RIGHT_TC, sc1, sc2, true, 50));
    res.push_back(SuperCat::TypeChange(pool, SbNPfSbNP, SuperCat::RIGHT_TC, sc1, sc2, true, 51));
    res.push_back(SuperCat::TypeChange(pool, SbNPbSbNP, SuperCat::RIGHT_TC, sc1, sc2, true, 52));
    res.push_back(SuperCat::TypeChange(pool, SbS, SuperCat::RIGHT_TC, sc1, sc2, true, 53));
    return true;
  }

  if(cat1->is_SdclbSdcl() && !sc1->conj()){
    res.push_back(SuperCat::TypeChange(pool, SfS, SuperCat::RIGHT_TC, sc1, sc2, true, 54));
    return true;
  }

  return false;
}

bool
Rules::gen_right_comma_typechange(const SuperCat *sc1, const SuperCat *sc2,
				  const Cat *tb, SuperCats &res){
  if(!sc2->cat->is_comma())
    return false;

  const Cat *cat1 = sc1->cat;
  if(cat1->is_NP()){
    if(tb->is_SfS()){
      res.push_back(SuperCat::TypeChange(pool, SfS, SuperCat::RIGHT_TC, sc1, sc2, false, 55));
      return true;
    }

    return false;
  }

  if(cat1->is_SdclfSdcl()){
    if(tb->is_SfS()){
      res.push_back(SuperCat::TypeChange(pool, SfS, SuperCat::RIGHT_TC, sc1, sc2, true, 50));
      return true;
    }

    if(tb->is_SbNPfSbNP()){
      res.push_back(SuperCat::TypeChange(pool, SbNPfSbNP, SuperCat::RIGHT_TC, sc1, sc2, true, 51));
      return true;
    }

    if(tb->is_SbNPbSbNP()){
      res.push_back(SuperCat::TypeChange(pool, SbNPbSbNP, SuperCat::RIGHT_TC, sc1, sc2, true, 52));
      return true;
    }

    if(tb->is_SbS()){
      res.push_back(SuperCat::TypeChange(pool, SbS, SuperCat::RIGHT_TC, sc1, sc2, true, 53));
      return true;
    }

    return false;
  }

  if(cat1->is_SdclbSdcl()){
    if(tb->is_SfS()){
      res.push_back(SuperCat::TypeChange(pool, SfS, SuperCat::RIGHT_TC, sc1, sc2, true, 54));
      return true;
    }

    return false;
  }
  return false;
}

bool
Rules::gen_right_punct(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res){
  if(sc2->cat->is_punct()){
    const Cat *cat1 = sc1->cat;

    SCatFlags inherited = sc1->flags & SuperCat::CONJ;

    if(cat1->uhash == tb->uhash){
      res.push_back(SuperCat::Punct(pool, cat1, SuperCat::RIGHT_PUNCT | inherited, sc1, sc2, sc1));
      return true;
    }else if(tb->is_SfS()){
      if(cat1->is_NP()){
	res.push_back(SuperCat::Special(pool, SfS, SuperCat::RIGHT_TC | inherited, sc1, sc2, sc1, 13));
	return true;
      }

      if(cat1->is_SdclbSdcl()){
	res.push_back(SuperCat::Special(pool, SfS, SuperCat::RIGHT_TC | inherited, sc1, sc2, sc1, 15));
	return true;
      }

      return false;
    }else if(tb->is_SbS()){
      if(cat1->is_SdclfSdcl()){
	res.push_back(SuperCat::Special(pool, SbS, SuperCat::RIGHT_TC | inherited, sc1, sc2, sc1, 16));
	return true;
      }

      return false;
    }else if(tb->is_SbNPbSbNP()){
      if(cat1->is_SdclfSdcl()){
	res.push_back(SuperCat::Special(pool, SbNPbSbNP, SuperCat::RIGHT_TC | inherited, sc1, sc2, sc1, 19));
	return true;
      }

      return false;
    }else if(tb->is_SbNPfSbNP()){
      if(cat1->is_SfS() && cat1->res->has_dcl() && cat1->arg->has_dcl()){
	res.push_back(SuperCat::Special(pool, SbNPfSbNP, SuperCat::RIGHT_TC | inherited, sc1, sc2, sc1, 20));
	return true;
      }

      return false;
    }
  }

  return false;
}

bool
Rules::gen_misc(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res){
  if(sc1->cat->is_N() && sc2->cat->is_N() && tb->is_N()){
    res.push_back(SuperCat::Punct(pool, sc2->cat, SuperCat::GEN_MISC, sc1, sc2, sc2));
    return true;
  }

  if(sc1->cat->uhash == tb->uhash && sc2->cat->is_SbS()){
    res.push_back(SuperCat::Punct(pool, sc1->cat, SuperCat::GEN_MISC, sc1, sc2, sc1));
    return true;
  }

  if(sc2->cat->uhash == tb->uhash && sc1->cat->is_SfS()){
    res.push_back(SuperCat::Punct(pool, sc2->cat, SuperCat::GEN_MISC, sc1, sc2, sc2));
    return true;
  }

  return false;
}

bool
Rules::appositions(const SuperCat *sc1, const SuperCat *sc2, SuperCats &res){
  if(!EXTRA_RULES || !NOISY_RULES)
    return false;

  if(sc1->cat->is_NP()){
    if(sc2->cat->is_NP()){
      res.push_back(SuperCat::Apposition(pool, SuperCat::APPO, sc1, sc2));
      return true;
    }
    return false;
  }

  if(sc1->cat->is_Sdcl()){
    if(sc2->cat->is_Sdcl()){
      res.push_back(SuperCat::Apposition(pool, SuperCat::APPO, sc1, sc2));
      return true;
    }
    return false;
  }

  return false;
}

bool
Rules::gen_appositions(const SuperCat *sc1, const SuperCat *sc2, const Cat *tb, SuperCats &res){
  if(sc1->cat->is_NP()){
    if(sc2->cat->is_NP() && tb->is_NP()){
      res.push_back(SuperCat::Apposition(pool, SuperCat::APPO, sc1, sc2));
      return true;
    }
    return false;
  }

  if(sc1->cat->is_Sdcl()){
    if(sc2->cat->is_Sdcl() && tb->is_Sdcl()){
      res.push_back(SuperCat::Apposition(pool, SuperCat::APPO, sc1, sc2));
      return true;
    }
    return false;
  }

  return false;
}

}}
