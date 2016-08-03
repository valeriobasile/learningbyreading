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
#include "parser/supercat.h"
#include "parser/unify.h"

using namespace std;

namespace NLP { namespace CCG {

bool
Unify::_unify(const Cat *c1, const Cat *c2){
  if(c1->atom){
    if(!c2->atom)
      return false;

    // we want NP and NP[nb] to unify so don't check features
    // now we also want to ignore features on N e.g. N[num]
    // so we generalise this to ignore features on everything
    // except for S e.g. S[dcl] etc
    if(c1->atom != Atoms::S)
      /* do nothing */;
    else if(c1->feature != Features::X){
      if(c2->feature != Features::X){
        if(c1->feature != c2->feature)
          return false;
      }else
        feature = c1->feature;
    }else
      feature = c2->feature;
  }else{
    if(c2->atom)
      return false;

    if(c1->flags != c2->flags)
      return false;
    if(c1->res->uhash != c2->res->uhash ||
       c1->arg->uhash != c2->arg->uhash)
      return false;

    if(!_unify(c1->res, c2->res) || !_unify(c1->arg, c2->arg))
      return false;
  }

  heads[c1->var][c2->var] = 1;
  if(max1 < c1->var)
    max1 = c1->var;

  if(max2 < c2->var)
    max2 = c2->var;

  // assumes that we don't have the case
  // ((S\NP{X*})/NP{X+})
  lrange1[c1->var] |= c1->lrange;
  lrange2[c2->var] |= c2->lrange;
  return true;
}

void
Unify::_matrix2trans(void){
  nvariables = 1;
  memset(trans1, 0, sizeof(trans1));
  memset(trans2, 0, sizeof(trans2));
  memset(old1, 0, sizeof(old1));
  memset(old2, 0, sizeof(old2));
  for(uchar i = 1; i <= max1; ++i)
    for(uchar j = 1; j <= max2; ++j)
      if(heads[i][j]){
        trans1[i] = nvariables;
        trans2[j] = nvariables;
        old1[nvariables] = i;
        old2[nvariables] = j;
        ++nvariables;
        if(nvariables > Vars::NVARS)
          throw NLP::Exception("too many variables created in unification");
      }
}

void
Unify::add_var(VarID var, VarID trans[], VarID old[]){
  if(var && !trans[var]){
    if(nvariables >= Vars::NVARS)
      throw NLP::Exception("too many variables added from result category");

    trans[var] = nvariables;
    old[nvariables] = var;
    ++nvariables;
  }
}

void
Unify::add_vars(const Cat *c1, VarID trans[], VarID old[]){
  add_var(c1->var, trans, old);
  if(c1->arg){
    add_vars(c1->arg, trans, old);
    add_vars(c1->res, trans, old);
  }
}

void
Unify::reorder(const SuperCat *sc1, const SuperCat *sc2){
  for(VarID i = 1; i < Vars::NVARS; ++i){
    // JC: here is the modification
    if(!seen[i]){
      if(sc1->vars[old1[i]].is_filled() || sc2->vars[old2[i]].is_filled())
        seen[i] = ++order;
    }
  }
  nvariables = static_cast<ulong>(order) + 1;

  /*
  memset(old1, 0, sizeof(old1));
  memset(old2, 0, sizeof(old2));

  for(VarID i = 1; i < Vars::NVARS; ++i){
    trans1[i] = seen[trans1[i]];
    trans2[i] = seen[trans2[i]];
    old1[trans1[i]] = i;
    old2[trans2[i]] = i;
  }
  */

  VarID tmp1[Vars::NVARS], tmp2[Vars::NVARS];

  memmove(tmp1, old1, sizeof(tmp1));
  memmove(tmp2, old2, sizeof(tmp2));

  memset(old1, 0, sizeof(old1));
  memset(old2, 0, sizeof(old2));

  for(VarID i = 1; i < Vars::NVARS; ++i){
    trans1[i] = seen[trans1[i]];
    trans2[i] = seen[trans2[i]];
    old1[seen[i]] = tmp1[i];
    old2[seen[i]] = tmp2[i];
  }
}

} }
