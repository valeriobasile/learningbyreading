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

using namespace std;

namespace NLP { namespace CCG {

Variable::Variable(const Variable &v1, const Variable &v2){
  const Position *j = v1.fillers;
  const Position *k = v2.fillers;
  if(!*j)
    ++j;
  else if(!*k)
    ++k;
  
  const Position *const end = fillers + NFILLERS;
  for(Position *p = fillers; p != end; ++p){
    if(*j < *k){
      *p = *j;
      ++j;
    }else{
      *p = *k;
      ++k;
    }
  }
}

}}
