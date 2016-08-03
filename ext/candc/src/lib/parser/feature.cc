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

#include "parser/fixed.h"
#include "parser/feature.h"

using namespace std;

namespace NLP { namespace CCG {

uchar
Feature::_convert(const char *s){
  switch(s[0]){
    case 'X': return Features::X;
    case 'a':
      if(s[1] == 'd')
        return Features::ADJ;
      else if(s[2] == 'u')
        return Features::ASUP;
      else
        return Features::AS;
    case 'b':
      if(s[1] == 'e')
	return Features::BEM;
      else
	return Features::B;
    case 'd': return Features::DCL;
    case 'e': 
      if(s[1] == 'm')
	return Features::EM;
      else
	return Features::EXPL;
    case 'f':
      if(s[1] == 'o')
        return Features::FOR;
      else
        return Features::FRG;
    case 'i': 
      if(s[2] == 't')
	return Features::INTJ;
      else
	return Features::INV;
    case 'n':
      switch(s[1]){
        case 'b': return Features::NB;
        case 'g': return Features::NG;
        case 'u': return Features::NUM;
      }
    case 'p':
      switch(s[1]){
        case 'o': return Features::POSS;
        case 's': return Features::PSS;
        case 't': return Features::PT;
      }
      break;
    case 'q':
      if(s[1] == 'e')
        return Features::QEM;
      else
        return Features::Q;
    case 't':
      if(s[1] == 'h')
	return Features::THR;
      else
	return Features::TO;
    case 'w': return Features::WQ;
  default: throw NLP::Exception("unexpected feature value '" + string(s) + "'");
  }
  return Features::NONE;
}

Feature::operator const char *(void) const {
  switch(_value){
    case Features::NONE: return "+";
    case Features::X: return "X";
    case Features::ADJ: return "adj";
    case Features::AS: return "as";
    case Features::ASUP: return "asup";
    case Features::B: return "b";
    case Features::BEM: return "bem";
    case Features::DCL: return "dcl";
    case Features::EM: return "em";
    case Features::EXPL: return "expl";
    case Features::FOR: return "for";
    case Features::FRG: return "frg";
    case Features::INTJ: return "intj";
    case Features::INV: return "inv";
    case Features::NB: return "nb";
    case Features::NG: return "ng";
    case Features::NUM: return "num";
    case Features::POSS: return "poss";
    case Features::PSS: return "pss";
    case Features::PT: return "pt";
    case Features::Q: return "q";
    case Features::QEM: return "qem";
    case Features::TO: return "to";
    case Features::THR: return "thr";
    case Features::WQ: return "wq";
    default: assert(!"illegal feature value");
  }
  return 0;
}

} }
