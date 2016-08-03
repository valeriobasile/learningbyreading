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
#include "parser/varid.h"

using namespace std;

namespace NLP { namespace CCG {

uchar
VarID::_convert(const char *s){
  switch(s[0]){
    case '+': return Vars::NONE;
    case '_': // underscore is for lexical filled variables
              // shares the same VarID as X
              // data/ccg/markedup cannot use the variable X 
    case 'X': return Vars::X;
    case 'Y': return Vars::Y;
    case 'Z': return Vars::Z;
    case 'W': return Vars::W;
    case 'V': return Vars::V;
    case 'U': return Vars::U;
    case 'T': return Vars::T;
    case 'R': return Vars::R;
    case 'Q': return Vars::Q;
    case 'A': return Vars::A;
    case 'B': return Vars::B;
    case 'C': return Vars::C;
    case 'D': return Vars::D;
    case 'E': return Vars::E;
    case 'F': return Vars::F;
    default: throw NLP::Exception("unexpected variable identifiers");
  }
  return Vars::NONE;
}

VarID::operator const char *(void) const {
  switch(_value){
    case Vars::NONE: return "+";
    case Vars::X: return "X";
    case Vars::Y: return "Y";
    case Vars::Z: return "Z";
    case Vars::W: return "W";
    case Vars::V: return "V";
    case Vars::U: return "U";
    case Vars::T: return "T";
    case Vars::R: return "R";
    case Vars::Q: return "Q";
    case Vars::A: return "A";
    case Vars::B: return "B";
    case Vars::C: return "C";
    case Vars::D: return "D";
    case Vars::E: return "E";
    case Vars::F: return "F";
    default: throw NLP::Exception("illegal variable identifier");
  }
  return 0;
}

} }
