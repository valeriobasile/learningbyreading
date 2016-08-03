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
#include "parser/atom.h"

using namespace std;

namespace NLP { namespace CCG {

uchar Atom::_convert(const char *s){
  switch(s[0]){
    case ':': return Atoms::COLON;
    case ',': return Atoms::COMMA;
    case 'c': return Atoms::CONJ;
    case 'L':
      if(s[1] == 'R')
	return Atoms::LRB;
      else
	return Atoms::LQU;
    case 'N':
      if(s[1] == 'P')
        return Atoms::NP;
      else
        return Atoms::N;
    case '.': return Atoms::PERIOD;
    case 'P': return Atoms::PP;
    case 'R':
      if(s[1] == 'R')
	return Atoms::RRB;
      else
	return Atoms::RQU;
    case 'S': return Atoms::S;
    case ';': return Atoms::SEMICOLON;
    default: throw NLP::Exception("unrecognised atom string");
  }
}

Atom::operator const char *(void) const {
  switch(_value){
    case Atoms::NONE: return "+";
    case Atoms::COLON: return ":";
    case Atoms::COMMA: return ",";
    case Atoms::CONJ: return "conj";
    case Atoms::LRB: return "LRB";
    case Atoms::LQU: return "LQU";
    case Atoms::N: return "N";
    case Atoms::NP: return "NP";
    case Atoms::PERIOD: return ".";
    case Atoms::PP: return "PP";
    case Atoms::RRB: return "RRB";
    case Atoms::RQU: return "RQU";
    case Atoms::S: return "S";
    case Atoms::SEMICOLON: return ";";
    default: assert(!"illegal atom value");
  }
  return 0;
}

const char *
Atom::prolog(void) const {
  switch(_value){
    case Atoms::COLON: return "colon";
    case Atoms::COMMA: return "comma";
    case Atoms::CONJ: return "conj";
    case Atoms::LRB: return "lrb";
    case Atoms::LQU: return "lqu";
    case Atoms::N: return "n";
    case Atoms::NP: return "np";
    case Atoms::PERIOD: return "period";
    case Atoms::PP: return "pp";
    case Atoms::RRB: return "rrb";
    case Atoms::RQU: return "rqu";
    case Atoms::S: return "s";
    case Atoms::SEMICOLON: return "semi";
    default: assert(!"illegal atom value in Prolog representation");
  }
  return 0;
}

} }
