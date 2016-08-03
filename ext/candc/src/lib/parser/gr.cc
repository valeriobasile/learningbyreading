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

using namespace std;

namespace NLP { namespace CCG {

GRTemplate::GRTemplate(const Categories &cats, const std::string &cat,
		       ulong slot, const std::string &markedup)
  : markedup(markedup), ignore(false), other_rel(0),
    constrained(false), groups(cats.gr_constraints), con_cat(0), con_rel(0), next(0){

  for(const char *s = markedup.c_str(); *s; ++s){
    if(*s == '#')
      break;

    if(*s == '='){
      constrained = true;

      std::string val;
      for(++s; *s && *s != ' '; ++s)
	val += *s;

      if(islower(val[1])){
	if(!con_lex.empty())
	  throw NLP::Exception("lexical constraint has already been set for " + markedup);
	con_lex = '=' + val;
      }else{
	if(!_tmp_cat.empty())
	  throw NLP::Exception("category constraint has already been set for " + markedup);
	_tmp_cat = val;
      }

      if(!*s)
	break;
      continue;
    }

    fmt += *s;
    if(*s == '%'){
      if(!*++s)
	throw NLP::Exception("GR format expression ends with a single %");
      ulong oslot = 0;
      switch(*s){
      case '%':
      case 'f':
      case 'l':
	fmt += *s;
	continue;
      case '1':
      case '2':
      case '3':
	oslot = *s - '0';
	if(oslot == slot)
	  throw NLP::Exception(string("GR should not use own slot as field specifier %") + *s);
	other_rel = cats.relations.get(cat, oslot);
	fmt += 'o';
	continue;
      case 'c':
	fmt += 'c';
	con_rel = 1;
	continue;
      case 'k':
	fmt += 'c';
	con_rel = 2;
	continue;
      default:
	throw NLP::Exception(string("unrecognised GR field specifier %") + *s);
      }
    }
  }

  for(int i = fmt.length(); i != 0; --i)
    if(fmt[i - 1] != ' '){
      fmt.erase(i);
      break;
    }

  if(fmt == "ignore")
    ignore = true;
}

void
GRTemplate::set_cat(const Categories &cats){
  if(_tmp_cat.empty())
    return;

  con_cat = cats.markedup[_tmp_cat];
  if(!con_cat)
    throw NLP::Exception("constraint category " + _tmp_cat + " does not exist in markedup");
  if(con_rel)
    con_rel = cats.relations.get(cats.markedup.markedup(_tmp_cat), con_rel);
}

bool
GRTemplate::satisfy(const Sentence &sent, const SuperCat *, const Filled *dep) const {
  if(!constrained)
    return true;

  if(con_cat && *sent.cats[dep->filler - 1] != *con_cat)
    return false;

  if(!con_lex.empty()){
    std::string word = sent.words[dep->head - 1];
    lower(word);

    return groups(con_lex, word);
  }

  return true;
}

void
GRTemplate::get(GRs &grs, const std::string &format, const Sentence &sent,
		const SuperCat *, const Filled *dep, const Filled *other,
		const Filled *constraint) const {

  GR result;

  istringstream in(format);
  std::string argfmt, label;

  in >> result.label;

  while(in >> argfmt){
    Argument arg;

    if(argfmt[0] == '%'){
      switch(argfmt[1]){
      case 'f':
	arg.raw = sent.words[dep->filler - 1];
	arg.pos = dep->filler - 1;
	break;
      case 'l':
	arg.raw = sent.words[dep->head - 1];
	arg.pos = dep->head - 1;
	break;
      case 'o':
	arg.raw = sent.words[other->filler - 1];
	arg.pos = other->filler - 1;
	break;
      case 'c':
	arg.raw = sent.words[constraint->filler - 1];
	arg.pos = constraint->filler - 1;
	break;
      case '%':
	arg.raw = argfmt.substr(1);
	arg.pos = -1;
	break;
      default:
	throw NLP::Exception("format string for GR " + format + " has unknown format after %");
      }
    }else{
      arg.raw = argfmt;
      arg.pos = -1;
    }
    result.args.push_back(arg);
  }

  grs.push_back(result);
}

void
GRTemplate::get(GRs &grs, const Sentence &sent, const SuperCat *sc,
		const FilledDeps &seen, const Filled *dep) const {

  if(!satisfy(sent, sc, dep)){
    if(next)
      next->get(grs, sent, sc, seen, dep);
    return;
  }

  if(ignore)
    return;

  if(dep->rule){
    switch(dep->rule){
    case 1: break;
    case 11: break;
    case 3:
    case 7:
    case 12:
      get(grs, "xmod _ %f %l", sent, sc, dep, 0, 0);
      get(grs, "ncsubj %l %f _", sent, sc, dep, 0, 0);
      return;
    case 2:
      get(grs, "xmod _ %f %l", sent, sc, dep, 0, 0);
      get(grs, "ncsubj %l %f obj", sent, sc, dep, 0, 0);
      return;
    case 4:
    case 5:
    case 6:
    case 8:
    case 9:
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 18:
    case 19:
    case 93:
    case 94:
    case 95:
      get(grs, "xmod _ %f %l", sent, sc, dep, 0, 0);
      return;
    case 10:
      get(grs, "cmod _ %f %l", sent, sc, dep, 0, 0);
      get(grs, "dobj %l %f", sent, sc, dep, 0, 0);
      return;
    case 20:
      return;
    case 21:
    case 22:
      get(grs, "cmod _ %f %l", sent, sc, dep, 0, 0);
      return;
    }
  }

  if(other_rel){
    if(con_rel){
      for(FilledDeps::const_iterator j = seen.begin(); j != seen.end(); ++j)
	for(FilledDeps::const_iterator k = seen.begin(); k != seen.end(); ++k)
	  if((*j)->head == dep->head && (*j)->rel == other_rel &&
	     (*k)->head == dep->filler && (*k)->rel == con_rel)
	    get(grs, fmt, sent, sc, dep, *j, *k);
    }else{
      for(FilledDeps::const_iterator j = seen.begin(); j != seen.end(); ++j)
	if((*j)->head == dep->head && (*j)->rel == other_rel)
	  get(grs, fmt, sent, sc, dep, *j, 0);
    }
  }else if(con_rel){
    for(FilledDeps::const_iterator k = seen.begin(); k != seen.end(); ++k)
      if((*k)->head == dep->filler && (*k)->rel == con_rel)
	get(grs, fmt, sent, sc, dep, 0, *k);
  }else
    get(grs, fmt, sent, sc, dep, 0, 0);
}

} }
