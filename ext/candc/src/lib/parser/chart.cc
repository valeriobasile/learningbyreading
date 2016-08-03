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
#include "parser/gr_constraints.h"
#include "parser/gr.h"
#include "parser/relation.h"
#include "parser/variable.h"
#include "parser/dependency.h"
#include "parser/filled.h"
#include "parser/markedup.h"
#include "parser/relations.h"
#include "parser/canonical.h"
#include "parser/categories.h"
#include "parser/supercat.h"
#include "parser/unify.h"
#include "parser/cell.h"
#include "parser/equiv.h"
#include "parser/treebank.h"
#include "parser/chart.h"

using namespace std;

namespace NLP { namespace CCG {

Chart::Chart(Categories &cats, bool EXTRA_RULES, ulong MAXWORDS)
  : EXTRA_RULES(EXTRA_RULES),
    MAXWORDS(MAXWORDS), MAXCELLS(MAXWORDS*MAXWORDS/2 + MAXWORDS/2 + 1),
    pool(new Pool(1 << 22)),
    equiv("equiv"), cells(new Cell[MAXCELLS]), cats(cats),
    nwords(0), ncells(0),
    
    NP(cats.markedup["NP"]), NbN(cats.markedup["N\\N"]), NPbNP(cats.markedup["NP\\NP"]),
    SbS(cats.markedup["S\\S"]), SfS(cats.markedup["S/S"]),
    SbNPbSbNP(cats.markedup["(S\\NP)\\(S\\NP)"]),
    SbNPfSbNP(cats.markedup["(S\\NP)/(S\\NP)"]), SfSbSfS(cats.markedup["(S/S)\\(S/S)"]),
    SbNPbSbNPbSbNPbSbNP(cats.markedup["((S\\NP)\\(S\\NP))\\((S\\NP)\\(S\\NP))"]),
    NPfNPbNP(cats.markedup["NP/(NP\\NP)"]){}

void
Chart::set_constraint(const Constraint &c){
	int pos = c.pos;
	int span = c.span;
	cell(pos, span).set(c);

	if(c.exclude)
		return;

	int p = pos - 1;
	int start = 2;
	int height = span - 1;
	while(p >= 0){
		for(int s = 0; s < height; ++s)
			cell(p, start + s).exclude = true;
		--p;
		++start;
	}

	p = pos + 1;
	start = span;
	height = nwords - pos - span;
	while(p < pos + span){
		for(int s = 0; s < height; ++s)
			cell(p, start + s).exclude = true;
		++p;
		--start;
	}
}

bool
Chart::load(const Sentence &sent, double BETA, bool repair,
	    bool lexTR, bool qu_parsing){
  if(repair)
    mark();
  else
    reset();

  nwords = sent.words.size();
  if(nwords > MAXWORDS)
    throw NLP::ParseError("number of words is larger than MAXWORDS");

  ncells = (nwords + 1)*nwords/2 + 1;

	for(Constraints::const_iterator i = sent.constraints.begin();
			i != sent.constraints.end(); ++i)
		set_constraint(**i);

  for(ulong i = 0; i < nwords; ++i){
    const MultiRaw &multi = sent.msuper[i];
    double prob_cutoff = multi[0].score*BETA;

    for(MultiRaw::const_iterator j = multi.begin(); j != multi.end(); ++j){
      if(j->score < prob_cutoff)
        continue;

      const Cat *cat = cats.markedup[j->raw];
      if(!cat)
        throw NLP::ParseError("attempted to load category without markedup " + j->raw);

      create(pool, i, cat);
    }
    if(lexTR){
      lex(i, 1, qu_parsing);
      tr(i, 1);
    }
  }
  return true;
}

ulong
Chart::build_tree(const TBSentence &sentence, ulong &node, ulong pos){
  const TBNode &current = sentence[node];
  if(current.is_internal()){
    ulong span = build_tree(sentence, ++node, pos);
    if(current.nchildren == 2)
      span += build_tree(sentence, ++node, pos + span);
    const Cat *cat = cats.parse(current.cat.c_str());
    add(pos, span, SuperCat::Lexical(pool, 0, cat, 0));
    return span;
  }else if(current.is_leaf()){
    const Cat *cat = cats.parse(current.cat.c_str());
    add(pos, 1, SuperCat::Lexical(pool, pos, cat, 0));
    return 1;
  }else
    assert(!"expected LEAF or INTERNAL node");

  return 0;
}

bool
Chart::load(const TBSentence &sentence){
  reset();

  nwords = 0;
  for(TBSentence::const_iterator i = sentence.begin(); i != sentence.end(); ++i)
    if(i->is_leaf())
      nwords++;

  if(nwords > MAXWORDS)
    throw NLP::ParseError("number of words is larger than MAXWORDS");

  ncells = nwords*nwords/2 + nwords/2 + 1;

  ulong node = 0;
  ulong pos = 0;
  ulong span = build_tree(sentence, node, pos);

// TODO: why do we get one sentence where span != nwords?
//  if(span != nwords){
//    cout << "warning: build_node span of " << span << " != nwords " << nwords << endl;
//    copy(sentence.begin(), sentence.end(), ostream_iterator<TBNode>(cout, " "));
//    cout << endl;
//  }

   if(span != nwords)
     return false;

   return true;
}

bool
Chart::gen_tr(SuperCat *TBsc, SuperCat *TBscRes, Cell &cell){
  SuperCat *sc = cell.back();
  const Cat *TBcat = TBsc->cat;
  const Cat *TBres = TBscRes->cat; 
  const TRCats *trcats = 0;

  if(TBcat->is_NP()){
    trcats = &cats.trNP;
    for(TRCats::const_iterator j = trcats->begin(); j != trcats->end(); ++j)
      // TODO doesn't quite work but okay for now
      if((*j).cat->uhash == TBres->uhash){
        cell.add(SuperCat::TypeRaise(pool, *j, SuperCat::TR, sc, 11));
        return true;
      }
    return false;
  }
  if(TBcat->is_PP()){
    trcats = &cats.trPP;
    for(TRCats::const_iterator j = trcats->begin(); j != trcats->end(); ++j)
      if((*j).cat->uhash == TBres->uhash){
        cell.add(SuperCat::TypeRaise(pool, *j, SuperCat::TR, sc, 11));
        return true;
      }
    return false;
  }
  if(TBcat->is_AP()){
    trcats = &cats.trAP;
    for(TRCats::const_iterator j = trcats->begin(); j != trcats->end(); ++j)
      if((*j).cat->uhash == TBres->uhash){
        cell.add(SuperCat::TypeRaise(pool, *j, SuperCat::TR, sc, 11));
        return true;
      }
    return false;
  }
  if(TBcat->is_StobNP()){
    trcats = &cats.trVP_to;
    for(TRCats::const_iterator j = trcats->begin(); j != trcats->end(); ++j)
      if((*j).cat->uhash == TBres->uhash){
        cell.add(SuperCat::TypeRaise(pool, *j, SuperCat::TR, sc, 11));
        return true;
      }
    return false;
  }

  return false;
}

void
Chart::tr(Position pos, Position span){
  tmp.clear();
  Cell &current = cell(pos, span);

  for(ulong i = current.nold; i != current.size(); ++i){
    SuperCat *sc = current[i];
    const Cat *cat = sc->cat;

    const TRCats *trcats = 0;
    if(cat->is_NP())
      trcats = &cats.trNP;
    else if(cat->is_AP())
      trcats = &cats.trAP;
    else if(cat->is_PP())
      trcats = &cats.trPP;
    else if(cat->is_StobNP())
      trcats = &cats.trVP_to;
    else
      continue;

    for(TRCats::const_iterator j = trcats->begin(); j != trcats->end(); ++j){
      SuperCat *new_sc = SuperCat::TypeRaise(pool, *j, SuperCat::TR, sc, 11);
      if(equiv.add(pos, span, new_sc))
        tmp.add(new_sc);
    }
  }
  current.add(tmp);
}

void
Chart::_add_lex(Position pos, Position span, const Cat *cat, const SuperCat *sc, bool replace, RuleID ruleid){
  SuperCat *new_sc = SuperCat::LexRule(pool, cat, SuperCat::LEX, sc, replace, ruleid);
  if(equiv.add(pos, span, new_sc))
    tmp.add(new_sc);
}

bool
Chart::_add_genlex(Cell &cell, const Cat *cat, const SuperCat *sc, bool replace, RuleID ruleid){
  cell.add(SuperCat::LexRule(pool, cat, SuperCat::LEX, sc, replace, ruleid));
  return true;
}

void
Chart::lex(Position pos, Position span, bool qu_parsing){
  tmp.clear();
  Cell &current = cell(pos, span);
  for(Cell::iterator i = current.old(); i != current.end(); ++i){
    SuperCat *sc = *i;
    const Cat *cat = sc->cat;

    if(cat->is_N())
      _add_lex(pos, span, NP, sc, false, 1);
    else if(EXTRA_RULES && cat->is_NP())
      _add_lex(pos, span, NPfNPbNP, sc, false, 11);
    else if(cat->is_SbNP()){
      RuleID ruleid = 0;
      switch(cat->res->feature){
        case Features::DCL:
          if(!EXTRA_RULES)
            continue;
          ruleid = 12;
          break;
        case Features::PSS:
          if(EXTRA_RULES){
            _add_lex(pos, span, SbNPbSbNP, sc, false, 17);
            _add_lex(pos, span, SfS, sc, false, 13);
          }
          if(qu_parsing)
            _add_lex(pos, span, NbN, sc, true, 95);
          ruleid = 2;
          break;
        case Features::NG:
          _add_lex(pos, span, SbNPbSbNP, sc, false, 4);
          _add_lex(pos, span, SfS, sc, false, 5);
          if(EXTRA_RULES){
            _add_lex(pos, span, SbNPfSbNP, sc, false, 18);
            _add_lex(pos, span, SbS, sc, false, 16);
            _add_lex(pos, span, NP, sc, false, 20);
          }
          ruleid = 3;
          break;
        case Features::ADJ:
          // given this a 93 id to match the gen_lex rule
          _add_lex(pos, span, SbNPbSbNP, sc, false, 93);
          if(EXTRA_RULES)
            _add_lex(pos, span, SfS, sc, false, 15);
          if(qu_parsing)
            _add_lex(pos, span, NbN, sc, true, 94);
          ruleid = 6;
          break;          
        case Features::TO:
          _add_lex(pos, span, SbNPbSbNP, sc, false, 8);
          _add_lex(pos, span, NbN, sc, true, 9);
          if(EXTRA_RULES)
            _add_lex(pos, span, SfS, sc, false, 14);
          ruleid = 7;
          break;
        default:
          continue;
      }
      _add_lex(pos, span, NPbNP, sc, true, ruleid);
    }else if(cat->is_SfNP() && cat->res->has_dcl())
      _add_lex(pos, span, NPbNP, sc, true, 10);
    else if(EXTRA_RULES && cat->is_StobNPfNP())
      _add_lex(pos, span, NPbNP, sc, true, 19);
    else if(EXTRA_RULES && cat->is_Sdcl()){
      _add_lex(pos, span, NPbNP, sc, false, 21);
      _add_lex(pos, span, SbS, sc, false, 22);
    }
  }
  current.add(tmp);
}

bool
Chart::genlex(SuperCat *TBsc, SuperCat *TBscRes, Cell &cell){
  SuperCat *sc = cell.back();
  const Cat *cat = sc->cat;
  const Cat *TBcat = TBsc->cat;
  const Cat *TBres = TBscRes->cat;

  if(TBcat->is_N() && TBres->is_NP() && cat->is_N())
    return _add_genlex(cell, NP, sc, false, 1);

  if(TBcat->is_NP() && TBres->is_NPfNPbNP() && cat->is_NP())
    return _add_genlex(cell, NPfNPbNP, sc, false, 11);

  if(TBcat->is_SbNP() && cat->is_SbNP()){
    RuleID ruleid = 0;
    switch(TBcat->res->feature){
      case Features::DCL:
        ruleid = 12;
        break;
      case Features::PSS:
        if(TBres->is_SbNPbSbNP())
          return _add_genlex(cell, SbNPbSbNP, sc, false, 17);

        if(TBres->is_SfS())
          return _add_genlex(cell, SfS, sc, false, 13);

        ruleid = 2;
        break;
      case Features::NG:
        if(TBres->is_SbNPfSbNP())
          return _add_genlex(cell, SbNPfSbNP, sc, false, 18);
        if(TBres->is_SbNPbSbNP())
          return _add_genlex(cell, SbNPbSbNP, sc, false, 4);
        if(TBres->is_SfS())
          return _add_genlex(cell, SfS, sc, false, 5);
        if(TBres->is_SbS())
          return _add_genlex(cell, SbS, sc, false, 16);
        if(TBres->is_NP())
          return _add_genlex(cell, NP, sc, false, 20);

        ruleid = 3;
        break;
      case Features::ADJ:
        if(TBres->is_SbNPbSbNP())
          return _add_genlex(cell, SbNPbSbNP, sc, false, 93);
        if(TBres->is_SfS())
          return _add_genlex(cell, SfS, sc, false, 15);

        ruleid = 6;
        break;          
      case Features::TO:
        if(TBres->is_SbNPbSbNP())
          return _add_genlex(cell, SbNPbSbNP, sc, false, 8);
        if(TBres->is_NbN())
          return _add_genlex(cell, NbN, sc, true, 9);
        if(TBres->is_SfS())
          return _add_genlex(cell, SfS, sc, false, 14);
        ruleid = 7;
        break;
      default:
        ruleid = 90;
        break;
    }

    if(TBres->is_NPbNP())
      return _add_genlex(cell, NPbNP, sc, true, ruleid);
  }else if(TBcat->is_SfNP() && TBcat->res->has_dcl() &&
           cat->is_SfNP() && cat->res->has_dcl()){
    if(TBres->is_NPbNP())
      return _add_genlex(cell, NPbNP, sc, true, 10);
  }else if(TBcat->is_StobNPfNP() && cat->is_StobNPfNP()){
    if(TBres->is_NPbNP())
      return _add_genlex(cell, NPbNP, sc, true, 19);
  }else if(TBcat->is_Sdcl() && cat->is_Sdcl()){
    if(TBres->is_NPbNP())
      return _add_genlex(cell, NPbNP, sc, false, 21);
    if(TBres->is_SbS())
      return _add_genlex(cell, SbS, sc, false, 22);
  }

  // these are catch all rules which look for the resultcat
  if(TBres->is_NPbNP())
    return _add_genlex(cell, NPbNP, sc, false, 91);

  if(TBres->is_NbN())
    return _add_genlex(cell, NbN, sc, false, 92);

  if(TBres->is_SbNPbSbNP())
    return _add_genlex(cell, SbNPbSbNP, sc, false, 93);

  if(TBres->is_SbNPfSbNP())
    return _add_genlex(cell, SbNPfSbNP, sc, false, 94);

  if(TBres->is_SfS())
    return _add_genlex(cell, SfS, sc, false, 95);

  if(TBres->is_SbS())
    return _add_genlex(cell, SbS, sc, false, 96);
  
  if(TBres->is_bwd() && 
     TBres->res->is_SfS() && TBres->arg->is_SfS())
    return _add_genlex(cell, SfSbSfS, sc, false, 97);

  if(TBres->is_bwd() && 
     TBres->res->is_SbNPbSbNP() && TBres->arg->is_SbNPbSbNP())
    return _add_genlex(cell, SbNPbSbNPbSbNPbSbNP, sc, false, 98);

  return false;
}

void
Chart::dump(ostream &out) const {
  for(ulong span = 1; span <= nwords; ++span)
    for(ulong pos = 0; pos <= nwords - span; ++pos){
      const Cell &c = cell(pos, span);
      out << "CELL " << span << ',' << pos << ' ' << c.size() << endl;
      for(Cell::const_iterator i = c.begin(); i != c.end(); ++i){
	const SuperCat *equiv = *i;
	out << "  " << span << ',' << pos << ' '  << *equiv->cat;
	out << ' ' << equiv->nequiv() << endl;
      }
    } 
}

} }
