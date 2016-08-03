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
#include "parser/cell.h"
#include "parser/equiv.h"
#include "parser/treebank.h"
#include "parser/chart.h"
#include "parser/generator.h"

using namespace std;

namespace NLP { namespace CCG {

bool
Generator::combine(const long j, const long i, const long k, ostream &log){
  SuperCat *left = chart(i, k - i).back();
  SuperCat *right = chart(k, j - k).back();
  SuperCat *TBres = TBchart(i, j - i).front();
  SuperCat *TBleft =  TBchart(i, k - i).back();
  SuperCat *TBright = TBchart(k, j - k).back();
 
  results.resize(0);
  rules(left, right, TBleft, TBright, TBres, false, results);
  
  if(results.size() == 0){
    log << nsentences << ":missing binary rule " << *left->cat << ' ' << *right->cat;
    log << ' ' << *TBleft->cat << ' ' << *TBright->cat << ' ' << *TBres->cat << endl;
    return false;
  }

  chart.add(i, j - i, results[0]);

  return true;
}

void
Generator::print_deps(ostream &out, const SuperCat *sc, char type,
		      const Raws &heads, const Raws &words){
  sc->print_filled(out, type, heads, words);
  if(sc->left)
    print_deps(out, sc->left, type, heads, words);
  if(sc->right)
    print_deps(out, sc->right, type, heads, words);
}

void
Generator::print_root(ostream &out, const Cat *cat, char type,
		      const Raws &values, const Variable *var){
  const Position *const end = var->fillers + Variable::NFILLERS;
  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;
 
    out << type << ' ' << *cat << ' ' << values[*p - 1] << '\n';
  }
}

void
Generator::print_deps_dist(ostream &out, const SuperCat *sc, char type,
			   const Raws &values, const Raws &tags, DistType dist){
  switch(dist){
    case WORDS: sc->print_filled_words(out, type, values, tags); break;
    case VERBS: sc->print_filled_verbs(out, type, values, tags); break;
    case PUNCT: sc->print_filled_punct(out, type, values, tags); break;
  }
  if(sc->left)
    print_deps_dist(out, sc->left, type, values, tags, dist);
  if(sc->right)
    print_deps_dist(out, sc->right, type, values, tags, dist);
}

void
Generator::print_deps(ostream &out, const SuperCat *sc, const Markedup &markedup,
		      const Relations &rels, const Raws &heads,
		      const Raws &words, const bool julia_slots){
  sc->print_filled(out, markedup, rels, heads, words, julia_slots);
  if(sc->left)
    print_deps(out, sc->left, markedup, rels, heads, words, julia_slots);

  if(sc->right)
    print_deps(out, sc->right, markedup, rels, heads, words, julia_slots);
}

void
Generator::print_rules(ostream &out, const SuperCat *sc, char utype, char btype){
  if(!sc->left)
    return;

  const bool outerbrack = true;
  if(sc->right){
    out << btype << ' ';
    sc->left->cat->out_novar(out, outerbrack);
    out << ' ';
    sc->right->cat->out_novar(out, outerbrack);
    out << ' ';
    sc->cat->out_novar(out, outerbrack);
    out << '\n';
  }
  else{
    out << utype << ' ';
    sc->left->cat->out_novar(out, outerbrack);
    out << ' ';
    sc->cat->out_novar(out, outerbrack);
    out << ' ';
    sc->cat->out_novar(out, outerbrack);
    out << '\n';
  }
  print_rules(out, sc->left, utype, btype);

  if(sc->right)
    print_rules(out, sc->right, utype, btype);
}

void
Generator::print_rule_types(ostream &out, const SuperCat *sc, char type){
  if(!sc->left)
    return;
  
  const bool outerbrack = true;
  ushort flags = SuperCat::GEN_RULES & sc->flags;
  out << type << ' ';
  sc->cat->out_novar(out, outerbrack);
  out << ' ' << flags << '\n';

  print_rule_types(out, sc->left, type);

  if(sc->right)
    print_rule_types(out, sc->right, type);
}

void
Generator::print_rheads(ostream &out, const Cat *cat1, const Cat *cat2, const Cat *cat3,
			char type, const Raws &values, const Variable *var){
  const Position *const end = var->fillers + Variable::NFILLERS;
  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    const bool outerbrack = true;
    out << type << ' ';
    cat1->out_novar(out, outerbrack);
    out << ' ';
    cat2->out_novar(out, outerbrack);
    out << ' ';
    cat3->out_novar(out, outerbrack);
    out << ' ' << values[*p - 1] << '\n';
  }
}

void
Generator::print_rules(ostream &out, const SuperCat *sc, char utype, char btype, const Raws &values){
  if(!sc->left)
    return;

  if(sc->right)
    print_rheads(out, sc->left->cat, sc->right->cat, sc->cat, btype, values, &sc->vars[sc->cat->var]);
  else
    print_rheads(out, sc->left->cat, sc->cat, sc->cat, utype, values, &sc->vars[sc->cat->var]);

  print_rules(out, sc->left, utype, btype, values);

  if(sc->right)
    print_rules(out, sc->right, utype, btype, values);
}

void
Generator::_print_rdeps(ostream &out, const Cat *cat1, const Cat *cat2, const Cat *cat3, char type, 
			const Variable *var1, const Variable *var2, const Raws &values1, 
			const Raws &values2){
  const Position *const end1 = var1->fillers + Variable::NFILLERS;
  const Position *const end2 = var2->fillers + Variable::NFILLERS;
  for(const Position *p = var1->fillers; p != end1 && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;
    
    for(const Position *q = var2->fillers; q != end2 && *q != Variable::SENTINEL; ++q){
      if(!*q)
        continue;

      const bool outerbrack = true;
      out << type << ' ';
      cat1->out_novar(out, outerbrack);
      out << ' ';
      cat2->out_novar(out, outerbrack);
      out << ' ';
      cat3->out_novar(out, outerbrack);
      out << ' ' << values1[*p - 1] << ' ' << values2[*q - 1] << '\n';
    }
  }
}

void
Generator::print_rules_deps(ostream &out, const SuperCat *sc, char type,
			    const Raws &values1, const Raws &values2){
  if(!sc->left)
    return;

  if(sc->right)
    _print_rdeps(out, sc->left->cat, sc->right->cat, sc->cat, type, &sc->left->vars[sc->left->cat->var], &sc->right->vars[sc->right->cat->var], values1, values2);
  
  print_rules_deps(out, sc->left, type, values1, values2);

  if(sc->right)
    print_rules_deps(out, sc->right, type, values1, values2);
}

void
Generator::_print_rdeps_dist(ostream &out, char, Position lpos, Position rpos,
			     const Raws &tags, DistType dist){
  ulong nverbs = 0;
  ulong npunct = 0;

  ulong start = lpos;
  ulong end = rpos;

  if(dist == WORDS){
    ulong words = end - start - 1;
    if(words > 2)
      words = 2;

    out << words << '\n';
    return;
  }

  if(dist == VERBS){
    for(ulong i = start + 1; i < end; ++i){
      if(tags[i][0] == 'V' && nverbs < 1)
	nverbs++;
    }
    out << nverbs << '\n';
    return;
  }

  if(dist == PUNCT){
    for(ulong i = start + 1; i < end; ++i){
      if((tags[i][0] == ',' || tags[i][0] == ':' || tags[i][0] == '.' || tags[i][0] == ';') 
	 && npunct < 2)
	npunct++;
    }
    out << npunct << '\n';
    return;
  } 
}

//TODO need argument order and variable names consistent with parser code

void
Generator::_print_rdeps_dist(ostream &out, const Cat *cat1, const Cat *cat2, const Cat *cat3, char type,
			     const Variable *hvar, const Variable *lvar, const Variable *rvar,
			     const Raws &values, const Raws &tags, DistType dist){
  const Position *const end = hvar->fillers + Variable::NFILLERS;
  const Position *const endl = lvar->fillers + Variable::NFILLERS;
  const Position *const endr = rvar->fillers + Variable::NFILLERS;

  for(const Position *p = hvar->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    for(const Position *l = lvar->fillers; l != endl && *l != Variable::SENTINEL; ++l){
      if(!*l)
        continue;
      for(const Position *r = rvar->fillers; r != endr && *r != Variable::SENTINEL; ++r){
        if(!*r)
	  continue;
	if(*p != *l && *p != *r)
	  continue;
	
	const bool outerbrack = true;
	out << type << ' ';
	cat1->out_novar(out, outerbrack);
	out << ' ';
	cat2->out_novar(out, outerbrack);
	out << ' ';
	cat3->out_novar(out, outerbrack);
	out << ' ' << values[*p - 1] << ' ';
	_print_rdeps_dist(out, type, *l - 1, *r - 1, tags, dist);
      }
    }
  }
}

void
Generator::print_ruledeps_dist(ostream &out, const SuperCat *sc, char type,
			       const Raws &values, const Raws &tags, DistType dist){
  if(!sc->left)
    return;

  if(sc->right)
    _print_rdeps_dist(out, sc->left->cat, sc->right->cat, sc->cat, type, &sc->vars[sc->cat->var], &sc->left->vars[sc->left->cat->var], &sc->right->vars[sc->right->cat->var], values, tags, dist);

  print_ruledeps_dist(out, sc->left, type, values, tags, dist);

  if(sc->right)
    print_ruledeps_dist(out, sc->right, type, values, tags, dist);
}

void
Generator::print_filled(std::ostream &out, const SuperCat *sc,
			const Raws &words, const bool julia_slots){
  sc->print_filled(out, cats.markedup, cats.relations, words, words, julia_slots);
  if(sc->left)
    print_filled(out, sc->left, words, julia_slots);
  if(sc->right)
    print_filled(out, sc->right, words, julia_slots);
}

void
Generator::print_filled(std::ostream &out, const SuperCat *sc){
  sc->print_filled(out);
  if(sc->left)
    print_filled(out, sc->left);
  if(sc->right)
    print_filled(out, sc->right);
}

bool
Generator::unary(Cell &TBcell, Cell &cell) {
  for(ulong i = 0; i < TBcell.size() - 1; ++i)
    if(!chart.genlex(TBcell[i], TBcell[i + 1], cell) &&
       !chart.gen_tr(TBcell[i], TBcell[i + 1], cell))
      return false;

  return true;
}

//used to print sentences for which we can produce the gold standard
void
Generator::print_sentence(std::ostream &out, const Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    if(i > 0)
      out << " ";
    out << sent.words[i] << '|' << sent.msuper[i][0].raw << '|' << sent.pos[i];
  }
  out << '\n';
}

// TODO fix the 0.0 BETA passed to the chart.load methods below

bool
Generator::parse(const vector<TBNode> &TBsentence){
  ++nsentences;
  if(!TBchart.load(TBsentence))
    return false;

  Sentence sent;
  
  convertTBsent(TBsentence, sent);
  try {
    chart.load(sent, 0.0, false, false, false); // final argument is used for question parsing
  }catch(ParseError e){
    // ignore sentences we can't load because of unknown supertags
    return false;
  }

  const long NWORDS = sent.words.size();
  for(long j = 2; j <= NWORDS; ++j){
    for(long i = j - 2; i >= 0; --i){

	if(TBchart(i, j - i).size() > 0){
	  for(long k = i + 1; k < j; ++k){
	    if(TBchart(i, k - i).size() > 0 && TBchart(k, j - k).size() > 0){

	      if(TBchart(i, k - i).size() > 1)
		if(!unary(TBchart(i, k - i), chart(i, k - i)))
		  return false;

	      if(TBchart(k, j - k).size() > 1)
		if(!unary(TBchart(k, j - k), chart(k, j - k)))
		  return false;
	      
	      if(!combine(j, i, k, cerr))
		return false;
	    }
	  }
	}
    }
  }
  return true;
}

bool
Generator::parse(const TBSentence &tb, const Sentence &sent, ostream &log){
  ++nsentences;
  if(!TBchart.load(tb)){
    log << nsentences << ":failed to load treebank chart" << endl;
    return false;
  }
  try {
    chart.load(sent, 0.0, false, false, false);
  }catch(NLP::Exception e){
    log << nsentences << ":failed to load parser chart " << e.msg << endl;
    return false;
  }

  const long NWORDS = sent.words.size();
  for(long j = 2; j <= NWORDS; ++j){
    for(long i = j - 2; i >= 0; --i){

	if(TBchart(i, j - i).size() > 0){
	  for(long k = i + 1; k < j; ++k){
	    if(TBchart(i, k - i).size() > 0 && TBchart(k, j - k).size() > 0){

	      if(TBchart(i, k - i).size() > 1)
		if(!unary(TBchart(i, k - i), chart(i, k - i))){
		  log << nsentences << ":missing unary rule " << *chart(i, k - i)[0]->cat << ' ';
		  log << *TBchart(i, k - i)[0]->cat << endl;
		  return false;
		}

	      if(TBchart(k, j - k).size() > 1)
		if(!unary(TBchart(k, j - k), chart(k, j - k))){
		  log << nsentences << ":missing unary rule " << *chart(k, j - k)[0]->cat << ' ';
		  log << *TBchart(k, j - k)[0]->cat << endl;
		  return false;
		}
	      
	      if(!combine(j, i, k, log)){
		return false;
	      }
	    }
	  }
	}
    }
  }
  log << nsentences << ":parse successful" << endl;
  return true;
}

}}
