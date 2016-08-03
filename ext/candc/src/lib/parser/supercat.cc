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

using namespace NLP;

void *
operator new(size_t size, Pool *pool) {
  return (void *)pool->alloc(size);
}

void *
operator new[](size_t size, Pool *pool) {
  return (void *)pool->alloc(size);
}

void
operator delete(void *, Pool *) { /* do nothing */ }

void
operator delete[](void *, Pool *) { /* do nothing */ }

namespace NLP { namespace CCG {

const Filled *
from_unfilled(Pool *pool, const Dependency *dep, const Variable *var,
	      CatID lrange, const Filled *next){

  const ConjFactor conj_factor = var->count_fillers();
  const Position *const end = var->fillers + Variable::NFILLERS;

  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p)
    next = new (pool) Filled(dep, *p, lrange, conj_factor, next);

  return next;
}

const Filled *
from_category(Pool *pool, RelID rel, Position head, const Variable *var, const Filled *next){
  const ConjFactor conj_factor = var->count_fillers();
  const Position *const end = var->fillers + Variable::NFILLERS;

  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p)
    next = new (pool) Filled(rel, head, *p, 0, 0, conj_factor, next);

  return next;
}

ulong SuperCat::nsupercats = 0;

SuperCat::SuperCat(Pool *pool, Position pos, const Cat *cat, SCatFlags flags):
    cat(cat), unfilled(Dependency::get(pool, pos, cat)), filled(0),
    flags(flags), nvars(cat->nvars()), nactive(nvars),
    vars(new (pool) Variable[nvars]), feature(Features::NONE), depth(0),
		pos(pos - 1), span(1), left(0), right(0), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  if(cat->var)
    vars[cat->var] = Variable(pos);
  ++nsupercats;
}
  
SuperCat *
SuperCat::Lexical(Pool *pool, Position pos, const Cat *cat, SCatFlags flags){
  return new (pool) SuperCat(pool, pos, cat, flags);
}

SuperCat::SuperCat(Pool *pool, const Cat *cat, SCatFlags flags, uchar depth,
                   const SuperCat *left, const SuperCat *right, Unify &unify):
    cat(cat), unfilled(0), filled(0),
    flags(flags), nvars(unify.nvariables), nactive(cat->nvars()),
    vars(new (pool) Variable[nvars]), feature(unify.feature), depth(depth),
    pos(left->pos), span(left->span + right->span),
		left(left), right(right), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  // this should never happen now since the SuperCat::Rule static method
  // should intercept these cases first
  if(nactive > nvars){
    cout << "left: " << *left << endl;
    cout << "right: " << *right << endl;
    cout << "left left: " << *left->left << endl;
    cout << "left left left: " << *left->left->left << endl;
    cout << "left left right: " << *left->left->right << endl;
    cout << "left left left left: " << *left->left->left->left << endl;
    cout << "left left left right: " << *left->left->left->right << endl;
    throw NLP::Exception("attempt to create excess active variables");
  }

  for(VarID i = 1; i < nvars; ++i){
    if(unify.old1[i] >= left->nvars || unify.old2[i] >= right->nvars){
      cerr << *left << *right;
      cerr << *cat << endl;
      throw NLP::Exception("attempt to access variables outside range");
    }
    vars[i] = Variable(left->vars[unify.old1[i]], right->vars[unify.old2[i]]);
  }

  Dependencies unfilled_deps;

  for(const Dependency *dep = left->unfilled; dep; dep = dep->next){
    ulong var = unify.trans1[dep->var];
    if(!var)
      continue;

    CatID lrange = unify.lrange2[unify.old2[var]];

    if(vars[var].is_filled())
      filled = from_unfilled(pool, dep, &vars[var], lrange, filled);
    else{
      Dependency *new_dep = new (pool) Dependency(*dep, var, lrange);
      unfilled_deps.push_back(new_dep);
    }
  }

  for(const Dependency *dep = right->unfilled; dep; dep = dep->next){
    ulong var = unify.trans2[dep->var];
    if(!var)
      continue;

    CatID lrange = unify.lrange1[unify.old1[var]];

    if(vars[var].is_filled())
      filled = from_unfilled(pool, dep, &vars[var], lrange, filled);
    else{
      Dependency *new_dep = new (pool) Dependency(*dep, var, lrange);
      unfilled_deps.push_back(new_dep);
    }
  }
  
  unfilled = Dependency::link(unfilled_deps);

  ++nsupercats;
}

SuperCat *
SuperCat::Rule(Pool *pool, const Cat *cat, SCatFlags flags, uchar depth,
	       const SuperCat *left, const SuperCat *right, Unify &unify){
  if(cat->nvars() > unify.nvariables)
    return 0;

  return new (pool) SuperCat(pool, cat, flags, depth, left, right, unify);
}

SuperCat::SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
                   const SuperCat *left, const SuperCat *right):
    cat(cat), unfilled(right->unfilled), filled(0),
    flags(flags), nvars(right->nvars), nactive(right->nactive),
    vars(new (pool) Variable[nvars]), feature(right->feature), depth(0),
    pos(left->pos), span(left->span + right->span),
    left(left), right(right), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  assert(nactive <= nvars);

  for(VarID i = 1; i < nvars; ++i)
    vars[i] = Variable(right->vars[i]);

  assert(left->vars[1].is_filled());

  const Variable *var = &right->vars[right->cat->var];
  if(var->is_filled()){
    const Position head = left->vars[1].pos();
    unfilled = new (pool) Dependency(head, Relations::conj1, cat->arg->var, 0,
				     const_cast<Dependency *>(unfilled));
  }

  ++nsupercats;
}

SuperCat *
SuperCat::Conj(Pool *pool, const Cat *cat, SCatFlags flags,
	       const SuperCat *left, const SuperCat *right){
  return new (pool) SuperCat(pool, cat, flags, left, right);
}

SuperCat::SuperCat(Pool *, const Cat *cat, SCatFlags flags,
                   const SuperCat *left, const SuperCat *right, const SuperCat *vars):
    cat(cat), unfilled(vars->unfilled), filled(0),
    flags(flags), nvars(vars->nvars), nactive(vars->nactive),
    vars(vars->vars), feature(vars->feature), depth(0),
    pos(left->pos), span(left->span + right->span),
    left(left), right(right), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  assert(nactive <= nvars);

  ++nsupercats;
}

SuperCat *
SuperCat::Punct(Pool *pool, const Cat *cat, SCatFlags flags,
		const SuperCat *left, const SuperCat *right, 
		const SuperCat *vars){
  return new (pool) SuperCat(pool, cat, flags, left, right, vars);
}

SuperCat::SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
                   const SuperCat *left, bool replace, RuleID rule):
    cat(cat), unfilled(0), filled(0), flags(flags),
    nvars(cat->nvars()), nactive(nvars),
    vars(new (pool) Variable[nvars]), feature(Features::NONE), depth(0),
    pos(left->pos), span(left->span),
    left(left), right(0), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  assert(nactive <= nvars);

  const Variable &head = left->vars[left->cat->var];

  if(replace){
    if(left->cat->arg)
      unfilled = Dependency::clone(pool, left->cat->arg->var, cat->arg->var, rule, left->unfilled);
  }else
    unfilled = Dependency::get(pool, head, cat, rule);

  if(cat->var)
    vars[cat->var] = head;

  ++nsupercats;
}


SuperCat *
SuperCat::LexRule(Pool *pool, const Cat *cat, SCatFlags flags,
		  const SuperCat *left, bool replace, RuleID rule){
  return new (pool) SuperCat(pool, cat, flags, left, replace, rule);
}


SuperCat::SuperCat(Pool *pool, const TRCat &trcat, SCatFlags flags,
                   const SuperCat *left, RuleID):
    cat(trcat.cat), unfilled(0), filled(0),
    flags(flags), nvars(trcat.cat->nvars()), nactive(nvars),
    vars(new (pool) Variable[nvars]), feature(Features::NONE), depth(0),
    pos(left->pos), span(left->span),
    left(left), right(0), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  assert(nactive <= nvars);

  ++nsupercats;

  if(trcat.lex)
    vars[trcat.lex] = left->vars[left->cat->var];
  if(trcat.dep == Vars::NONE)
    return;

  Dependencies unfilled_deps;
  
  for(const Dependency *dep = left->unfilled; dep; dep = dep->next){
    Dependency *new_dep = new (pool) Dependency(*dep, trcat.dep, dep->lrange);
    unfilled_deps.push_back(new_dep);
  }
  
  unfilled = Dependency::link(unfilled_deps);
}

SuperCat *
SuperCat::TypeRaise(Pool *pool, const TRCat &trcat, SCatFlags flags,
		    const SuperCat *left, RuleID rule){
  return new (pool) SuperCat(pool, trcat, flags, left, rule);
} 

SuperCat::SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
                   const SuperCat *left, const SuperCat *right,
                   const SuperCat *head_sc, RuleID rule):
    cat(cat), unfilled(0), filled(0), flags(flags),
    nvars(cat->nvars()), nactive(nvars),
    vars(new (pool) Variable[nvars]), feature(Features::NONE), depth(0),
    pos(left->pos), span(left->span + right->span),
    left(left), right(right), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  assert(nactive <= nvars);

  const Variable &head = head_sc->vars[head_sc->cat->var];
  unfilled = Dependency::get(pool, head, cat, rule);

  if(cat->var)
    vars[cat->var] = head;

  ++nsupercats;
}

SuperCat *
SuperCat::Special(Pool *pool, const Cat *cat, SCatFlags flags,
		  const SuperCat *left, const SuperCat *right,
		  const SuperCat *head_sc, RuleID rule){
  return new (pool) SuperCat(pool, cat, flags, left, right, head_sc, rule);
}

SuperCat::SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
                   const SuperCat *left, const SuperCat *right,
		   bool replace, RuleID rule):
    cat(cat), unfilled(0), filled(0), flags(flags),
    nvars(cat->nvars()), nactive(nvars),
    vars(new (pool) Variable[nvars]), feature(Features::NONE), depth(0),
    pos(left->pos), span(left->span + right->span),
    left(left), right(right), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  assert(nactive <= nvars);

  const Variable &head = left->vars[left->cat->var];

  if(replace){
    if(left->cat->arg)
      unfilled = Dependency::clone(pool, left->cat->arg->var, cat->arg->var, rule, left->unfilled);
  }else
    unfilled = Dependency::get(pool, head, cat, rule);

  if(cat->var)
    vars[cat->var] = head;

  ++nsupercats;
}

SuperCat *
SuperCat::TypeChange(Pool *pool, const Cat *cat, SCatFlags flags,
		     const SuperCat *left, const SuperCat *right,
		     bool replace, RuleID rule){
  return new (pool) SuperCat(pool, cat, flags, left, right, replace, rule);
}

SuperCat::SuperCat(Pool *pool, SCatFlags flags, const SuperCat *left, const SuperCat *right):
    cat(left->cat), unfilled(0), filled(0),
    flags(flags), nvars(2), nactive(nvars),
    vars(new (pool) Variable[nvars]), feature(Features::NONE), depth(0),
    pos(left->pos), span(left->span + right->span),
    left(left), right(right), next(0), max(0),
    marker(MARK_NONE), score(0.0), inside(0.0), outside(0.0), d_inside(0.0) {

  vars[1] = Variable(left->vars[1], right->vars[1]);

  ++nsupercats;
}

SuperCat *
SuperCat::Apposition(Pool *pool, SCatFlags flags, const SuperCat *left, const SuperCat *right){
  return new (pool) SuperCat(pool, flags, left, right);
}

void
SuperCat::print_filled(ostream &out) const {
  for(const Filled *dep = filled; dep; dep = dep->next)
    out << *dep << endl;
}

void 
SuperCat::print_filled(ostream &out, char type, const vector<string> &heads, 
		       const vector<string> &words) const {
  for(const Filled *dep = filled; dep; dep = dep->next){
    ulong ruleid = dep->rule;
    out << type << ' ' << heads[dep->head - 1] << ' ' << dep->rel << ' '
	<< words[dep->filler - 1] << ' ' << ruleid << ' ' << dep->lrange << '\n';
  } 
}

void
SuperCat::print_filled_words(ostream &out, char type, const vector<string> &words,
			     const vector<string> &) const {
  for(const Filled *dep = filled; dep; dep = dep->next){
    ulong start, end;
    if(dep->head < dep->filler){
      start = dep->head - 1;
      end = dep->filler - 1;
    }else{
      end = dep->head - 1;
      start = dep->filler - 1;
    }
    ulong nwords = end - start - 1;
    if(nwords > 2)
      nwords = 2;

    out << type << ' ' << words[dep->head - 1] << ' ' << dep->rel << ' '
	<< ulong(dep->rule) << ' ' << dep->lrange << ' ' << nwords << '\n';
  }
  return;
}

void
SuperCat::print_filled_verbs(ostream &out, char type, const vector<string> &words, 
			     const vector<string> &tags) const {
  for(const Filled *dep = filled; dep; dep = dep->next){
    ulong nverbs = 0;
    ulong start, end;
    if(dep->head < dep->filler){
      start = dep->head - 1;
      end = dep->filler - 1;
    }else{
      end = dep->head - 1;
      start = dep->filler - 1;
    }

    for(ulong i = start + 1; i < end; ++i){
      if(tags[i][0] == 'V' && nverbs < 1)
	nverbs++;
    }
    out << type << ' ' << words[dep->head - 1] << ' ' << dep->rel << ' ' << ulong(dep->rule) << ' ' << dep->lrange << ' ' << nverbs << '\n';
  }
  return;
}

void
SuperCat::print_filled_punct(ostream &out, char type, const vector<string> &words, 
			     const vector<string> &tags) const {
  for(const Filled *dep = filled; dep; dep = dep->next){
    ulong npunct = 0;
    ulong start, end;
    if(dep->head < dep->filler){
      start = dep->head - 1;
      end = dep->filler - 1;
    }else{
      end = dep->head - 1;
      start = dep->filler - 1;
    }

    for(ulong i = start + 1; i < end; ++i){
      if((tags[i][0] == ',' || tags[i][0] == ':' || tags[i][0] == '.' || tags[i][0] == ';') 
	 && npunct < 2)
	npunct++;
    }
    out << type << ' ' << words[dep->head - 1] << ' ' << dep->rel << ' ' << ulong(dep->rule) << ' ' << dep->lrange << ' ' << npunct << '\n';
  }
  return;
}


void 
SuperCat::print_filled(ostream &out, const Markedup &markedup, const Relations &rels,
		       const vector<string> &heads, const vector<string> &words,
		       const bool julia_slots) const {
  for(const Filled *dep = filled; dep; dep = dep->next){
    ulong ruleid = dep->rule;
    if(dep->lrange){
      out << heads[dep->head - 1] << '_' << static_cast<ulong>(dep->head) << ' ';
      rels[dep->rel].print_slot(out, julia_slots);
      out << ' ' << words[dep->filler - 1] << '_' << static_cast<ulong>(dep->filler)
	  << ' ' << ruleid << ' ' << *markedup[dep->lrange] << '\n';
    }else{
      out << heads[dep->head - 1] << '_' << static_cast<ulong>(dep->head) << ' ';
      rels[dep->rel].print_slot(out, julia_slots);
      out << ' ' << words[dep->filler - 1] << '_' << static_cast<ulong>(dep->filler) 
	  << ' ' << ruleid << '\n';
    }
  }
}

void 
SuperCat::get_grs(GRs &grs, const Relations &rels,
		  FilledDeps &seen, const Sentence &sent) const {
  for(const Filled *dep = filled; dep; dep = dep->next){
    const Relation &rel = rels[dep->rel];
    if(rel.gr)
      rel.gr->get(grs, sent, this, seen, dep);
    seen.push_back(dep);
  }
}

void 
SuperCat::get_filled(GRs &deps, const Relations &rels,
		     const Sentence &sent, const bool julia_slots) const {
  for(const Filled *dep = filled; dep; dep = dep->next){
    const Relation &rel = rels[dep->rel];
    GR gr;
    ostringstream label;
    rel.print_slot(label, julia_slots);
    gr.label = label.str();
    Argument arg(sent.words[dep->head - 1], dep->head - 1);
    gr.args.push_back(arg);
    arg.raw = sent.words[dep->filler - 1];
    arg.pos = dep->filler - 1;
    gr.args.push_back(arg);
    deps.push_back(gr);
  }
}

const char *
SuperCat::flags2str(void) const{
  if(SuperCat::TR & flags)
    return "tr";
  else if(SuperCat::LEX & flags)
    return "lex";
  else if(SuperCat::FWD_APP & flags)
    return "fa";
  else if(SuperCat::BWD_APP & flags)
    return "ba";
  else if(SuperCat::RECURSIVE & flags){
    if(SuperCat::FWD_COMP & flags)
      return "gfc";
    else if(SuperCat::BWD_COMP & flags)
      return "gbc";
    else if(SuperCat::BWD_CROSS & flags)
      return "gbx";
    else
      return "gother";
  }else if(SuperCat::FWD_COMP & flags)
    return "fc";
  else if(SuperCat::BWD_COMP & flags)
    return "bc";
  else if(SuperCat::BWD_CROSS & flags)
    return "bx";
  else if(SuperCat::LEFT_PUNCT & flags)
    return "lp";
  else if(SuperCat::RIGHT_PUNCT & flags)
    return "rp";
  else if(SuperCat::LEFT_TC & flags)
    return "ltc";
  else if(SuperCat::RIGHT_TC & flags)
    return "rtc";
  else if(SuperCat::CONJ & flags)
    return "conj";
  else if(SuperCat::FUNNY_CONJ & flags)
    return "funny";
  else if(SuperCat::APPO & flags)
    return "appo";
  else
    return "other";
}

std::ostream &
SuperCat::lex_info(std::ostream &stream) const{
  if(SuperCat::LEX & flags){
    stream << '\'';
    left->cat->out_novar(stream, false);
    stream << "\',";
  }
  return stream;
}

std::ostream &
SuperCat::conj_info(std::ostream &stream) const{
  if(SuperCat::CONJ & flags){
    stream << '\'';
    left->cat->out_novar(stream, false);
    stream << "\',\'";
    right->cat->out_novar(stream, false);
    stream << "\',";
  }
  return stream;
}

std::ostream &
SuperCat::out_boxer(std::ostream &out, Feature parent) const{
  if(SuperCat::LEX & flags){
    // lx is a special case!
    out << "lx(";
    cat->out_boxer(out, parent);
    out << ", ";
    left->cat->out_boxer(out, feature.override(parent));
    return out << ',';
  }else if(SuperCat::CONJ & flags){
    // resolve the ambiguous case of lp(conj(...)) versus conj(comma,
    if((flags & SuperCat::LEFT_PUNCT) && (SuperCat::CONJ & right->flags)){
      out << "lp(";
      // now fall through to the end like left punct
    }else if((flags & SuperCat::RIGHT_PUNCT) && (SuperCat::CONJ & left->flags)){
      out << "rp(";
      // now fall through to the end like left punct
    }else{
      // conj is a special case!
      out << "conj(";
      cat->out_boxer(out, parent);
      out << ", ";
      right->cat->out_boxer(out, feature.override(parent));
      return out << ',';
    }
  }else if(SuperCat::FWD_APP & flags)
    out << "fa(";
  else if(SuperCat::BWD_APP & flags)
    out << "ba(";
  else if(SuperCat::RECURSIVE & flags){
    if(SuperCat::FWD_COMP & flags)
      out << "gfc(";
    else if(SuperCat::BWD_COMP & flags)
      out << "gbc(";
    else if(SuperCat::BWD_CROSS & flags)
      out << "gbxc(";
    else
      assert(!"illegal recursive flag for combinator");
    cat->out_boxer(out, parent);
    return out << ", " << static_cast<ulong>(depth) << ',';
  }else if(SuperCat::FWD_COMP & flags)
    out << "fc(";
  else if(SuperCat::BWD_COMP & flags)
    out << "bc(";
  else if(SuperCat::BWD_CROSS & flags)
    out << "bxc(";
  else if(SuperCat::LEFT_PUNCT & flags)
    out << "lp(";
  else if(SuperCat::RIGHT_PUNCT & flags)
    out << "rp(";
  else if(SuperCat::LEFT_TC & flags)
    out << "ltc(";
  else if(SuperCat::RIGHT_TC & flags)
    out << "rtc(";
  else if(SuperCat::TR & flags)
    out << "tr(";
  else if(SuperCat::FUNNY_CONJ & flags)
    out << "funny(";
  else if(SuperCat::APPO & flags)
    out << "appo(";
  else
    assert(!"unrecognised combinator flag in Prolog printing");

  cat->out_boxer(out, parent);
  return out << ',';
}

ulong equiv_calls = 0;
ulong equiv_unary = 0;
ulong equiv_nactive = 0;
ulong equiv_cat = 0;
ulong equiv_vars = 0;
ulong equiv_deps = 0;
ulong equiv_ndeps = 0;
ulong equiv_true = 0;

std::string
equivalent_explain(const SuperCat *sc1, const SuperCat *sc2){
  if(sc1->nactive != sc2->nactive)
    return "sc->nactive";

  if(!vars_eq(sc1->vars, sc2->vars, sc1->nactive))
    return "sc->vars";

  if(sc1->unary() != sc2->unary())
    return "sc->unary()";
     
  if(*sc1->cat != *sc2->cat)
    return "sc->cat";

  return "equiv";
  }

} }
