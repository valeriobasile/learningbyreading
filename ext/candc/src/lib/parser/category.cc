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
#include "parser/gr_constraints.h"
#include "parser/gr.h"
#include "parser/relation.h"
#include "parser/variable.h"
#include "parser/dependency.h"
#include "parser/filled.h"
#include "parser/relations.h"
#include "parser/supercat.h"
#include "parser/unify.h"

using namespace std;

namespace NLP { namespace CCG {

Hash
Cat::uhash_basic(Atom atom){
  return Hash(static_cast<ulong>(atom));
}

Hash
Cat::rhash_basic(Atom atom, Feature feature){
  Hash h(static_cast<ulong>(atom));
  // ignore all features except on the S atom
  // where we also ignore NONE and the variable X
  if(atom == Atoms::S && feature > Features::X)
    h += static_cast<ulong>(feature);

  return h;
}

Hash
Cat::uhash_complex(const Cat *res, Slash slash, const Cat *arg){
  Hash h = res->uhash;
  h *= 2;
  h |= slash;
  h += arg->uhash;

  return h;
}

Hash
Cat::rhash_complex(const Cat *res, Slash slash, const Cat *arg){
  Hash h = res->rhash;
  h += arg->rhash;
  h *= 2;
  h |= slash;

  return h;
}

Hash
Cat::rhash_rebuild(void) const {
  if(atom)
    return rhash_basic(atom, feature);
  else
    return rhash_complex(res, flags & SLASH, arg);
}

CatFlags 
Cat::build_flags(const Cat *res, const Slash slash, const Cat *arg){
  CatFlags flags = slash;

  switch(res->atom){
  case Atoms::NONE:
    if(res->is_SbNP())
      flags |= RES_SbNP;
    break;
  case Atoms::N:
    flags |= RES_N;
    break;
  case Atoms::NP:
    flags |= RES_NP;
    break;
  case Atoms::S:
    flags |= RES_S;
    break;
  }

  switch(arg->atom){
  case Atoms::N:
    flags |= ARG_N;
    break;
  case Atoms::NP:
    flags |= ARG_NP;
    break;
  case Atoms::S:
    flags |= ARG_S;
    break;
  }

  return flags;
}

Cat::Cat(Atom atom, Feature feature, VarID var, RelID rel, CatID lrange):
    uhash(uhash_basic(atom)), atom(atom),
    feature(feature), flags(0), var(var), rel(rel), lrange(lrange),
    res(0), arg(0), rhash(rhash_basic(atom, feature)) {}

Cat *
Cat::Basic(Pool *pool, Atom atom, Feature feature, VarID var, RelID rel, CatID lrange){
  return new (pool) Cat(atom, feature, var, rel, lrange);
}

Cat::Cat(const Cat *res, Slash slash, const Cat *arg, VarID var):
    uhash(uhash_complex(res, slash, arg)),
    flags(build_flags(res, slash, arg)), var(var), rel(0), lrange(0), 
    res(res), arg(arg), rhash(rhash_complex(res, slash, arg)) {}

Cat *
Cat::Complex(Pool *pool, const Cat *res, Slash slash, const Cat *arg, VarID var){
  return new (pool) Cat(res, slash, arg, var);
}

Cat::Cat(const Cat *res, Slash slash, const Cat *arg, VarID var,
                   RelID rel, CatID lrange):
    uhash(uhash_complex(res, slash, arg)),
    flags(build_flags(res, slash, arg)), var(var), rel(rel), lrange(lrange), 
    res(res), arg(arg), rhash(rhash_complex(res, slash, arg)) {}

Cat *
Cat::Complex(Pool *pool, const Cat *res, Slash slash, const Cat *arg,
             VarID var, RelID rel, CatID lrange){
  return new (pool) Cat(res, slash, arg, var, rel, lrange);
}

Cat::Cat(Pool *pool, const Cat *other):
    uhash(other->uhash),
    atom(other->atom), feature(other->feature),
    flags(other->flags), var(other->var),
    rel(other->rel), lrange(other->lrange),
    res(other->res ? new (pool) Cat(pool, other->res) : 0),
    arg(other->arg ? new (pool) Cat(pool, other->arg) : 0),
    rhash(other->rhash) {}

Cat *
Cat::Clone(Pool *pool, const Cat *other){
  return new (pool) Cat(pool, other);
}

Cat::Cat(Pool *pool, const Cat *other, VarID trans[], Feature X):
    uhash(other->uhash), atom(other->atom),
    feature(other->feature == Features::X && uchar(X) != 0 ? X 
	    : other->feature),
    flags(other->flags),
    var(trans[other->var]),
    rel(other->rel), lrange(other->lrange),
    res(other->res ? new (pool) Cat(pool, other->res, trans, X) : 0),
    arg(other->res ? new (pool) Cat(pool, other->arg, trans, X) : 0),
    rhash(rhash_rebuild()) {}

Cat *
Cat::Trans(Pool *pool, const Cat *other, VarID trans[], Feature X){
  return new (pool) Cat(pool, other, trans, X);
}

Cat *
Cat::Join12(Pool *pool, const Cat *c1, Slash slash, const Cat *c2,
            VarID var1, const SuperCat *sc1, const SuperCat *sc2, Unify &unify){
  unify.add_vars1(c1);
  unify.add_vars2(c2);
  unify.add_var1(var1);

  c1->order(unify.trans1, unify.seen, unify.order);
  c2->order(unify.trans2, unify.seen, unify.order);

  if(var1 && !unify.seen[unify.trans1[var1]])
    unify.seen[unify.trans1[var1]] = ++unify.order;
  if(unify.order >= Vars::NVARS)
    throw NLP::Exception("run out of variables in join12");

  unify.reorder(sc1, sc2);

  var1 = unify.trans1[var1];

  c1 = Trans(pool, c1, unify.trans1, unify.feature);
  c2 = Trans(pool, c2, unify.trans2, unify.feature);
  return Complex(pool, c1, slash, c2, var1);
}


Cat *
Cat::Join21(Pool *pool, const Cat *c2, Slash slash, const Cat *c1,
            VarID var1, const SuperCat *sc1, const SuperCat *sc2, Unify &unify){
  unify.add_vars2(c2);
  unify.add_vars1(c1);
  unify.add_var1(var1);

  c2->order(unify.trans2, unify.seen, unify.order);
  c1->order(unify.trans1, unify.seen, unify.order);

  if(var1 && !unify.seen[unify.trans1[var1]])
    unify.seen[unify.trans1[var1]] = ++unify.order;
  if(unify.order >= Vars::NVARS)
    throw NLP::Exception("run out of variables in join21");

  unify.reorder(sc1, sc2);
  var1 = unify.trans1[var1];

  c1 = Trans(pool, c1, unify.trans1, unify.feature);
  c2 = Trans(pool, c2, unify.trans2, unify.feature);
  return Complex(pool, c2, slash, c1, var1);
}

Cat *
Cat::do_insert12(Pool *pool, const Cat *replace, const Cat *at, const Cat *current, Unify &unify){
  if(current == at)
    return Trans(pool, replace, unify.trans1, unify.feature);
  else if(current->res){
    const Cat *res = do_insert12(pool, replace, at, current->res, unify);
    const Cat *arg = do_insert12(pool, replace, at, current->arg, unify);
    VarID v = unify.trans2[current->var];
    return Complex(pool, res, current->slash(), arg, v, current->rel, current->lrange);
  }else
    return Trans(pool, current, unify.trans2, unify.feature);
}

static void
do_order2(const Cat *cat, const Cat *at, Unify &unify){
  if(cat == at)
    return;
  do_order2(cat->res, at, unify);
  cat->arg->order(unify.trans2, unify.seen, unify.order);
  if(cat->var && !unify.seen[unify.trans2[cat->var]])
    unify.seen[unify.trans2[cat->var]] = ++unify.order;
}

Cat *
Cat::Insert12(Pool *pool, const Cat *replace, const Cat *at, const Cat *root,
	      VarID var1, const SuperCat *sc1, const SuperCat *sc2, Unify &unify){

  unify.add_vars1(replace);
  for(const Cat *cat = root->res; cat != at; cat = cat->res){
    unify.add_vars2(cat->arg);
    unify.add_var2(cat->var);
  }
  unify.add_vars2(root->arg);
  unify.add_var1(var1);

  replace->order(unify.trans1, unify.seen, unify.order);
  do_order2(root->res, at, unify);
  root->arg->order(unify.trans2, unify.seen, unify.order);
  if(var1 && !unify.seen[unify.trans1[var1]])
    unify.seen[unify.trans1[var1]] = ++unify.order;
  if(unify.order >= Vars::NVARS)
    throw NLP::Exception("run out of variables in insert12");

  unify.reorder(sc1, sc2);

  const Cat *res = Cat::do_insert12(pool, replace, at, root->res, unify);
  const Cat *arg = Cat::do_insert12(pool, replace, at, root->arg, unify);
  var1 = unify.trans1[var1];

  return Complex(pool, res, root->slash(), arg, var1, root->rel, root->lrange);
}

Cat *
Cat::do_insert21(Pool *pool, const Cat *replace, const Cat *at, const Cat *current, Unify &unify){
  if(current == at)
    return Trans(pool, replace, unify.trans2, unify.feature);
  else if(current->res){
    const Cat *res = do_insert21(pool, replace, at, current->res, unify);
    const Cat *arg = do_insert21(pool, replace, at, current->arg, unify);
    VarID v = unify.trans1[current->var];
    return Complex(pool, res, current->slash(), arg, v, current->rel, current->lrange);
  }else
    return Trans(pool, current, unify.trans1, unify.feature);
}

static void
do_order1(const Cat *cat, const Cat *at, Unify &unify){
  if(cat == at)
    return;
  do_order1(cat->res, at, unify);
  cat->arg->order(unify.trans1, unify.seen, unify.order);
  if(cat->var && !unify.seen[unify.trans1[cat->var]])
    unify.seen[unify.trans1[cat->var]] = ++unify.order;
}

Cat *
Cat::Insert21(Pool *pool, const Cat *replace, const Cat *at, const Cat *root,
	      VarID var1, const SuperCat *sc1, const SuperCat *sc2, Unify &unify){

  unify.add_vars2(replace);
  for(const Cat *cat = root->res; cat != at; cat = cat->res){
    unify.add_vars1(cat->arg);
    unify.add_var1(cat->var);
  }
  unify.add_vars1(root->arg);
  unify.add_var1(var1);

  replace->order(unify.trans2, unify.seen, unify.order);
  do_order1(root->res, at, unify);
  root->arg->order(unify.trans1, unify.seen, unify.order);

  if(var1 && !unify.seen[unify.trans1[var1]])
    unify.seen[unify.trans1[var1]] = ++unify.order;
  if(unify.order >= Vars::NVARS)
    throw NLP::Exception("run out of variables in insert21");

  unify.reorder(sc1, sc2);

  const Cat *res = Cat::do_insert21(pool, replace, at, root->res, unify);
  const Cat *arg = Cat::do_insert21(pool, replace, at, root->arg, unify);
  var1 = unify.trans1[var1];

  return Complex(pool, res, root->slash(), arg, var1, root->rel, root->lrange);
}

std::ostream &
Cat::out(std::ostream &stream) const {
  if(atom){
    stream << atom;
    if(feature)
      stream << '[' << feature << ']';
  }else
    stream << '(' << *res << (slash() ? '/' : '\\') << *arg << ')';
  if(var)
    stream << '{' << var << (lrange ? "*}" : "}");
  if(rel)
    stream << '<' << rel << '>';

  return stream;
}

std::ostream &
Cat::out_novar(std::ostream &stream, const bool brack) const {
  if(atom){
    stream << atom;
    if(feature)
      stream << '[' << feature << ']';
  }else{
    if(brack)
      stream << '(';
    res->out_novar(stream, true);
    stream << (slash() ? '/' : '\\');
    arg->out_novar(stream, true);
    if(brack)
      stream << ')';
  }
  return stream;
}

inline
std::ostream &
out_feature(std::ostream &stream, Feature feature){
  if(feature && feature != Features::X)
    stream << '[' << feature << ']';
  return stream;
}

std::ostream &
Cat::out_novar_noX(std::ostream &stream, const bool brack) const {
  if(atom){
    stream << atom;
    out_feature(stream, feature);
  }else{
    if(brack)
      stream << '(';
    res->out_novar_noX(stream, true);
    stream << (slash() ? '/' : '\\');
    arg->out_novar_noX(stream, true);
    if(brack)
      stream << ')';
  }
  return stream;
}

std::ostream &
Cat::out_boxer(std::ostream &stream, Feature parent, const bool brack) const {
  if(atom){
    stream << atom.prolog();
    if(feature && feature.override(parent))
      stream << ':' << feature.override(parent);
  }else{
    if(brack)
      stream << '(';
    res->out_boxer(stream, parent, true);
    stream << (slash() ? '/' : '\\');
    arg->out_boxer(stream, parent, true);
    if(brack)
      stream << ')';
  }
  return stream;
}

std::ostream &
Cat::out_short(std::ostream &stream, const bool brack) const {
  if(atom){
    stream << atom;
    out_feature(stream, feature);
  }else if(is_SbNP()){
    stream << "VP";
    out_feature(stream, res->feature);
  }else{
    if(brack)
      stream << '(';
    res->out_short(stream, true);
    stream << (slash() ? '/' : '\\');
    arg->out_short(stream, true);
    if(brack)
      stream << ')';
  }
  return stream;
}

std::ostream &
Cat::out_js(std::ostream &stream) const {
  stream << "{ ";
  if(atom){
    stream << "'a': '" << atom << "', ";
    if(!feature.is_none())
      stream << "'f': '" << feature << "', ";
    stream << "'v': '" << var << "' }";
  }else if(is_SbNP()){
    stream << "'a': 'VP', ";
    if(!res->feature.is_none())
      stream << "'f': '" << res->feature << "', ";
    stream << "'v': '" << res->var << "' }";
  }else{
    stream << "'s': '" << (slash() ? "/" : "\\\\") << "', ";
    stream << "'res': ";
    res->out_js(stream);
    stream << ", 'arg': ";
    arg->out_js(stream);
    stream << ", 'v': '" << var << "' }";
  }
  return stream;
}

bool
_unequal(const Cat &c1, const Cat &c2){
  if(c1.atom){
    return c1.atom != c2.atom ||
           c1.var != c2.var ||
           c1.feature != c2.feature ||
           c1.lrange != c2.lrange;
  }else{
    // c1 is not atomic, but c2 is atomic
    if(c2.atom)
      return true;

    // check other attributes on this node
    if(c1.flags != c2.flags || c1.var != c2.var)
      return true;

    // check the hash value on the children
    if(c1.res->rhash != c2.res->rhash || c1.arg->rhash != c2.arg->rhash)
      return true;

    // check the children
    return _unequal(*c1.res, *c2.res) || _unequal(*c1.arg, *c2.arg);
  }
}

bool
_uneq(const Cat *c1, const Cat *c2){
  if(c1->atom){
    return c1->atom != c2->atom || c1->feature != c2->feature;
  }else{
    // c1 is not atomic, but c2 is atomic
    if(c2->atom)
      return true;

    // check other attributes on this node
    if(c1->flags != c2->flags)
      return true;

    // check the hash value on the children
    if(c1->res->rhash != c2->res->rhash || c1->arg->rhash != c2->arg->rhash)
      return true;

    // check the children
    return _uneq(c1->res, c2->res) || _uneq(c1->arg, c2->arg);
  }
}

} }
