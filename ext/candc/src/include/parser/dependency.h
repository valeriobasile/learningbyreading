/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

namespace NLP {
  namespace CCG {

    class Dependency;

    typedef std::vector<Dependency *> Dependencies;
    // ConjFactor is average divisor for multiple slot fillers in max-recall decoder
    typedef uchar ConjFactor;

    class Dependency {
    public:
      const RelID rel;
      const Position head;
      const VarID var;
      const RuleID rule;
      const ConjFactor conj;
      const CatID lrange;

      Dependency *next;

      Dependency(Position head, RelID rel, VarID var, RuleID rule, Dependency *next):
        rel(rel), head(head), var(var), rule(rule), conj(1), lrange(0), next(next) { assert(head); }

      Dependency(Position head, RelID rel, VarID var, CatID lrange,
                 RuleID rule, Dependency *next):
        rel(rel), head(head), var(var), rule(rule), conj(1), lrange(lrange), next(next) { assert(head); }

      Dependency(const Dependency &other):
        rel(other.rel), head(other.head), var(other.var), rule(other.rule),
        conj(other.conj), lrange(other.lrange), next(other.next) {}

      // this constructor is used in Dependency::clone
      // which in turn is used in SuperCat::LexRule
      Dependency(const Dependency &other, VarID var, RuleID rule):
        rel(other.rel), head(other.head), var(var), rule(rule),
	conj(other.conj), lrange(other.lrange), next(0) {}

      Dependency(const Dependency &other, VarID var, CatID lrange):
        rel(other.rel), head(other.head), var(var), rule(other.rule),
	conj(other.conj), 
	//TODO next line is a hack - need a motivated way of deciding on
	//long-range feature when more than one option exists
	lrange(lrange > other.lrange ? lrange : other.lrange),
        next(0) {}

      Dependency(const Dependency &other, VarID var, CatID lrange,
		 ConjFactor conj_factor):
        rel(other.rel), head(other.head), var(var), rule(other.rule),
	conj(other.conj*conj_factor), 
	//        lrange(lrange ? lrange : other.lrange),
	//TODO same hack as above
	lrange(lrange > other.lrange ? lrange : other.lrange),
        next(0) {}

      Dependency(Pool *pool, const Dependency &other):
        rel(other.rel), head(other.head), var(other.var),
        rule(other.rule), conj(other.conj), lrange(other.lrange),
        next(other.next ? new (pool) Dependency(pool, *other.next) : 0) {}

      ~Dependency(void) { /* do nothing */ }

      void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); }
      void operator delete(void *, Pool *) { /* do nothing */ }

      static void _get(Pool *pool, Position pos, const Cat *cat, RuleID rule, 
		       Dependencies &deps){
        if(cat->rel)
          deps.push_back(new (pool) Dependency(pos, cat->rel, cat->var, rule, 0));
        if(cat->res){
          _get(pool, pos, cat->res, rule, deps);
          _get(pool, pos, cat->arg, rule, deps);
        }
      }

      static void _get(Pool *pool, const Variable &var, const Cat *cat, RuleID rule, 
		       Dependencies &deps){
        if(cat->rel){
	  const Position *const end = var.fillers + Variable::NFILLERS;
	  for(const Position *p = var.fillers; p != end && *p != Variable::SENTINEL; ++p)
	    if(*p)
	      deps.push_back(new (pool) Dependency(*p, cat->rel, cat->var, rule, 0));
	}
        if(cat->res){
          _get(pool, var, cat->res, rule, deps);
          _get(pool, var, cat->arg, rule, deps);
        }
      }


      static Dependency *get(Pool *pool, Position pos, const Cat *cat, RuleID ruleid = 0);
      static Dependency *get(Pool *pool, const Variable &var, const Cat *cat, RuleID ruleid = 0);
      static Dependency *clone(Pool *pool, const VarID from, const VarID to, const RuleID rule,
			       const Dependency *src);
      static Dependency *link(Dependencies &deps);
    };

    inline std::ostream &operator <<(std::ostream &out, const Dependency &dep){
      return out << static_cast<ulong>(dep.head) << ' ' << dep.rel << ' ' 
		 << static_cast<ulong>(dep.var) << ' '
                 << dep.lrange << ' ' << static_cast<ulong>(dep.rule);
    }

    class DependencyCmp {
    public:
      bool operator()(const Dependency *d1, const Dependency *d2){
        if(d1->rel == d2->rel){
          if(d1->head == d2->head)
            return d1->var < d2->var;
          else
            return d1->head < d2->head;
        }else
          return d1->rel < d2->rel;
      }
    };

    inline Dependency *
    Dependency::link(Dependencies &deps){
      if(!deps.size())
        return 0;

      std::sort(deps.begin(), deps.end(), DependencyCmp());
      for(size_t i = 0; i < deps.size() - 1; ++i)
        deps[i]->next = deps[i+1];
      return deps[0];
    }

    inline Dependency *
    Dependency::get(Pool *pool, Position pos, const Cat *cat, RuleID ruleid){
      if(cat->atom){
        if(cat->rel)
          return new (pool) Dependency(pos, cat->rel, cat->var, ruleid, 0);
        else
          return 0;
      }else{
        Dependencies deps;
        _get(pool, pos, cat, ruleid, deps);
        switch(deps.size()){
          case 0: return 0;
          case 1: return deps[0];
          default: return link(deps);
        }
      }
    }

    inline Dependency *
    Dependency::get(Pool *pool, const Variable &var, const Cat *cat, RuleID ruleid){
      Dependencies deps;

      _get(pool, var, cat, ruleid, deps);

      switch(deps.size()){
        case 0: return 0;
        case 1: return deps[0];
        default: return link(deps);
      }
    }

    inline Dependency *
    Dependency::clone(Pool *pool, const VarID from, const VarID to, const RuleID rule,
		      const Dependency *src){
      Dependencies deps;

      for(const Dependency *d = src; d; d = d->next)
        if(d->var == from)
          deps.push_back(new (pool) Dependency(*d, to, rule));

      switch(deps.size()){
        case 0: return 0;
        case 1: return deps[0];
        default: return link(deps);
      }
    }

    inline bool operator!=(const Dependency &d1, const Dependency &d2){
      return d1.rel != d2.rel ||
             d1.head != d2.head ||
             d1.var != d2.var ||
             d1.lrange != d2.lrange ||
	     //sc: added this too
	     d1.rule != d2.rule;
    }

    inline bool operator==(const Dependency &d1, const Dependency &d2){
      return d1.rel == d2.rel &&
             d1.head == d2.head &&
             d1.var == d2.var &&
	     d1.lrange == d2.lrange &&
	     //sc: added this too
	     d1.rule == d2.rule;
    }
  }
}
