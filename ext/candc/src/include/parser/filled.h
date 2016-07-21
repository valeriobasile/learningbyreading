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

    class Filled {
    public:
      const RelID rel;
      const Position head;
      const Position filler;
      const RuleID rule;
      const ConjFactor conj;
      const CatID lrange;

      const Filled *next;

      Filled(const Dependency *dep, Position filler, CatID lrange, 
	     ConjFactor conj_factor, const Filled *next):
	rel(dep->rel), head(dep->head), filler(filler), rule(dep->rule), 
	conj(conj_factor), lrange(lrange ? lrange : dep->lrange), next(next) { assert(filler); }

      Filled(RelID rel, Position head, Position filler, RuleID rule,
	     CatID lrange, ConjFactor conj_factor, const Filled *next):
	rel(rel), head(head), filler(filler), rule(rule), 
	conj(conj_factor), lrange(lrange), next(next) { assert(filler); }

      Filled(Position head, RelID rel, Position filler, CatID lrange, RuleID rule):
	  rel(rel), head(head), filler(filler), rule(rule), conj(1), lrange(lrange), next(0) {
	assert(filler);
      }

      ~Filled(void) { /* do nothing */ }

      void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); };
      void operator delete(void *, Pool *) { /* do nothing */ }
    };

    inline std::ostream &operator <<(std::ostream &out, const Filled &dep){
      return out << static_cast<ulong>(dep.head) << ' ' << dep.rel << ' ' 
		 << static_cast<ulong>(dep.filler) << ' ' << dep.lrange << ' '
		 << static_cast<ulong>(dep.rule);
    }

    class FilledCmp {
    public:
      bool operator()(const Filled *d1, const Filled *d2){
        if(d1->rel == d2->rel){
          if(d1->head == d2->head)
            return d1->filler < d2->filler;
          else
            return d1->head < d2->head;
        }else
          return d1->rel < d2->rel;
      }
    };

    inline bool operator!=(const Filled &d1, const Filled &d2){
      return d1.rel != d2.rel ||
             d1.head != d2.head ||
             d1.filler != d2.filler ||
	     d1.rule != d2.rule ||
             d1.lrange != d2.lrange;
    }

    inline bool operator==(const Filled &d1, const Filled &d2){
      return d1.rel == d2.rel &&
             d1.head == d2.head &&
             d1.filler == d2.filler &&
	     d1.rule == d2.rule &&
             d1.lrange == d2.lrange;
    }

  }
}
