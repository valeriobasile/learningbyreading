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

    class Relations;
    class Markedup;

    typedef ushort SCatFlags;
    
    class SuperCat {
    public:
      static const SCatFlags CONJ = 1 << 0;
      static const SCatFlags TR = 1 << 1;
      static const SCatFlags LEX = 1 << 2;

      static const SCatFlags FWD_APP = 1 << 3;
      static const SCatFlags BWD_APP = 1 << 4;
      static const SCatFlags FWD_COMP = 1 << 5;
      static const SCatFlags BWD_COMP = 1 << 6;
      static const SCatFlags BWD_CROSS = 1 << 7;

      static const SCatFlags RECURSIVE = 1 << 8;

      static const SCatFlags FUNNY_CONJ = 1 << 9;

      static const SCatFlags LEFT_PUNCT = 1 << 10;
      static const SCatFlags RIGHT_PUNCT = 1 << 11;

      static const SCatFlags LEFT_TC = 1 << 12;
      static const SCatFlags RIGHT_TC = 1 << 13;

      static const SCatFlags APPO = 1 << 14;
      static const SCatFlags GEN_MISC = 1 << 15;

      static const SCatFlags CONJ_TR = CONJ | TR;
      static const SCatFlags TR_LEX = TR | LEX;
      static const SCatFlags GEN_RULES = CONJ | TR | LEX | FWD_APP | BWD_APP | FWD_COMP |
                                         BWD_COMP | BWD_CROSS;

      typedef ulong Marker;
      const static Marker MARK_NONE = 0;
      const static Marker MARK_ACTIVE = 1;
      const static Marker MARK_VISITED = 2;

      static ulong nsupercats;
    private:
      SuperCat(Pool *pool, Position pos, const Cat *cat, SCatFlags flags);

      SuperCat(Pool *pool, const Cat *cat, SCatFlags flags, uchar depth,
               const SuperCat *left, const SuperCat *right, Unify &unify);

      SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
               const SuperCat *left, const SuperCat *right);

      SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
	       const SuperCat *left, const SuperCat *right, const SuperCat *vars);

      SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
               const SuperCat *left, bool replace, RuleID rule);

      SuperCat(Pool *pool, const TRCat &trcat, SCatFlags flags,
               const SuperCat *left, RuleID rule);

      SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
               const SuperCat *left, const SuperCat *right, bool replace, RuleID rule);

      SuperCat(Pool *pool, const Cat *cat, SCatFlags flags,
	       const SuperCat *left, const SuperCat *right, const SuperCat *head_sc, RuleID rule);

      SuperCat(Pool *pool, SCatFlags flags, const SuperCat *left, const SuperCat *right);

    public:
      const Cat *cat;
      const Dependency *unfilled;
      const Filled *filled;
      mutable SCatFlags flags;
      const uchar nvars;
      const uchar nactive;
      Variable *vars;

      const Feature feature;
      const uchar depth;
			const Position pos;
			const Position span;

      const SuperCat *left;
      const SuperCat *right;

      SuperCat *next;
      mutable const SuperCat *max;

      mutable Marker marker;

      mutable double score;
      mutable double inside;
      mutable double outside;
      mutable double d_inside;
      
      static SuperCat *Lexical(Pool *pool, Position pos, const Cat *cat, SCatFlags flags);

      static SuperCat *Rule(Pool *pool, const Cat *cat, SCatFlags flags, uchar depth,
			    const SuperCat *left, const SuperCat *right, Unify &unify);

      static SuperCat *Conj(Pool *pool, const Cat *cat, SCatFlags flags,
			    const SuperCat *left, const SuperCat *right);

      static SuperCat *Punct(Pool *pool, const Cat *cat, SCatFlags flags,
			     const SuperCat *left, const SuperCat *right, 
			     const SuperCat *vars);

      static SuperCat *LexRule(Pool *pool, const Cat *cat, SCatFlags flags,
			       const SuperCat *left, bool replace, RuleID rule);

      static SuperCat *TypeRaise(Pool *pool, const TRCat &trcat, SCatFlags flags,
				 const SuperCat *left, RuleID rule);

      static SuperCat *Special(Pool *pool, const Cat *cat, SCatFlags flags,
			    const SuperCat *left, const SuperCat *right,
			    const SuperCat *head_sc, RuleID rule);

      static SuperCat *TypeChange(Pool *pool, const Cat *cat, SCatFlags flags,
				  const SuperCat *left, const SuperCat *right,
				  bool replace, RuleID rule);

      static SuperCat *Apposition(Pool *pool, SCatFlags flags,
				  const SuperCat *left, const SuperCat *right);

      ~SuperCat(void) { /* do nothing */ }

      void *operator new(size_t size, Pool *pool) { return (void *)pool->alloc(size); }
      void operator delete(void *, Pool *) { /* do nothing */ }

      bool conj(void) const { return flags & CONJ; }
      bool tr(void) const { return flags & TR; }
      bool lex(void) const { return flags & LEX; }
      bool unary(void) const { return flags & TR_LEX; }
      bool conj_or_tr(void) const { return flags & CONJ_TR; }
      bool conj_and_tr(void) const { return (flags & CONJ_TR) == CONJ_TR; }
      bool fcomp(void) const { return flags & FWD_COMP; }
      bool bcomp(void) const { return flags & BWD_COMP; }
      bool bxcomp(void) const { return flags & BWD_CROSS; }

      Hash ehash(void) const {
	Hash h = cat->rhash;
	for(const Dependency *dep = unfilled; dep; dep = dep->next)
	  h += dep->rel;
	const Variable *const end = vars + nactive;
	for(const Variable *v = vars + 1; v != end; ++v){
	  h += v->fillers[1];
	  h += v->fillers[2];
	  h += v->fillers[3];
	  h += v->fillers[4];
	  h += v->fillers[5];
	  h += v->fillers[6];
	  h += v->fillers[7];
	}
	return h;
      }

      void print_filled(std::ostream &out) const;

      void print_filled(std::ostream &out, char type, const Raws &heads, const Raws &words) const;
  
      void print_filled(std::ostream &out, const Markedup &markedup, const Relations &rels,
			const Raws &heads, const Raws &words, bool julia_slots) const;

      void get_grs(GRs &grs, const Relations &rels,
		   FilledDeps &seen, const Sentence &sent) const;

      void get_filled(GRs &deps, const Relations &rels,
		      const Sentence &sent, bool julia_slots) const;

      void print_filled_words(std::ostream &out, char type, const Raws &words, const Raws &tags) const;
      void print_filled_verbs(std::ostream &out, char type, const Raws &words, const Raws &tags) const;
      void print_filled_punct(std::ostream &out, char type, const Raws &words, const Raws &tags) const;

      const char *flags2str(void) const;

      std::ostream &lex_info(std::ostream &stream) const;
      std::ostream &conj_info(std::ostream &stream) const;

      std::ostream &out_boxer(std::ostream &stream, Feature unified) const;

      void mark_active(void) const { marker = MARK_ACTIVE; }
      void mark_visited(void) const { marker = MARK_VISITED; }
      bool is_active(void) const { return marker != MARK_NONE; }
      bool is_visited(void) const { return marker == MARK_VISITED; }

      void mark_active_disj(ulong &ndisj){
	++ndisj;

	mark_active();
	if(left && !left->is_active())
	  const_cast<SuperCat *>(left)->mark_active_disj(ndisj);

	if(right && !right->is_active())
	  const_cast<SuperCat *>(right)->mark_active_disj(ndisj);

	if(next)
	  const_cast<SuperCat *>(next)->mark_active_conj(ndisj);
      }

      void mark_active_conj(ulong &ndisj){
	mark_active();
	if(left && !left->is_active())
	  const_cast<SuperCat *>(left)->mark_active_disj(ndisj);

	if(right && !right->is_active())
	  const_cast<SuperCat *>(right)->mark_active_disj(ndisj);

	if(next)
	  const_cast<SuperCat *>(next)->mark_active_conj(ndisj);
      }

      ulong nequiv(void) const { return next ? 1 + next->nequiv() : 1; }
    };

    extern ulong equiv_calls;
    extern ulong equiv_unary;
    extern ulong equiv_nactive;
    extern ulong equiv_cat;
    extern ulong equiv_vars;
    extern ulong equiv_deps;
    extern ulong equiv_ndeps;
    extern ulong equiv_true;

    inline bool
    equivalent(const SuperCat *sc1, const SuperCat *sc2){
      ++equiv_calls;

      if(sc1->nactive != sc2->nactive){
	++equiv_nactive;
	return false;
      }

      if(!vars_eq(sc1->vars, sc2->vars, sc1->nactive)){
	++equiv_vars;
	return false;
      }

      // ensures cats created via unary rules get their own equiv classes
      if(sc1->unary() != sc2->unary()){
	//sc: need to deal with lexical cases too!
	++equiv_unary;
	return false;
      }
      
      if(*sc1->cat != *sc2->cat){
	++equiv_cat;
	return false;
      }

      /*
	const Dependency *d1 = sc1->unfilled;
	const Dependency *d2 = sc2->unfilled;
	for( ; d1 && d2; d1 = d1->next, d2 = d2->next)
	  if(*d1 != *d2){
	    ++equiv_deps;
	  return false;
	}

	if(d1 != d2){
	  ++equiv_ndeps;
	  return false;
	}
      */

      ++equiv_true;
      return true;
    }

    inline std::ostream &operator <<(std::ostream &stream, const SuperCat &sc){
      stream << *sc.cat << ' ' << sc.flags2str() << " nactive: " << (int)sc.nactive << " nvars: " << (int)sc.nvars << std::endl;
      for(VarID i = 1; i < sc.nvars; ++i)
        if(i < sc.nactive)
          stream << "active   " << i << " " << sc.vars[i] << std::endl;
        else
          stream << "inactive " << i << " " << sc.vars[i] << std::endl;

      for(const Filled *filled = sc.filled; filled; filled = filled->next)
        stream << "filled " << *filled << " filled\n";

      for(const Dependency *dep = sc.unfilled; dep; dep = dep->next)
        stream << "dep " << *dep << " unfilled\n";

      stream << sc.score << ' ' << sc.marker << '\n';

      return stream;
    }

    bool equivalent(const SuperCat *sc1, const SuperCat *sc2);

    typedef std::vector<SuperCat *> SuperCats;

    std::string
    equivalent_explain(const SuperCat *sc1, const SuperCat *sc2);
  }
}
