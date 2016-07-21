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

    class Chart {
    public:
      // calculating array index in terms of span and position
      // ncells = nwords**2/2 + nwords/2 + 1
      // (pos, span) -> index
      // index = (span - 1)*nwords - (span - 1)*(span - 2)/2 + pos

      const bool EXTRA_RULES;
      const ulong MAXWORDS;
      const ulong MAXCELLS;
      Pool *pool;
      Equivalence equiv;
      Cell *const cells;
      Categories &cats;

      ulong nwords;
      ulong ncells;

      Chart(Categories &cats, bool EXTRA_RULES, ulong MAXWORDS);
      ~Chart(void){ delete [] cells; }

      ulong index(ulong pos, ulong span) const {
        return (span - 1)*nwords - (span - 1)*(span - 2)/2 + pos;
      }

      const Cell &cell(Position pos, Position span) const {
        assert(index(pos, span) < ncells);
        return cells[index(pos, span)];
      }

      Cell &cell(Position pos, Position span){
        assert(index(pos, span) < ncells);
        return cells[index(pos, span)];
      }

      Cell &root(void){ return cell(0, nwords); }

      const Cell &operator()(Position pos, Position span) const { return cell(pos, span); }
      Cell &operator()(Position pos, Position span){ return cell(pos, span); }

      // adds SuperCat to cell if no equivalence class exists
      // otherwise adds to existing equivalence class
      void add(Position pos, Position span, SuperCat *sc){
        if(equiv.add(pos, span, sc))
          cell(pos, span).add(sc);
      }

      void add(Position pos, Position span, SuperCats &scs){
        for(SuperCats::iterator i = scs.begin(); i != scs.end(); ++i)
          add(pos, span, *i);
      }

      // add a SuperCat to a leaf cell if no equivalence class exists
      // otherwise adds to existing equivalence class
      // but only if the exact lexical item has not already been added
      // i.e. it checks more than just equivalence
      void create(Pool *pool, Position pos, const Cat *cat){
				if(cell(pos, 1).has_leaf(cat))
					return;
				add(pos, 1, SuperCat::Lexical(pool, pos+1, cat, 0));
      }

      Cell tmp;

      const Cat *const NP;
      const Cat *const NbN;
      const Cat *const NPbNP;
      const Cat *const SbS;
      const Cat *const SfS;
      const Cat *const SbNPbSbNP;
      const Cat *const SbNPfSbNP;
      const Cat *const SfSbSfS;
      const Cat *const SbNPbSbNPbSbNPbSbNP;
      const Cat *const NPfNPbNP;

    private:
      void _add_lex(Position pos, Position span, const Cat *cat, const SuperCat *sc,
		    bool replace, RuleID ruleid);
      bool _add_genlex(Cell &cell, const Cat *cat, const SuperCat *sc,
		       bool replace, RuleID ruleid);
    public:
      void lex(Position pos, Position span, bool qu_parsing);
      bool genlex(SuperCat *TBsc, SuperCat *TBscRes, Cell &cell);
      void tr(Position pos, Position span);
      bool gen_tr(SuperCat *TBsc, SuperCat *TBscRes, Cell &cell);

			void set_constraint(const Constraint &c);

      bool load(const Sentence &sent, double BETA, bool repair,
								bool lexTR, bool qu_parsing);

      ulong build_tree(const TBSentence &sentence, ulong &node, ulong pos);
      bool load(const TBSentence &sentence);

      void dump(std::ostream &out) const;

      void mark(void){
				for(ulong i = 0; i < ncells; ++i)
					cells[i].mark();
      }

      void reset(void){
        equiv.clear();
        pool->clear();
        for(ulong i = 0; i < ncells; ++i)
          cells[i].clear();
        ncells = 0;
        nwords = 0;
      }
    };

  }
}
