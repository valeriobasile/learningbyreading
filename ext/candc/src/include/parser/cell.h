/* -*- Mode: C++; indent-tabs-mode: nil -*- */
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

    class Cell: public std::vector<SuperCat *> {
    public:
      ulong nold;
			bool require;
			bool exclude;
			const Cat *match;

			Cell(void): nold(0), require(false), exclude(false), match(0){}
      
			void add(const Cell &cell){
				if(exclude)
					return;

				if(!match)
					insert(end(), cell.begin(), cell.end());
				else{
					reserve(size() + cell.size());
					for(Cell::const_iterator i = cell.begin(); i != cell.end(); ++i)
						add(*i);
				}
			}

			void add(SuperCat *sc){
				if(match && *sc->cat != *match){
					std::cerr << "filtered out " << *sc << std::endl;
					return;
				}
 
				push_back(sc);
			}

      void clear(void){
        resize(0);
        nold = 0;
				require = false;
				exclude = false;
				match = 0;
      }

      bool updated(void) { return nold != size(); }

      iterator old(void) { return begin() + nold; }
      const_iterator old(void) const { return begin() + nold; }

      void mark(void){ nold = size(); }

      bool has_leaf(const Cat *cat) const {
				for(const_iterator i = begin(); i != end(); ++i)
					if((*i)->left == 0 && *(*i)->cat == *cat)
						return true;
				return false;
      }

			void set(const Constraint &c){
				require = c.require;
				exclude = c.exclude;
				match = c.match;
			}
    };

  }
}
