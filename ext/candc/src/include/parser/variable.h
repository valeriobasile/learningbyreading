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

    class Variable {
    public:
      const static ulong NFILLERS = 8;
      const static Position UNFILLED = 0;
      const static Position SENTINEL = 255;
      const static ushort LEX_UNFILLED = SENTINEL;

      Position fillers[NFILLERS];

      // unfilled variable
      Variable(void){
	fillers[0] = UNFILLED; 
	memset(fillers + 1, SENTINEL, sizeof(fillers) - 1);
      };

      // lexical variable
      Variable(Position pos){
	fillers[0] = pos;
	memset(fillers + 1, SENTINEL, sizeof(fillers) - 1);
      };

      // unfilled chained
      Variable(const Variable &other) {
	fillers[0] = UNFILLED;
	if(other.is_filled())
	  memcpy(fillers + 1, other.fillers, sizeof(fillers) - 1);
	else
	  memset(fillers + 1, SENTINEL, sizeof(fillers) - 1);
      };

      // unify rule
      Variable(const Variable &v1, const Variable &v2);

      bool is_unfilled(void) const { return !fillers[0]; };
      bool is_filled(void) const { return fillers[0]; };
      bool is_lexical(void) const { return fillers[1] == SENTINEL && fillers[0]; };
      bool is_set(void) const { return fillers[1] != SENTINEL; };
      bool is_lex_unfilled(void) const { return !fillers[0] && fillers[1] == SENTINEL; };

      Position pos(void) const { return fillers[0]; };

      uchar count_fillers(void) const {
	uchar count = 0;
	const Position *const end = fillers + NFILLERS;
	for(const Position *p = fillers; p != end && *p != SENTINEL; ++p)
	  ++count;
	return count;
      };

      std::ostream &out(std::ostream &stream) const {
        if(is_lexical())
          return stream << static_cast<ulong>(fillers[0]);
        if(is_set()){
          if(is_filled()){
	    stream << static_cast<ulong>(fillers[0]);
          }else
            stream << "unfilled";
	  for(ulong i = 1; i < sizeof(fillers) && fillers[i] != SENTINEL; ++i)
	    stream << ", " << static_cast<ulong>(fillers[i]);
          return stream;
        }
        if(is_unfilled())
          return stream << "unfilled";
        else
          assert(!"unexpected variable state");

        return stream;
      }

    };

    inline std::ostream &operator<<(std::ostream &stream, const Variable &var){
      if(var.is_set()){
        stream << "{ ";
        var.out(stream);
        return stream << " }";
      }else
        return var.out(stream);
    }

    inline bool operator!=(const Variable &v1, const Variable &v2){
      return memcmp(v1.fillers, v2.fillers, sizeof(v1.fillers));
    }

    inline bool vars_eq(const Variable *v1, const Variable *v2, ulong nvars){
      // this function is designed to speedup the following loop from equivalent
      // in supercat.h
      /*
	for(VarID i = 1; i < sc1->nactive; ++i)
	  if(sc1->vars[i] != sc2->vars[i]){
	    ++equiv_vars;
	    return false;
	  }
      */

      return nvars && memcmp(v1 + 1, v2 + 1, sizeof(v1->fillers)*(nvars - 1)) == 0;
    }

  }
}
