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

namespace NLP { namespace CCG {

class Cat;

typedef uchar Position;
    
class Constraint {
public:
	const Position pos;
	const Position span;
	const bool require;
	const bool exclude;
	const Cat *match;

	Constraint(Position pos, Position span, bool require, bool exclude, const Cat *match)
		: pos(pos), span(span), require(require), exclude(exclude), match(match){}

	std::ostream &out(std::ostream &stream) const;
};

typedef std::vector<const Constraint *> Constraints;

} }
