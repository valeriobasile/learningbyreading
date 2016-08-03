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


#include "base.h"

#include "pool.h"

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"

namespace NLP { namespace CCG {

std::ostream &
Constraint::out(std::ostream &stream) const {
	if(exclude)
		return stream << "exclude " << (int)pos << ' ' << (int)span << std::endl;
	stream << "require " << (int)pos << ' ' << (int)span;
	if(match)
		return stream << ' ' << *match << std::endl;
	return stream << std::endl;
}

}}
