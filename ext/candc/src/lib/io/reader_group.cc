// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

/* -*- Mode: C++; -*- */

#include "base.h"

#include "io/reader.h"
#include "io/reader_group.h"

using namespace std;

namespace NLP { namespace IO {

void
GroupReader::reset(void){
  if(members.size() == 0)
    die(msg << "group reader " << name << " must have at least one member to reset");

  PREFACE = "";
  for(Members::const_iterator i = members.begin(); i != members.end(); ++i){
    (*i)->reset();
    PREFACE += (*i)->PREFACE;
  }
}

bool
GroupReader::next(NLP::Sentence &sent, bool add, bool expect){
  if(members.size() == 0)
    die(msg << "group reader " << name << " must have at least one member to read from");

  Members::const_iterator i = members.begin();
  bool res = (*i)->next(sent, add, expect);
  for(++i; i != members.end(); ++i)
    (*i)->next(sent, true, res);

  return res;
}

} }
