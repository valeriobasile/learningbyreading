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

#include "io/writer.h"
#include "io/writer_group.h"

namespace NLP { namespace IO {

void
GroupWriter::write_preface(const std::string &preface){
  if(members.size() == 0)
    die(msg << "group writer " << name << " must have at least one member to write to");

  for(Members::const_iterator i = members.begin(); i != members.end(); ++i)
    (*i)->write_preface(preface);
}

void
GroupWriter::next(NLP::Sentence &sent){
  if(members.size() == 0)
    die(msg << "group writer " << name << " must have at least one member to write to");

  for(Members::const_iterator i = members.begin(); i != members.end(); ++i)
    (*i)->next(sent);
}

} }
