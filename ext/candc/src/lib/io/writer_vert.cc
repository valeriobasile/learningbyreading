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
#include "io/writer_stream.h"
#include "io/writer_vert.h"

namespace NLP { namespace IO {

void
VWriter::next(NLP::Sentence &sent){
  out << BEGIN;
  nlines += std::count(BEGIN.begin(), BEGIN.end(), '\n');
  for(Raws::const_iterator i = sent.words.begin(); i != sent.words.end(); ++i, ++nlines)
    out << check(*i, SEP) << '\n';
  out << END;
  nlines += std::count(END.begin(), END.end(), '\n');
}

} }
