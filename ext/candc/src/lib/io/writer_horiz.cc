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
#include "io/writer_horiz.h"

namespace NLP { namespace IO {

void
HWriter::next(NLP::Sentence &sent){
  if(sent.words.size() > 0){
    Raws::const_iterator i = sent.words.begin();
    out << check(*i, SEP);
    for(++i; i != sent.words.end(); ++i)
      out << ' ' << check(*i, SEP);
  }

  out << '\n';
  ++nlines;
}

} }
