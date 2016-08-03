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
#include "io/writer_multi_horiz.h"

namespace NLP { namespace IO {

typedef Sentence::FieldNames FN;

MultiHWriter::MultiHWriter(std::ostream &out, const std::string &uri,
			   const FN &fieldnames, char SEP,
			   const std::string &name)
  : HWriter(out, uri, SEP, name + '(' + fieldnames + ')'), fieldnames(fieldnames){
  if(fieldnames.size() == 0)
    die(msg << "fieldnames must be contain at least one field descriptor");

  for(FN::const_iterator i = fieldnames.begin(); i != fieldnames.end(); ++i)
    if(Sentence::type(*i) == Sentence::TYPE_INVALID)
      die(msg << '\'' << *i << "' is not a valid fieldname");
}

void
MultiHWriter::write(NLP::Sentence &sent, ulong i){
  FN::const_iterator field = fieldnames.begin();
  out << check(sent.get_single(*field)[i], SEP);
  for(++field; field != fieldnames.end(); ++field)
    out << SEP << check(sent.get_single(*field)[i], SEP);
}

void
MultiHWriter::next(NLP::Sentence &sent){
  sent.check_sizes(fieldnames);

  const ulong LEN = sent.get_size(fieldnames[0]);
  if(LEN > 0){
    ulong i = 0;
    write(sent, i);
    for(++i; i != LEN; ++i){
      out << ' ';
      write(sent, i);
    }
  }

  out << '\n';
  ++nlines;
}

} }
