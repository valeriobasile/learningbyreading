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
#include "io/writer_multi_vert.h"

namespace NLP { namespace IO {

typedef Sentence::FieldNames FN;

MultiVWriter::MultiVWriter(std::ostream &out, const std::string &uri,
			   const FN &fieldnames, char SEP,
			   const std::string &BEGIN, const std::string &END,
			   const std::string &name)
  : VWriter(out, uri, SEP, BEGIN, END, name + '(' + fieldnames + ')'),
    fieldnames(fieldnames){

  if(fieldnames.size() == 0)
    die(msg << "fieldnames must be contain at least one field descriptor");

  for(FN::const_iterator i = fieldnames.begin(); i != fieldnames.end(); ++i)
    if(Sentence::type(*i) == Sentence::TYPE_INVALID)
      die(msg << '\'' << *i << "' is not a valid fieldname");
}

void
MultiVWriter::write(NLP::Sentence &sent, ulong i){
  FN::const_iterator field = fieldnames.begin();

  if(sent.type(*field) == Sentence::TYPE_MULTI){
    const MultiRaw &mraw = sent.get_multi(*field)[i];
    out << mraw.size();
    for(MultiRaw::const_iterator j = mraw.begin(); j != mraw.end(); ++j)
      out << SEP << check(j->raw, SEP) << SEP << j->score;
  }else
    out << check(sent.get_single(*field)[i], SEP);
  
  for(++field; field != fieldnames.end(); ++field){
    if(sent.type(*field) == Sentence::TYPE_MULTI){
      const MultiRaw &mraw = sent.get_multi(*field)[i];
      out << SEP << mraw.size();
      for(MultiRaw::const_iterator j = mraw.begin(); j != mraw.end(); ++j)
	out << SEP << check(j->raw, SEP) << SEP << j->score;
    }else
      out << SEP << check(sent.get_single(*field)[i], SEP);
  }
}

void
MultiVWriter::next(NLP::Sentence &sent){
  sent.check_sizes(fieldnames);

  const ulong LEN = sent.get_size(fieldnames[0]);

  out << BEGIN;
  nlines += std::count(BEGIN.begin(), BEGIN.end(), '\n');
  for(ulong i = 0; i < LEN; ++i, ++nlines){
    write(sent, i);
    out << '\n';
  }
  out << END;
  nlines += std::count(END.begin(), END.end(), '\n');
}

} }
