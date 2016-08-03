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

#include "io/format.h"

#include "io/writer.h"
#include "io/writer_stream.h"
#include "io/writer_format.h"

namespace NLP { namespace IO {

typedef Sentence::FieldNames FN;

FormatWriter::FormatWriter(std::ostream &out, const std::string &uri,
			   const Format &fmt, const std::string &name)
  : StreamWriter(out, uri, name + '(' + fmt.fields + ')'), fmt(fmt){

  if(fmt.fields.size() == 0)
    die(msg << "fieldnames must be contain at least one field descriptor");
}

  /*
void
FormatWriter::write(NLP::Sentence &sent, ulong i){
  FN::const_iterator field = fmt.fields.begin();
  std::string::const_iterator sep = fmt.separators.begin();
  out << check(sent.get_single(*field)[i], *sep);
  for(++field; field != fmt.fields.end(); ++field, ++sep)
    out << *sep << check(sent.get_single(*field)[i], *sep);
    }*/

void
FormatWriter::write(NLP::Sentence &sent, ulong i){
  FN::const_iterator field = fmt.fields.begin();
  std::string::const_iterator sep = fmt.separators.begin();

  if(sent.type(*field) == Sentence::TYPE_MULTI){
    const MultiRaw &mraw = sent.get_multi(*field)[i];
    out << mraw.size();
    for(MultiRaw::const_iterator j = mraw.begin(); j != mraw.end(); ++j)
      out << *sep << check(j->raw, *sep) << *sep << j->score;
  }else
    out << check(sent.get_single(*field)[i], *sep);
  
  for(++field; field != fmt.fields.end(); ++field, ++sep){
    if(sent.type(*field) == Sentence::TYPE_MULTI){
      const MultiRaw &mraw = sent.get_multi(*field)[i];
      out << *sep << mraw.size();
      for(MultiRaw::const_iterator j = mraw.begin(); j != mraw.end(); ++j)
	out << *sep << check(j->raw, *sep) << *sep << j->score;
    }else
      out << *sep << check(sent.get_single(*field)[i], *sep);
  }
}

void
FormatWriter::next(NLP::Sentence &sent){
  sent.check_sizes(fmt.fields);

  out << fmt.sent_pre;
  const ulong LEN = sent.get_size(fmt.fields[0]);
  if(LEN > 0){
    ulong i = 0;
    write(sent, i);
    for(++i; i != LEN; ++i){
      out << fmt.word_sep;
      write(sent, i);
    }
  }
  out << fmt.sent_post;

  ++nlines;
}

} }
