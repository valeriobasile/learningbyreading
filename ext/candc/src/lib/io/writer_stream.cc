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

namespace NLP { namespace IO {

void
StreamWriter::die(std::ostream &out) const {
  std::ostringstream &sout = dynamic_cast<std::ostringstream &>(out);

  std::string msg = sout.str();
  sout.str("");

  throw NLP::IOException(msg, uri, nlines);
}

void
StreamWriter::write_preface(const std::string &preface){
  out << preface << '\n';
}

} }
