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

#include "io/reader.h"
#include "io/reader_horiz.h"

using namespace std;

namespace NLP { namespace IO {

void
HReader::die(std::ostream &out) const {
  std::ostringstream &sout = dynamic_cast<std::ostringstream &>(out);

  std::string msg = sout.str();
  sout.str("");
  throw NLP::IOException(msg, uri, nlines);
}

bool
HReader::next_line(void){
  in.getline(buffer, sizeof(buffer), '\x0a');
  len = in.gcount();

  if(in.eof() && len == 0)
    return false;

  // remove the Windows carriage return if it exists
  if(buffer[len - 1] == '\x0d')
    buffer[--len] = '\0';

  ++nlines;

  if(!in)
    die(msg << "unexpected stream error (probably the sentence is too long)");

  return true;
}

HReader::HReader(std::istream &in, const std::string &uri, char SEP, const std::string &name):
    Reader(uri, name), prebuffer(' '), len(0), nlines(0), SEP(SEP), in(in){
  PREFACE = read_preface(uri, in, nlines, PREFACE_OPTIONAL);
}

HReader::~HReader(void){ /* do nothing */ }

void
HReader::reset(void){
  nlines = 0;
  in.clear();
  in.seekg(0, ios::beg);
  if(!in)
    die(msg << "the input stream could not be seeked to the beginning");
  PREFACE = read_preface(uri, in, nlines, PREFACE_OPTIONAL);
}

bool
HReader::next(NLP::Sentence &sent, bool add, bool expect){
  if(!add)
    sent.reset();

  if(!next_line())
    return check(add, expect, false);

  char *begin = buffer;
  char *end = begin;

  if(*begin == '\0')
    return check(add, expect, true);

  if(*begin == ' ')
    die(msg << "whitespace at the beginning of the sentence is illegal");

  ulong current = 0;
  for(; *end; ++end){
    if(*end == ' '){
      if(begin == end){
	while(*end && *end == ' ')
	  ++end;
	if(*end)
	  die(msg << "multiple spaces between words is illegal (after '" << sent.last() << "')");
	else
	  die(msg << "whitespace at the end of the sentence is illegal");
      }

      *end = '\0';
      if(add){
	if(current >= sent.words.size())
	  die(msg << "sentence is not aligned (token " << current + 1 << " '" << begin
	      << "' makes sentence too long)");

	if(sent.words[current] != begin)
	  die(msg << "words are not aligned (word " << current + 1 << " is '" << begin
	      << "' but should be '" << sent.words[current] << "\')");
	++current;
      }else
	sent.words.push_back(begin);
      begin = end + 1;
    }
  }

  if(*begin){
    if(add){
      if(sent.words[current] != begin)
	die(msg << "words are not aligned (word " << current + 1 << " is '" << begin
	    << "' but should be '" << sent.words[current] << "\')");
    }else
      sent.words.push_back(begin);
  }else
    die(msg << "whitespace at the end of the sentence is illegal");

  return check(add, expect, true);
}

} }
