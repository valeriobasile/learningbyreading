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
#include "io/reader_multi_horiz.h"

namespace NLP { namespace IO {

typedef Sentence::FieldNames FN;

static std::string build_name(const std::string &name, const FN &fieldnames){
  if(name.length())
    return name;
  else
    return "MultiHReader(" + fieldnames + ')';
}

MultiHReader::MultiHReader(std::istream &in, const std::string &uri,
			   const FN &fieldnames, char SEP, const std::string &name):
    HReader(in, uri, SEP, build_name(name, fieldnames)), fieldnames(fieldnames){
  if(fieldnames.size() == 0)
    die(msg << "fieldnames must be contain at least one field descriptor");

  for(FN::const_iterator i = fieldnames.begin(); i != fieldnames.end(); ++i)
    if(Sentence::type(*i) == Sentence::TYPE_INVALID)
      die(msg << '\'' << *i << "' is not a valid fieldname");
}

bool
MultiHReader::next(NLP::Sentence &sentence, bool add, bool expect){
  if(!add)
    sentence.reset();

  if(!next_line())
    return check(add, expect, false);

  char *begin = buffer;
  char *i = begin;
  char *end = begin + len;

  if(*begin == '\0')
    return check(add, expect, true);

  if(*begin == ' ')
    die(msg << "whitespace at the beginning of the sentence is illegal");

  ulong current = 0;
  const FN::const_iterator last_field = fieldnames.end() - 1;
  while(begin < end){
    FN::const_iterator field = fieldnames.begin();
    for( ; field != fieldnames.end(); ++field){
      switch(begin[0]){
      case '\0':
	switch(begin[-1]){
	case '\0': die(msg << "unexpected end of sentence (missing separator and field '" << *field << "')");
	case ' ': die(msg << "whitespace at the end of the sentence is illegal");
	default:
	  if(begin[-1] == SEP)
	    die(msg << "unexpected end of sentence (missing field '" << *field << "')");
	}
	break;
      case ' ':
	if(begin[-1] == ' '){
	  while(*begin == ' ')
	    ++begin;
	  if(*begin)
	    die(msg << "multiple whitespaces between words is illegal");
	  else
	    die(msg << "whitespace at the end of the sentence is illegal");
	}else if(begin[-1] == SEP)
	  die(msg << "unexpected end of token (missing field '" << *field << "')");
	break;
      default:
	if(begin[0] == SEP){
	  if(begin[-1] == SEP && Sentence::type(*field) > Sentence::TYPE_OPTIONAL)
	    die(msg << "empty fields (except [0-9?]) are illegal (empty field '" << *field << "')");
	  else if(begin[-1] == ' ')
	    die(msg << "unexpected sep before field " << *field);
	}
      }

      for( ; *i; ++i)
	if(*i == SEP || *i == ' ')
	  break;

      if(*i == ' ' && field != last_field)
	die(msg << "unexpected end of token (missing sep and " << field[1] << " field)");
      if(*i == '\0' && field != last_field)
	die(msg << "unexpected end of sentence (missing sep and " << field[1] << " field)");

      if(*field != '?'){
	std::vector<std::string> &tokens = sentence.get_single(*field);

	std::string token(begin, i - begin);
	if(add){
	  if(current >= tokens.size())
	    die(msg << "sentence is not aligned (token " << current + 1 << " '" << token
		<< "' makes sentence too long)");

	  if(tokens[current] != token)
	    die(msg << "field " << *field << " is not aligned (token " << current + 1 << " is '" << token
		<< "' but should be '" << tokens[current] << "\')");
	  ++current;
	}else
	  tokens.push_back(token);
      }

      begin = ++i;
    }

    if(i[-1] == SEP)
      die(msg << "too many fields in token");

    if(field != fieldnames.end())
      die(msg << "unexpected end of token (missing sep and " << *field << " field)");
  }

  return check(add, expect, true);
}

} }
