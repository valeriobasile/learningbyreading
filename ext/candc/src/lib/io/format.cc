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

using namespace std;

namespace NLP { namespace IO {

void
Format::parse(const std::string &fmt){
  sent_pre.clear();
  sent_post.clear();
  field_sep = word_sep = '\0';
  fields.clear();
  separators.clear();

  const char *s = fmt.c_str();

  // read the sent_pre marker (handling %% escape for %)
  for( ; *s; ++s){
    if(s[0] == '%'){
      if(s[1] == '%')
	++s;
      else
	break;
    }
    sent_pre += *s;
  }

  if(!*s)
    throw NLP::Exception("format string must contain at least one field specifier");

  for( ; *s; s += 3){
    if(s[0] != '%')
      break;
    if(!s[1])
      throw NLP::Exception("unexpected end of format string after %");

    if(s[1] == '%')
      break;
    if(Sentence::type(s[1]) == Sentence::TYPE_INVALID)
      throw NLP::Exception(string("format string field specifier %")
			   + s[1] + " is unknown [wlpcnsPCNS0123456789?]");
    fields += s[1];

    if(!s[2])
      throw NLP::Exception(string("format string is missing field or word separator after %") + s[1]);

    if(s[2] == '%'){
      if(s[3] == '%')
	++s;
      else if(!s[3])
	throw NLP::Exception("unexpected end of format string after %");
      else if(Sentence::type(s[3]) != Sentence::TYPE_INVALID)
	throw NLP::Exception(string("format string is missing field or word separator after %") + s[1]);
      else
	throw NLP::Exception(string("format string field specifier %")
			     + s[3] + " is unknown [wlpcnsPCNS0123456789?]");
    }

    separators += s[2];
  }

  // the last separator in the list is the word separator
  word_sep = separators[separators.size() - 1];
  separators.erase(separators.size() - 1);

  field_sep = separators[0];
  for(std::string::const_iterator i = separators.begin(); i != separators.end(); ++i)
    if(*i != field_sep){
      field_sep = '\0';
      break;
    }

  if(!*s)
    throw NLP::Exception("format string sentence separator is missing after word separator");

  // read the sent_post marker
  for( ; *s; ++s){
    if(*s == '%'){
      if(s[1] == '%')
	++s;
      else if(!s[1])
	throw NLP::Exception("unexpected end of format string after %");
      else if(Sentence::type(s[1]) != Sentence::TYPE_INVALID)
	throw NLP::Exception(string("field separator after field %")
			     + fields[fields.size() - 1] + " should only be one character");
      else
	throw NLP::Exception(string("format string field specifier %")
			     + s[3] + " is unknown [wlpcnsPCNS0123456789?]");
    }

    sent_post += *s;
  }

  if(sent_post.size() == 1)
    sent_sep = sent_post[0];
}

static
std::string
escape_percent(const std::string &s){
  if(s.find('%') == std::string::npos)
    return s;

  std::string res;
  for(std::string::const_iterator i = s.begin(); i != s.end(); ++i){
    res += *i;
    if(*i == '%')
      res += '%';
  }
  return res;
}

Format::operator std::string(void) const {
  std::string res = escape_percent(sent_pre);
  res += '%';
  res += fields[0];
  for(ulong i = 1; i < fields.size(); ++i){
    if(separators[i - 1] == '%')
      res += '%';
    res += separators[i - 1];
    res += '%';
    res += fields[i];
  }
  if(word_sep == '%')
    res += '%';
  res += word_sep;
  res += escape_percent(sent_post);

  return res;
}

} }
