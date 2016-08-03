// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <string>
#include <iostream>
#include <sstream>
#include <exception>

using namespace std;

#include "utils/escape.h"
#include "exception.h"

char
esc2char(char c){
  switch(c){
  case 'a': return '\a';
  case 'b': return '\b';
  case 'f': return '\f';
  case 'n': return '\n';
  case 'r': return '\r';
  case 't': return '\t';
  case 'v': return '\v';
  case '"': return '"';
  case '\'': return '\'';
  case '\\': return '\\';
  case '0': return '\0';
  default: return c;
  }
}

char
char2esc(char c){
  switch(c){
  case '\a': return 'a';
  case '\b': return 'b';
  case '\f': return 'f';
  case '\n': return 'n';
  case '\r': return 'r';
  case '\t': return 't';
  case '\v': return 'v';
  case '\"': return '\"';
  case '\'': return '\'';
  case '\\': return '\\';
  case '\0': return '0';
  default: return c;
  }
}

std::string
char2esc_str(char c){
  switch(c){
  case '\a': return "\\a";
  case '\b': return "\\b";
  case '\f': return "\\f";
  case '\n': return "\\n";
  case '\r': return "\\r";
  case '\t': return "\\t";
  case '\v': return "\\v";
  case '\"': return "\\\"";
  case '\'': return "\\\'";
  case '\\': return "\\\\";
  case '\0': return "\\0";
  default:
    string res;
    res += c;
    return res;
  }
}

// strip the surrounding " if they exist
std::string
strip_quotes(const std::string &s){
  size_t begin;
  size_t end;

  char quote_end = 0;

  // not including the inverted commas or the spaces outside the "..."
  for(begin = 0; begin < s.size(); ++begin){
    if(s[begin] == '"' || s[begin] == '\''){
      quote_end = s[begin];
      begin++;
      break;
    }
    if(!isspace(s[begin])) // not surrounded by quotes
      return s;
  }
	
  if(begin == s.size())
    return "";
	
  for(end = s.size() - 1; end >= begin; --end){
    if(s[end] == quote_end)
      break;
 
    if(!isspace(s[end]))
      throw NLP::Exception("missing matching end quote in quoted string '" + s + '\'');
  }
	
  if(end < begin)
    throw NLP::Exception("missing matching end quote in quoted string '" + s + '\'');
	
  return s.substr(begin, end - begin);
}

// convert escape sequences to characters
std::string
asc2bin(const std::string &s){
  bool in_escape = false;
  std::string ret = "";

  for(size_t i = 0; i < s.size(); ++i){
    if(!in_escape){
      if(s[i]=='\\'){
	in_escape = true;
	continue;
      }
		
      ret += s[i];
    }else{
      in_escape = false;
      ret += esc2char(s[i]);
    }
  }

  if(in_escape)
    throw NLP::Exception("unexpected end of escape sequence in '" + s + '\'');

  return ret;
}

// convert characters to escape sequences
std::string
bin2asc(const std::string &s){
  std::string ret = "";

  for(size_t i = 0; i < s.size(); ++i)
    ret += char2esc_str(s[i]);

  return ret;
}

std::string
strip_whitespace(const std::string &s){
  size_t begin, end;
	
  for(begin = 0; begin < s.size(); ++begin)
    if(!isspace(s[begin])) 
      break;
	
  for(end = s.size() - 1; end >= begin; --end)
    if(!isspace(s[end]))
      break;

  return s.substr(begin,end-begin+1);
}
