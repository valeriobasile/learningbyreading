/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Affix
// efficiently represents prefix/suffix strings of up to 4 characters
// used for affix feature storage and lookup in the taggers
// this code is not portable to multi-byte language encodings e.g. Unicode

#include	<cstring>

namespace NLP {

  class Affix {
  protected:
    ulong _value;	// affix string is stored in an unsigned long

    // convert an existing string to an unsigned long
    ulong _make_value(const char *s, ulong len){
      ulong val = 0;
      switch(len){
        default: assert(!"affix length must be less than 5 characters");
        case 4: val = static_cast<uchar>(*s++);
                val = val << 8;
                /* fall through */
        case 3: val |= static_cast<uchar>(*s++);
                val = val << 8;
                /* fall through */
        case 2: val |= static_cast<uchar>(*s++);
                val = val << 8;
                /* fall through */
        case 1: val |= static_cast<uchar>(*s);
      }
      return val;
    };

  public:
    // construct from char * and std::string
    // used when loading features from disk
    Affix(const char *s): _value(_make_value(s, strlen(s))) {};
    Affix(const std::string &s): _value(_make_value(s.data(), s.size())) {};

    // used when extracting features when tagging
    Affix(const char *s, ulong len): _value(_make_value(s, len)) {};
    Affix(const std::string &s, ulong len): _value(_make_value(s.data(), len)) {};

    // create a single character affix
    Affix(char c):_value(static_cast<uchar>(c)) {};

    Affix(const Affix &other): _value(other._value) {};

    // incremental construction
    // used when extracting features when tagging
    Affix &operator +=(char c){
      _value = _value << 8 | static_cast<uchar>(c);
      return *this;
    }

    ulong value(void) const { return _value; }
    char operator[](int i) const { return reinterpret_cast<const char *>(&_value)[i]; }
  };

  inline bool operator==(Affix a1, Affix a2){ return a1.value() == a2.value(); }

}
