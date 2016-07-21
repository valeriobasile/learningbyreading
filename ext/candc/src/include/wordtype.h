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

// NLP::WordType
// service class for translating a string into its word class
// representation using this mapping:
// [A-Z] => A
// [a-z] => a
// [0-9] => 0
// [-:] => -
// [.!] => .
// [,;] => ,

// multiple characters of the same class are compressed into a
// single character e.g. AAAaaa -> Aa
// an interesting extension of this would be not to do the
// compression (which gives an intermediate level of generalisation)

// the function call operator is overloaded for convenience
// the WordType is copied into an internal buffer, but this
// is fine when used in conjunction with the standard Attributes
// classes which always take copies when they need a pointer

// usually this involves going via the Lexicon object to get
// a canonical string back

namespace NLP {

  class WordType {
  private:
    char _start;
    char _buffer[512];
    char *_current;

    void _add(const char *word){
      for(const char *w = word; *w; ++w){
        if(islower(*w)){
          if(*_current != 'a')
            *++_current = 'a';
        }else if(isupper(*w)){
          if(*_current != 'A')
            *++_current = 'A';
        }else{
          switch(*w){
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': if(*_current != '0') *++_current = '0'; break;
            case '-':
            case ':': if(*_current != '-') *++_current = '-'; break;
            case '.':
            case '?':
            case '!': if(*_current != '.') *++_current = '.'; break;
            case ',':
            case ';': if(*_current != ',') *++_current = ','; break;
            default: if(*_current != *w) *++_current = *w; break;
          }
        }
      }
    }
  public:
    WordType(void): _start(0) {};

    ~WordType(void) {};

    const char *operator()(const char *word){
      _current = &_start;
      _add(word);
      *++_current = '\0';
      return _buffer;
    }

    const char *operator()(const std::string &word){
      return (*this)(word.c_str());
    }

    const char *operator()(const char *w1, const char *w2){
      _current = &_start;
      _add(w1);
      *++_current = '_';
      _add(w2);
      *++_current = '\0';
      return _buffer;
    }

    const char *operator()(const std::string &w1, const std::string &w2){
      return (*this)(w1.c_str(), w2.c_str());
    }

    const char *operator()(const char *w1, const char *w2, const char *w3){
      _current = &_start;
      _add(w1);
      *++_current = '_';
      _add(w2);
      *++_current = '_';
      _add(w3);
      *++_current = '\0';
      return _buffer;
    }

    const char *operator()(const std::string &w1, const std::string &w2, const std::string &w3){
      return (*this)(w1.c_str(), w2.c_str(), w3.c_str());
    }
  };

}
