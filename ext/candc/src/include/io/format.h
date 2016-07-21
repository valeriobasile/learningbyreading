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

namespace NLP {
  namespace IO {
    
    class Format {
    protected:
      void parse(const std::string &fmt);
    public:
      std::string sent_pre;
      std::string sent_post;

      char field_sep;
      char word_sep;
      char sent_sep;

      std::string fields;
      std::string separators;

      Format(void){}
      Format(const std::string &fmt){ parse(fmt); }
      ~Format(void){ /* do nothing */ }

      operator std::string(void) const;
      Format &operator =(const std::string &fmt){ parse(fmt); return *this; }
    };

  }
}
