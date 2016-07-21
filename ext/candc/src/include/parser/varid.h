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
  namespace CCG {

    struct Vars {
    public:
      const static uchar NONE = 0;
      const static uchar X = 1;
      const static uchar Y = 2;
      const static uchar Z = 3;
      const static uchar W = 4;
      const static uchar V = 5;
      const static uchar U = 6;
      const static uchar T = 7;
      const static uchar R = 8;
      const static uchar Q = 9;
      const static uchar A = 10;
      const static uchar B = 11;
      const static uchar C = 12;
      const static uchar D = 13;
      const static uchar E = 14;
      const static uchar F = 15;
      const static uchar NVARS = 16;
    };

    class VarID: public Fixed {
    private:
      uchar _convert(const char *s);
    public:
      VarID(void){}
      VarID(uchar value): Fixed(value) {}
      VarID(const VarID &other): Fixed(other) {}
      explicit VarID(const char *s): Fixed(_convert(s)) {}
      explicit VarID(const std::string &s): Fixed(_convert(s.c_str())) {}
      ~VarID(void) { /* do nothing */ }

      operator const char *(void) const;
      VarID &operator++(void){ ++_value; return *this; }
    };

    inline std::ostream &operator<<(std::ostream &stream, const VarID &v){
      return stream << static_cast<const char *>(v);
    }
  }
}
