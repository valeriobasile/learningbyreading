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

    struct Features {
    public:
      const static uchar NONE = 0;
      const static uchar X = 1;
      const static uchar ADJ = 2;
      const static uchar AS = 3;
      const static uchar ASUP = 4;
      const static uchar B = 5;
      const static uchar BEM = 6;
      const static uchar DCL = 7;
      const static uchar EM = 8;
      const static uchar EXPL = 9;
      const static uchar FOR = 10;
      const static uchar FRG = 11;
      const static uchar INTJ = 12;
      const static uchar INV = 13;
      const static uchar NB = 14;
      const static uchar NG = 15;
      const static uchar NUM = 16;
      const static uchar POSS = 17;
      const static uchar PSS = 18;
      const static uchar PT = 19;
      const static uchar Q = 20;
      const static uchar QEM = 21;
      const static uchar THR = 22;
      const static uchar TO = 23;
      const static uchar WQ = 24;
      const static ulong NFEATURES = 25;
    };

    class Feature: public Fixed {
    private:
      uchar _convert(const char *s);
    public:
      Feature(void){}
      Feature(uchar value): Fixed(value) {}
      Feature(const Feature &other): Fixed(other) {}
      Feature(const char *s): Fixed(_convert(s)) {}
      Feature(const std::string &s): Fixed(_convert(s.c_str())) {}
      ~Feature(void) { /* do nothing */ }

      operator const char *(void) const;

      bool is_none(void) const { return _value == Features::NONE; }
      bool is_var(void) const { return _value == Features::X; }
      bool is_free(void) const { return _value <= Features::X; }
      bool is_adj(void) const { return _value == Features::ADJ; }
      bool is_pss(void) const { return _value == Features::PSS; }
      bool is_to(void) const { return _value == Features::TO; }
      bool is_ng(void) const { return _value == Features::NG; }
      bool is_dcl(void) const { return _value == Features::DCL; }
      bool is_b(void) const { return _value == Features::B; }

      Feature override(const Feature parent) const { return is_free() && parent ? parent : *this; }
    };

    inline std::ostream &operator<<(std::ostream &stream, const Feature &f){
      return stream << static_cast<const char *>(f);
    }
  }
}
