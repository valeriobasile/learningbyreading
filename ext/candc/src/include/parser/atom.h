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

    struct Atoms {
    public:
      const static uchar NONE = 0;
      const static uchar N = 1;
      const static uchar NP = 2;
      const static uchar S = 3;
      const static uchar PP = 4;
      const static uchar CONJ = 5;
      const static uchar PERIOD = 6;
      const static uchar COLON = 7;
      const static uchar SEMICOLON = 8;
      const static uchar COMMA = 9;
      const static uchar LQU = 10;
      const static uchar RQU = 11;
      const static uchar LRB = 12;
      const static uchar RRB = 13;
      const static ulong NATOMS = 14;
    };

    class Atom: public Fixed {
    private:
      uchar _convert(const char *s);
    public:
      Atom(void){}
      Atom(uchar value): Fixed(value) {}
      Atom(const Atom &other): Fixed(other) {}
      Atom(const char *s): Fixed(_convert(s)) {}
      Atom(const std::string &s): Fixed(_convert(s.c_str())) {}
      ~Atom(void) { /* do nothing */ }
      operator const char *(void) const;
      const char *prolog(void) const;

      bool is_NP(void) const { return _value == Atoms::NP; }
      bool is_N(void) const { return _value == Atoms::N; }
      bool is_NorNP(void) const { return _value == Atoms::NP || _value == Atoms::N; }
      bool is_PP(void) const { return _value == Atoms::PP; }
      bool is_conj(void) const { return _value == Atoms::CONJ; }
      bool is_S(void) const { return _value == Atoms::S; }
      bool is_comma(void) const { return _value == Atoms::COMMA; }
      bool is_commaORperiod(void) const { return _value == Atoms::COMMA || _value == Atoms::PERIOD; }
      bool is_period(void) const { return _value == Atoms::PERIOD; }
      bool is_colon(void) const { return _value == Atoms::COLON; }
      bool is_semicolon(void) const { return _value == Atoms::SEMICOLON; }
      bool is_semiORcolon(void) const { return _value == Atoms::COLON || _value == Atoms::SEMICOLON; }
      bool is_LRB(void) const { return _value == Atoms::LRB; }
      bool is_RRB(void) const { return _value == Atoms::RRB; }
      bool is_LQU(void) const { return _value == Atoms::LQU; }
      bool is_RQU(void) const { return _value == Atoms::RQU; }
      bool is_punct(void) const { return _value >= Atoms::PERIOD && _value <= Atoms::RRB; }
    };

    inline std::ostream &operator<<(std::ostream &stream, const Atom &a){
      return stream << static_cast<const char *>(a);
    }
  }
}
