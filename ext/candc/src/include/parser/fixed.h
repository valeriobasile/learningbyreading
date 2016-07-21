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

    class Fixed {
    protected:
      uchar _value;
    public:
      Fixed(void): _value(0) {}
      Fixed(uchar value): _value(value) {}
      Fixed(const Fixed &other): _value(other._value) {}
      ~Fixed(void) { /* do nothing */ }

      Fixed &operator=(const Fixed &other){ _value = other._value; return *this; }

      uchar value(void) const { return _value; }

      operator void *(void) const { return (void *)static_cast<size_t>(_value); }
      bool operator !(void) const { return _value == 0; }
      Fixed &operator++(void){ ++_value; return *this; }
      operator uchar(void) const { return _value; }
      operator const char *(void) const;
    };

    inline bool operator==(const Fixed &f1, const Fixed &f2){ return f1.value() == f2.value(); }
    inline bool operator==(const Fixed &f1, uchar v){ return f1.value() == v; }
    inline bool operator!=(const Fixed &f1, const Fixed &f2){ return f1.value() != f2.value(); }
    inline bool operator!=(const Fixed &f1, uchar v){ return f1.value() != v; }
    inline bool operator<(const Fixed &f1, const Fixed &f2){ return f1.value() < f2.value(); }
    inline bool operator<(const Fixed &f1, uchar v){ return f1.value() < v; }
    inline bool operator<(const Fixed &f1, ushort v){ return f1.value() < v; }
    inline bool operator<(const Fixed &f1, ulong v){ return f1.value() < v; }
  }
}
