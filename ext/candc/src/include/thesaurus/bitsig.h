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


template <ulong B>
class Signature {
public:
  static const ulong NBITS = B;
  static const ulong BITSPERULONG = 32;
  static const ulong NULONG = NBITS/BITSPERULONG;
private:
  ulong _mask[NULONG];
public:
  Signature(void) {};

  void set(ulong bit) { _mask[bit/BITSPERULONG] |= 1 << (bit % BITSPERULONG); };
  void clearall(void) { memset(_mask, 0, sizeof(_mask)); };

  bool intersect(const Signature &s) const {
    for(ulong i = 0; i < NULONG; i++)
      if(s._mask[i] & _mask[i])
        return 1;
    return 0;
  }
  void print(FILE *file) const {
    for(ulong i = 0; i < NULONG; i++){
      for(ulong j = 0; j < BITSPERULONG; j++){
        if(_mask[i] & (1 << j))
          fputc('1', file);
        else
          fputc('0', file);
      }
      fputc('\n', file);
    }
  }
};
