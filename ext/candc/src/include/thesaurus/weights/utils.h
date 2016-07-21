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
  namespace Thesaurus {

    template <class T> float f(T t){ return t->f(); };
    template <class T> float n(T t){ return t->n(); };
    template <class T> float p(T t){ return t->p(); };
    template <class T> float q(T t){ return t->q(); };
    template <class T> float F(T t){ return t->ftotal; };
    template <class T> float N(T t){ return t->ntotal; };
    template <class T> float w(T t){ return t->score(); };

    inline float cut(float value, float cutoff){ return value > cutoff ? value : cutoff; }
    inline float log2(float value){ return log(value)/M_LN2; }

  }
}
