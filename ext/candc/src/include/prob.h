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

// NLP::Probability, NLP::PDF and NLP::Actives
// probability distributions and per class active features

namespace NLP {
  // probability
  typedef double Probability;

  // probability distribution
  typedef std::vector<Probability> PDF;

  // number of active features for each class
  typedef std::vector<ulong> Actives;

  template <class Vector>
  void
  zero(Vector &vec){ memset(&vec.front(), 0, sizeof(vec[0])*vec.size()); }

  template <class Vector>
  void
  identity(Vector &vec){ vec.assign(vec.size(), 1.0); }

}
