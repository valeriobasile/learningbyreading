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

// define some standard typedefs for unsigned types
// and include some auxilliary string functions

#include "utils/aux_strings.h"

inline float
div0(float num, float denom){
  return denom ? num/denom : 0.0;
}

