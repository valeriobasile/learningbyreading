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

    struct Statistics {
      double logderivs;
      
      ulong nequiv;
      ulong ntotal;
      
      ulong ncombines;
      ulong ncombines_zeros;
      ulong ncombines_reduced;
      ulong ncombines_rejected;
   
      Statistics(void){ reset(); }
   
      void reset(void){ memset(this, 0, sizeof(*this)); }

      Statistics &operator +=(const Statistics &other){
	ncombines += other.ncombines;
	ncombines_zeros += other.ncombines_zeros;
	ncombines_reduced += other.ncombines_reduced;
	ncombines_rejected += other.ncombines_rejected;
	return *this;
      }
    };

  }
}
