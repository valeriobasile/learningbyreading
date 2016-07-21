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

    class InsideOutside;

    class DepsRecallDecoder: public Decoder {
    public:
      InsideOutside *inside_outside;

      DepsRecallDecoder(void);
      virtual ~DepsRecallDecoder(void);

      virtual double best_score(const SuperCat *sc);
      virtual const SuperCat *best(Chart &chart);
    };
  }
}
