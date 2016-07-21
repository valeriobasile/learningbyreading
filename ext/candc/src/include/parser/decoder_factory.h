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

    class DecoderFactory: public Decoder {
    public:
      static void check(const std::string &name);
    private:
      Decoder *decoder;
    public:
      DecoderFactory(const std::string &name);
      virtual ~DecoderFactory(void){ delete decoder; }

      virtual double best_score(const SuperCat *sc){
	return decoder->best_score(sc);
      }
      const SuperCat *best_equiv(const SuperCat *sc){
	return decoder->best_equiv(sc);
      }
      virtual const SuperCat *best(Chart &chart){
	return decoder->best(chart);
      }
    };

  }
}
