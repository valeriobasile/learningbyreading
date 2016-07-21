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

    class GRsPrinter: public DepsPrinter {
    protected:
      virtual void get_grs(const SuperCat *sc, Sentence &sent);
      virtual void unary(Sentence &){}
      virtual void derivation(const SuperCat *sc, Sentence &sent);
    public:
      FilledDeps filled;
      GRs grs;

      GRsPrinter(Categories &cats, const Format FORMAT,
		 IO::Output &out, IO::Log &log)
	: DepsPrinter(cats, FORMAT, out, log){}

      virtual ~GRsPrinter(void){ /* do nothing */ }

      virtual void parsed(const SuperCat *sc, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	filled.clear();
	grs.clear();
	StreamPrinter::parsed(sc, sent, BETA, DICT_CUTOFF);
      }
    };
  }
}
