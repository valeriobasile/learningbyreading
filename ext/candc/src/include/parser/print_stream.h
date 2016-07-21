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

    class SuperCat;

    class StreamPrinter: public Printer {
    public:
      typedef int Format;

      const static Format FMT_WORDS = 1 << 0;
      const static Format FMT_LEMMA = 1 << 1;
      const static Format FMT_POS = 1 << 2;
      const static Format FMT_CHUNK = 1 << 3;
      const static Format FMT_NER = 1 << 4;
      const static Format FMT_SUPER = 1 << 5;
      const static Format FMT_CAT = 1 << 6;

      const static Format FMT_WS = 1 << 7;

      const static Format FMT_FORCE_WORDS = 1 << 8;
      const static Format FMT_JULIA_SLOTS = 1 << 9;
      const static Format FMT_PRINT_UNARY = 1 << 10;

      const static Format FMT_DEV = FMT_WORDS | FMT_POS | FMT_SUPER | FMT_JULIA_SLOTS | FMT_WS;

      const static Format FMT_TREC = FMT_WORDS | FMT_LEMMA | FMT_POS | FMT_NER | FMT_PRINT_UNARY | FMT_WS;

      const static Format FMT_ALL = FMT_WORDS | FMT_LEMMA | FMT_POS | FMT_CHUNK | FMT_NER | FMT_SUPER |
                                    FMT_FORCE_WORDS | FMT_PRINT_UNARY | FMT_WS;
    public:
      const Format FORMAT;

      IO::Output &out;
      IO::Log &log;

      StreamPrinter(Categories &cats, const Format FORMAT,
		    IO::Output &out, IO::Log &log)
	: Printer(cats), FORMAT(FORMAT), out(out), log(log){}

      virtual ~StreamPrinter(void){ /* do nothing */ }

      virtual void parsed(const SuperCat *root, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	set(true, true, "parsed", BETA, DICT_CUTOFF);

	sent.cats.clear();

	if(root){
	  log.stream << nsentences << " parsed at B=" << BETA << ", K=" << DICT_CUTOFF << std::endl;
	  log.stream << nsentences << " coverage " << coverage() << '%' << std::endl;
	  derivation(root, sent);
	  lexical(sent);
	}else{
	  if(FORMAT & FMT_PRINT_UNARY){
	    log.stream << nsentences << " parsed unary " << std::endl;
	    unary(sent);
	    lexical(sent);
	  }else{
	    log.stream << nsentences << " ignored unary " << std::endl;
	  }
	}

	out.stream << '\n';
      }

      virtual void stats(const Statistics &stats){
	Printer::stats(stats);
	log.stream << nsentences << " stats " << stats.logderivs << ' ' << stats.nequiv
		   << ' ' << stats.ntotal
		   << " comb " << stats.ncombines << ' ' << stats.ncombines_zeros
		   << ' ' << stats.ncombines_rejected
		   << ' ' << stats.ncombines_reduced << std::endl;
      }

      virtual void attempted(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	sent.cats.clear();
	set(false, false, REASON, BETA, DICT_CUTOFF);
	log.stream << (nsentences + 1) << " attempt " <<  reason << " at B=" << beta << ", K=" << dict_cutoff << std::endl;
      }

      virtual void failed(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	sent.cats.clear();
	set(true, false, REASON, BETA, DICT_CUTOFF);
	log.stream << nsentences << " failed " << reason << " at B=" << beta << ", K=" << dict_cutoff << std::endl;

	if(FORMAT & FMT_FORCE_WORDS)
	  lexical(sent);
	out.stream << '\n';
      }

      virtual void error(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	sent.cats.clear();
	set(true, false, "exception", BETA, DICT_CUTOFF);
	log.stream << nsentences << " exception at B=" << beta << ", K=" << dict_cutoff << ' ' << REASON << std::endl;

	if(FORMAT & FMT_FORCE_WORDS)
	  lexical(sent);
	out.stream << '\n';
      }
    };

  }
}
