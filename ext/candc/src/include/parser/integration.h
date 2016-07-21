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

    using namespace NLP::Config;
    using namespace NLP::Taggers;

    class Integration {
    public:
      typedef std::vector<double> Betas;
      typedef std::vector<ulong> DictCutoffs;

      class Config: public Cfg {
      public:
	Op<ulong> start;
	Op<Betas> betas;
	Op<DictCutoffs> dict_cutoffs;

	Config(const std::string &name = "int",
	       const std::string &desc = "supertagger and parser integration config");
	virtual ~Config(void){ /* do nothing */ }

	virtual void check(void);
      };
    public:
      long START;
      Betas BETAS;
      double MIN_BETA;
      DictCutoffs DICT_CUTOFFS;

      Super super;
      Categories cats;
      Parser parser;

      ulong nsentences;
      ulong nwords;

      ulong nexceptions;

      ulong nfail_nospan;
      ulong nfail_explode;
      ulong nfail_nospan_explode;
      ulong nfail_explode_nospan;

      ulong nrepairs;

      Statistics global_stats;

      std::vector<ulong> nsuccesses;

      Integration(Integration::Config &int_cfg,
		  Super::Config &super_cfg,
		  Parser::Config &parser_cfg,
		  Sentence &sent,
		  ulong load = Parser::LOAD_WEIGHTS);

      ~Integration(void);

      bool parse(Sentence &sent, Decoder &decoder, Printer &printer, bool USE_SUPER = true);

    protected:
      Taggers::State *const super_state;
    };

  }
}
