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

  using namespace IO;
  using namespace Config;

  namespace Classifier {

    class Classifier {
    public:
      class Config: public Model::Config {
      public:
	Op<ulong> cutoff;

	Config(const OpPath *base = 0, Mode mode = DECODE,
	       const std::string &name = "classifier",
	       const std::string &desc = "classifier config",
	       double SIGMA = 1.414, ulong NITER = 400);
	virtual ~Config(void){ /* do nothing */ }
      };
    public:
      Config &cfg;

      virtual ~Classifier(void);

      NLP::TagSet tagset(void) const;
      NLP::Lexicon lexicon(void) const;

      virtual Tag classify(const RawWords &attrs) const;

      // private implementation trick
      class Impl;
    protected:
      Classifier(Classifier::Config &cfg, Impl *impl);
      Classifier(Classifier &other);

      Impl *impl_;
    };

  }
}

