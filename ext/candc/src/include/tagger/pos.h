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

// NLP::Tagger::POS
// takes tokenized text input and adds part of speech tags to the output
// there are two interfaces:
//   a input/output file interface
//   a single input sentence interface

namespace NLP {
  namespace Taggers {

    class POS: public Tagger {
    public:
      class Config: public Tagger::Config {
      public:
	OpPath number_unknowns;

      	Config(const OpPath *base = 0, Mode mode = DECODE,
	       const std::string &name = "pos",
	       const std::string &desc = "POS tagger config");
      };
    public:
      POS(POS::Config &cfg);
      POS(POS &other);

      virtual ~POS(void);

      // the set of tags permissible for numbers not seen in the training data
      const NLP::Tags &number_tags(void) const;
    private:
      // private implementation trick
      class Impl;
    };

  }
}
