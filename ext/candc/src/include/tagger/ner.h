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

// NLP::Tagger::NER
// takes POS/chunk tagged input and add named entity tags to the output
// there are two interfaces:
//   a input/output file interface
//   a single input sentence/POS tags/chunk tags interface

namespace NLP {
  namespace Taggers {

    class NER: public Tagger {
    public:
      class Config: public Tagger::Config {
      public:
	OpPath postags;
	OpPath chunktags;
	OpPath gazetteers;

	Model::Types types;

      	Config(const OpPath *base = 0, Mode mode = DECODE,
	       const std::string &name = "ner",
	       const std::string &desc = "NE tagger config");
      };
    public:
      NER(NER::Config &cfg);
      NER(NER &other);
      virtual ~NER(void);

      NLP::TagSet postags(void) const;
      NLP::TagSet chunktags(void) const;

      // the set of tags permissible for words not seen in the training data
      const NLP::Tags &unknown_tags(void) const;
      // the set of tags permissible for numbers not seen in the training data
      const NLP::Tags &number_tags(void) const;
    private:
      // private implementation trick
      class Impl;
    };

  }
}
