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

// NLP::Extract::Exp
// extracts features from named entity tagged training data
// in the experimental column-format which allows additional
// 'experimental' features to be tested with very little effort
// saves the extracted model into the specified model directory
// which is then loaded by NLP::MaxEnt::GIS for estimating
// the parameters of the model, and NLP::Tagger::Exp for tagging

namespace NLP {
  namespace Extract {

    class Exp {
    public:
      const MaxEnt::Config config;      // tagger parameters
      const MaxEnt::Options options;	// estimation parameters
      MaxEnt::Info info;		// model parameters

      const Types types;		// feature types

      ulong nsentences(void) const;	// number of training sentences processed
      ulong nwords(void) const;		// number of training words processed
      ulong ncontexts(void) const;	// number of unique training instances

      NLP::TagSet tagset(void) const;	// named entity tagset
      NLP::Lexicon lexicon(void) const;	// lexicon produced

      // configuration information for extracting the model
      Exp(const MaxEnt::Config &config, const MaxEnt::Options &options, const Types &types);
      // shared, reference counted copy constructor
      Exp(const Exp &other);

      ~Exp(void);

      // extract the model from the training file specified in options
      void extract(void);
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

  }
}
