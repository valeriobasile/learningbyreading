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

// NLP::Extract::SuperExp
// extracts features from supertagged training data in the experimental column format
// saves the extracted model into the specified model directory
// which is then loaded by NLP::MaxEnt::GIS for estimating
// the parameters of the model, and NLP::Tagger::SuperExp for tagging

namespace NLP {
  namespace Extract {

    class SuperExp {
    public:
      const MaxEnt::Config config;      // tagger parameters
      const MaxEnt::Options options;	// estimation parameters
      MaxEnt::Info info;		// model parameters

      ulong nsentences(void) const;	// number of training sentences processed
      ulong nwords(void) const;		// number of training words processed
      ulong ncontexts(void) const;	// number of unique training instances

      NLP::TagSet tagset(void) const;	// supertag tagset
      NLP::Lexicon lexicon(void) const;	// lexicon produced

      // configuration information for extracting the model
      SuperExp(const MaxEnt::Config &config, const MaxEnt::Options &options);
      // shared, reference counted copy constructor
      SuperExp(const SuperExp &other);

      ~SuperExp(void);

      // extract the model from the training file specified in options
      void extract(void);
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

  }
}
