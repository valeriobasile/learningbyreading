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

// NLP::Extract::POS
// extracts features from POS tagged training data
// saves the extracted model into the specified model directory
// which is then loaded by NLP::MaxEnt::GIS for estimating
// the parameters of the model, and NLP::Taggers::POS for tagging

namespace NLP {
  namespace Extract {

    class POS {
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    public:
      ulong nevents(void) const;	// number of training words processed
      ulong ncontexts(void) const;	// number of unique training instances

      NLP::TagSet tagset(void) const;	// part of speech tagset
      NLP::Lexicon lexicon(void) const;	// lexicon produced

      // configuration information for extracting the model
      POS(NLP::Taggers::POS::Config &cfg, const std::string &preface, bool verbose);
      // shared, reference counted copy constructor
      POS(const POS &other);

      ~POS(void);

      // extract the model from the training file specified in options
      void extract(NLP::IO::Reader &reader);
    };

  }
}
