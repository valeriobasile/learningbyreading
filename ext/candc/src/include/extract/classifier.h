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
  namespace Extract {

    class Classifier {
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    public:
      ulong nevents(void) const;	// number of training sentences processed
      ulong ncontexts(void) const;	// number of unique training instances

      NLP::TagSet tagset(void) const;	// part of speech tagset
      NLP::Lexicon lexicon(void) const;	// lexicon produced

      // configuration information for extracting the model
      Classifier(NLP::Classifier::Classifier::Config &cfg, const std::string &preface, bool verbose);
      // shared, reference counted copy constructor
      Classifier(const Classifier &other);

      ~Classifier(void);

      // extract the model from the training file specified in options
      void extract(NLP::IO::Reader &reader);
    };

  }
}
