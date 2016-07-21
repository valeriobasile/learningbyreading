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

namespace NLP { namespace Extract {

// private implementation, which is shared
class _TaggerImpl: public _BaseImpl {
protected:
  virtual void _apply_cutoffs(void);

  virtual void _pass1(NLP::IO::Reader &reader, bool save_klasses = true);
public:
  NLP::Taggers::Tagger::Config &cfg;

  const std::string SENTINEL;
  const std::string SENTINEL2;
  const std::string SENTINEL3;
  NLP::Lexicon tagdict;

  _TaggerImpl(NLP::Taggers::Tagger::Config &cfg, const std::string &PREFACE, bool VERBOSE);
  virtual ~_TaggerImpl(void);

  virtual void extract(NLP::IO::Reader &reader, bool MKDIR);
};

} }
