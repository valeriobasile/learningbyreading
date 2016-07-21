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

// NLP::Extract::_BaseImpl
// extracts features from POS tagged training data
// saves the extracted model into the specified model directory
// which is then loaded by NLP::MaxEnt::GIS for estimating
// the parameters of the model, and NLP::Taggers::POS for tagging

#include "base.h"

#include "cluster.h"

#include "config/config.h"

#include "model/types.h"
#include "model/model.h"
#include "model/registry.h"

#include "io/reader.h"
#include "io/writer.h"

#include "tagger/taghist.h"
#include "tagger/tagdict.h"
#include "tagger/tagger.h"

#include "extract/feature.h"
#include "extract/contexts.h"
#include "extract/attributes.h"

#include "timer.h"
#include "share.h"

namespace NLP { namespace Extract {

// private implementation, which is shared
class _BaseImpl: public Shared {
protected:
  virtual void reg_types(void);

  virtual void _generate_counts(const NLP::Sentence &sent) = 0;
  virtual void _generate_features(const NLP::Sentence &sent) = 0;
  virtual void _generate_contexts(const NLP::Sentence &sent) = 0;
  virtual void _apply_cutoffs(void) = 0;
  virtual void _make_unknowns(void) const = 0;

  virtual void _pass1(NLP::IO::Reader &reader, bool save_klasses = true);
  virtual void _pass2(NLP::IO::Reader &reader);
  virtual void _pass2_postload(NLP::IO::Reader &) { }
  virtual void _pass3(NLP::IO::Reader &reader);
  virtual void _save_contexts(void);
public:
  template <class This, class Fn>
  void _apply(NLP::IO::Reader &reader, const std::string &msg, This th, Fn fn){
    reader.reset();

    NLP::Sentence sent;

    // determine the number of sentences for MPI
    ulong total_sents;
    if(Cluster::USE_MPI){
      for(total_sents = 0; reader.next(sent); total_sents++)
	;
      reader.reset();
    }

    nevents = 0;
    // loop over the sentences in the training data
    const ulong LOG_INTERVAL = 100000;
    ulong next_log = LOG_INTERVAL;
    ulong cur_sent = 0;
    while(reader.next(sent)){
      if(!Cluster::USE_MPI || !cfg.model.split_input() || cur_sent % Cluster::size == Cluster::rank)
        (th->*fn)(sent);
      cur_sent++;
      if(VERBOSE && Cluster::rank == 0 && nevents >= next_log){
        std::cerr << msg << ": " << next_log << " events" << std::endl;
        next_log += LOG_INTERVAL;
      }
    }
    if(VERBOSE && Cluster::rank == 0)
      std::cerr << msg << ": " << nevents << " events" << std::endl;
  }

  NLP::Model::Config &cfg;
  std::string PREFACE;
  const bool VERBOSE;
  const bool MERGE;
  const bool SORT;

  const std::string NONE;

  ulong nevents;
  ulong ncontexts;

  NLP::Model::Registry registry;

  NLP::Lexicon lexicon;
  NLP::TagSet klasses;
  Attributes attributes;
  Contexts contexts;

  NLP::Lexicon counts;

  Context context;

  _BaseImpl(NLP::Model::Config &cfg, const std::string &PREFACE, bool VERBOSE);
  virtual ~_BaseImpl(void);

  virtual void extract(NLP::IO::Reader &reader, bool MKDIR);
};

} }
