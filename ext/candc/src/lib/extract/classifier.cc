// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "extract/_baseimpl.h"

#include "classifier.h"
#include "extract/classifier.h"

using namespace std;

namespace NLP { namespace Extract {

// private implementation, which is shared
class Classifier::_Impl: public _BaseImpl {
protected:
  template <class TC>
  void _generate(TC &tc, const Sentence &sent);

  void _generate_counts(const NLP::Sentence &sent);
  void _generate_features(const NLP::Sentence &sent);
  void _generate_contexts(const NLP::Sentence &sent);
  void _apply_cutoffs(void);
  void _make_unknowns(void) const {}
public:
  NLP::Classifier::Classifier::Config &cfg;

  _Impl(NLP::Classifier::Classifier::Config &cfg, const std::string &PREFACE, bool VERBOSE);
  virtual ~_Impl(void);
};

// apply cutoffs to the generated features
void
Classifier::_Impl::_apply_cutoffs(void){
  attributes.apply_cutoff(cfg.cutoff());
}

template <class TC>
void
Classifier::_Impl::_generate(TC &tc, const NLP::Sentence &sent){
  for(RawWords::const_iterator i = sent.words.begin() + 1; i != sent.words.end(); ++i)
    attributes(tc, Types::null, *i);
}

// count the number of tags, words and word/tag pairs in the given sentence
void
Classifier::_Impl::_generate_counts(const NLP::Sentence &sent){
  counts.add(sent.words[0], 1);
  for(RawWords::const_iterator i = sent.words.begin() + 1; i != sent.words.end(); ++i) 
    lexicon.add(*i, 1);
  ++nevents;
}

// count the features for each training instance in the given sentence
void
Classifier::_Impl::_generate_features(const NLP::Sentence &sent){
  Tag klass = klasses[sent.words[0]];
  _generate(klass, sent);
  ++nevents;
}

// add the features for each training instance to the context
// and dump it model/contexts for the given sentence
void
Classifier::_Impl::_generate_contexts(const NLP::Sentence &sent){
  context.resize(0);

  Tag klass = klasses[sent.words[0]];

  _generate(context, sent);

  if(context.size() > 0){
    std::sort(context.begin(), context.end());
    contexts.add(klass, context);
  }
  ++nevents;
}

Classifier::_Impl::_Impl(NLP::Classifier::Classifier::Config &cfg,
			 const std::string &PREFACE, bool VERBOSE)
  : _BaseImpl(cfg, PREFACE, VERBOSE), cfg(cfg){}

Classifier::_Impl::~_Impl(void) {}

Classifier::Classifier(NLP::Classifier::Classifier::Config &cfg,
		       const std::string &PREFACE, bool VERBOSE)
  : _impl(new _Impl(cfg, PREFACE, VERBOSE)){}

Classifier::Classifier(const Classifier &other)
  : _impl(share(other._impl)){}

Classifier::~Classifier(void){
  release(_impl);
}

ulong Classifier::nevents(void) const { return _impl->nevents; };
ulong Classifier::ncontexts(void) const { return _impl->ncontexts; };

TagSet Classifier::tagset(void) const { return _impl->klasses; };
Lexicon Classifier::lexicon(void) const { return _impl->lexicon; };

void Classifier::extract(NLP::Reader &reader){ _impl->extract(reader, true); }

} }
