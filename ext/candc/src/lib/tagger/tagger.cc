// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "tagger/_baseimpl.h"

namespace NLP { namespace Taggers {

Tagger::Config::Config(const std::string &name, const std::string &desc,
		       const OpPath *base, Mode mode, double SIGMA, ulong NITER)
  : NLP::Model::Config(name, desc, base, mode, SIGMA, NITER),

    tagdict(*this, SPACE, "tagdict", "the tag dictionary file path", "//tagdict", &path),
    unknowns(*this, "unknowns", "the set of tags for unknown words", "//unknowns", &path),

    cutoff_default(*this, "cutoff_default", "the minimum frequency cutoff for features", 1),
    cutoff_words(*this, "cutoff_words", "the minimum frequency cutoff for word features", 1),
    cutoff_attribs(*this, "cutoff_attribs", "the minimum frequency cutoff for attributes", 1),

    rare_cutoff(*this, "rare_cutoff", "the word frequency for which rare word features are used", 5),

    beam_width(*this, "beam_width", "the number of best tags to keep in the beam", 5),
    beam_ratio(*this, "beam_ratio", "the ratio of the worst:best tags in the beam", 0.005),
    forward_beam_ratio(*this, "forward_beam_ratio", "the beta*ratio of the worst:best tags in the forward step", 0.01),

    tagdict_min(*this, "tagdict_min", "the minimum frequency for adding a word-tag pair to the tag dict", 5),
    tagdict_ratio(*this, "tagdict_ratio", "the ratio of the min:max frequency of word-tag pairs in the tag dict", 500),
    maxwords(*this, SPACE, "maxwords", "maximum sentence length the tagger will accept", 250){}

Tagger::Tagger(Config &cfg, Impl *impl): cfg(cfg), impl_(impl){}

Tagger::Tagger(Tagger &other): cfg(other.cfg), impl_(share(other.impl_)){}

Tagger::~Tagger(void){ release(impl_); }

TagSet Tagger::tagset(void) const { return impl_->klasses; }
Lexicon Tagger::lexicon(void) const { return impl_->lexicon; }
TagDict Tagger::tagdict(void) const { return impl_->tagdict; }

const Tags &Tagger::unknown_tags(void) const { return impl_->unknown_klasses; }

State *Tagger::create_state(void) const { return impl_->create_state(); }

void
Tagger::begin_document(State *state) const {
  impl_->begin_document(state);
}

void
Tagger::tag(NLP::Sentence &sent, const Algorithm alg,
	    const ulong DICT_CUTOFF, State *state) const {
  impl_->tag(sent, alg, DICT_CUTOFF, state);
}

void
Tagger::tag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
	    const Algorithm alg, const ulong DICT_CUTOFF) const {
  impl_->tag(reader, writer, alg, DICT_CUTOFF);
}

void
Tagger::mtag(NLP::Sentence &sent, const Algorithm alg,
	     const ulong DICT_CUTOFF, const double BETA, State *state) const {
  impl_->mtag(sent, alg, DICT_CUTOFF, BETA, state);
}

void
Tagger::mtag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
	     const Algorithm alg, const ulong DICT_CUTOFF,
	     const double BETA) const {
  impl_->mtag(reader, writer, alg, DICT_CUTOFF, BETA);
}

} }
