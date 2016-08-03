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
#include "tagger/tagsetdict.h"
#include "tagger/chunk.h"

namespace NLP { namespace Taggers {

Chunk::Config::Config(const OpPath *base, Mode mode,
		      const std::string &name, const std::string &desc)
  : Tagger::Config(name, desc, base, mode, 0.811, 200),
    postags(*this, "postags", "the POS tag set", "//postags", &path){}

typedef Tagger::Impl Base;

class Chunk::Impl: public Base {
public:
  const NLP::TagSet postags;

  Impl(Chunk::Config &cfg);
  virtual ~Impl(void);
protected:
  TagAttributes t_attribs;
  TagAttributes pt_attribs;
  TagAttributes ppt_attribs;
  TagAttributes nt_attribs;
  TagAttributes nnt_attribs;

  BiTagAttributes ptt_attribs;
  BiTagAttributes pptpt_attribs;
  BiTagAttributes ptnt_attribs;
  BiTagAttributes tnt_attribs;
  BiTagAttributes ntnnt_attribs;

  void add_surrounding_tags(const OffsetTags &tags, ulong i, PDF &dist) const;
  void add_surrounding_bitags(const OffsetTags &tags, ulong i, PDF &dist) const;

  void reg_attributes(void);
  void create_unknowns(const Tagger::Config &cfg);
  void can_sentence(const Sentence &sent, State &state) const;
  const Tags &get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const;
  void add_features(const State &state, ulong i, PDF &pdf) const;
  void unpack_tags(State &state, Sentence &sent) const;
  void unpack_mtags(State &state, Sentence &sent, double BETA) const;
};

Chunk::Impl::Impl(Chunk::Config &cfg)
  : Base("Chunk", cfg),
    postags("postags", cfg.postags()),
    t_attribs(postags), pt_attribs(postags), ppt_attribs(postags),
    nt_attribs(postags), nnt_attribs(postags),
    ptt_attribs(postags), pptpt_attribs(postags), ptnt_attribs(postags),
    tnt_attribs(postags), ntnnt_attribs(postags){
  create_model(cfg);
}

Chunk::Impl::~Impl(void){}

inline void
Chunk::Impl::add_surrounding_tags(const OffsetTags &tags, ulong i, PDF &dist) const {
  _add_attribute(pt_attribs(tags[i - 1]), dist);
  _add_attribute(ppt_attribs(tags[i - 2]), dist);
  _add_attribute(nt_attribs(tags[i + 1]), dist);
  _add_attribute(nnt_attribs(tags[i + 2]), dist);
}

inline void
Chunk::Impl::add_surrounding_bitags(const OffsetTags &tags, ulong i, PDF &dist) const {
  _add_attribute(ptt_attribs(tags[i - 1], tags[i]), dist);
  _add_attribute(pptpt_attribs(tags[i - 2], tags[i - 1]), dist);
  _add_attribute(ptnt_attribs(tags[i - 1], tags[i + 1]), dist);
  _add_attribute(tnt_attribs(tags[i], tags[i + 1]), dist);
  _add_attribute(ntnnt_attribs(tags[i + 1], tags[i + 2]), dist);
}

void
Chunk::Impl::reg_attributes(void){
  Base::reg_attributes();

  registry.reg(Types::ppt, ppt_attribs);
  registry.reg(Types::pt, pt_attribs);
  registry.reg(Types::t, t_attribs);
  registry.reg(Types::nt, nt_attribs);
  registry.reg(Types::nnt, nnt_attribs);

  registry.reg(Types::ppt_pt_b, pptpt_attribs);
  registry.reg(Types::pt_t_b, ptt_attribs);
  registry.reg(Types::pt_nt_b, ptnt_attribs);
  registry.reg(Types::t_nt_b, tnt_attribs);
  registry.reg(Types::nt_nnt_b, ntnnt_attribs);

  registry.reg(Types::pst, pk_attribs);
  registry.reg(Types::ppst, ppkpk_attribs);
}

void
Chunk::Impl::create_unknowns(const Tagger::Config &){
  unknown_klasses.reserve(klasses.size() - 2);
  for(ulong i = 2; i < klasses.size(); ++i)
    unknown_klasses.push_back(Tag(i));
}

void
Chunk::Impl::can_sentence(const Sentence &sent, State &state) const {
  if(sent.words.size() > maxwords)
    throw NLP::Exception("sentence length exceeds maximum number of words for chunker");

  state.raws = sent.words;
  if(sent.words.size() != sent.pos.size())
    throw NLP::Exception("the number of words and POS tags in a sentence must be the same");
  lexicon.can(state.raws, state.words);
  postags.tag(sent.pos, state.pos);
}

const Tags &
Chunk::Impl::get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const {
  if(state.words[i].freq() >= DICT_CUTOFF){
    Tags &tags = tagdict[state.words[i]];
    if(tags.size())
      return tags;
  }

  return unknown_klasses;
}

void
Chunk::Impl::add_features(const State &state, ulong i, PDF &pdf) const {
  _add_attribute(w_attribs(Types::w, state.words[i]), pdf);
  _add_attribute(t_attribs(state.pos[i]), pdf);
  add_surrounding_words(state.words, i, pdf);
  add_surrounding_tags(state.pos, i, pdf);
  add_surrounding_bitags(state.pos, i, pdf);
}

void
Chunk::Impl::unpack_tags(State &state, Sentence &sent) const {
  _unpack_tags(state, sent.chunks, false);
}

void
Chunk::Impl::unpack_mtags(State &state, Sentence &sent, const double BETA) const {
  _unpack_mtags(state, sent.mchunks, BETA, false);
}

Chunk::Chunk(Chunk::Config &cfg): Tagger(cfg, new Impl(cfg)){}
Chunk::Chunk(Chunk &other): Tagger(other){}
Chunk::~Chunk(void){}

TagSet
Chunk::postags(void) const {
  return dynamic_cast<const Chunk::Impl *>(impl_)->postags;
}

} }
