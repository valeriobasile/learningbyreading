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
#include "tagger/super.h"

namespace NLP { namespace Taggers {

Super::Config::Config(const OpPath *base, Mode mode,
		      const std::string &name, const std::string &desc)
  : Tagger::Config(name, desc, base, mode, 0.811, 200),
    category_cutoff(*this, "category_cutoff", "the minimum frequency cutoff for categories", 10),
    postags(*this, "postags", "the POS tag set", "//postags", &path),
    posdict(*this, "posdict", "the POS tag dictionary file path", "//posdict", &path),
    surr3words(*this, "surr3words", "use words up to three in front and behind", false),
    surr3tags(*this, "surr3tags", "use tags up to three in front and behind", false),
    biwords(*this, "biwords", "use bigrams of words up to two in front and behind", false),
    biwords_far(*this, "biwords_far", "use bigrams of words up to three in front and behind", false),
    triwords(*this, "triwords", "use trigrams of words up to two in front and behind", false),
    triwords_far(*this, "triwords_far", "use trigrams of words up to three in front and behind", false),
    bitags(*this, "bitags", "use bigram tags up to two in front and behind", true),
    bitags_far(*this, "bitags_far", "use bigram tags up to three in front and behind", false),
    tritags(*this, "tritags", "use trigram tags up to two in front and behind", false),
    tritags_far(*this, "tritags_far", "use trigram tags up to three in front and behind", false){}
typedef Tagger::Impl Base;

class Super::Impl: public Base {
public:
  const NLP::TagSet postags;
  TagSetDict posdict;

  Impl(Super::Config &cfg);
  virtual ~Impl(void);
protected:

  const bool use_surr3tags;
  const bool use_bitags;
  const bool use_bitags_far;
  const bool use_tritags;
  const bool use_tritags_far;

  TagAttributes t_attribs;
  TagAttributes pt_attribs;
  TagAttributes ppt_attribs;
  TagAttributes pppt_attribs;
  TagAttributes nt_attribs;
  TagAttributes nnt_attribs;
  TagAttributes nnnt_attribs;

  BiTagAttributes ppptppt_attribs;
  BiTagAttributes pptpt_attribs;
  BiTagAttributes ptt_attribs;
  BiTagAttributes ptnt_attribs;
  BiTagAttributes tnt_attribs;
  BiTagAttributes ntnnt_attribs;
  BiTagAttributes nntnnnt_attribs;
  BiTagAttributes nnntnnnnt_attribs;

  TriTagAttributes ppptpptpt_attribs;
  TriTagAttributes pptptt_attribs;
  TriTagAttributes pttnt_attribs;
  TriTagAttributes tntnnt_attribs;
  TriTagAttributes ntnntnnnt_attribs;
  TriTagAttributes nntnnntnnnnt_attribs;

  void add_surrounding_tags(const OffsetTags &tags, ulong i, PDF &dist) const;
  void add_surrounding_bitags(const OffsetTags &tags, ulong i, PDF &dist) const;
  void add_surrounding_tritags(const OffsetTags &tags, ulong i, PDF &dist) const;

  void reg_attributes(void);
  void create_unknowns(const Tagger::Config &cfg);
  void can_sentence(const Sentence &sent, State &state) const;
  const Tags &get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const;
  void add_features(const State &state, ulong i, PDF &pdf) const;
  void unpack_tags(State &state, Sentence &sent) const;
  void unpack_mtags(State &state, Sentence &sent, double BETA) const;
};

Super::Impl::Impl(Super::Config &cfg)
  : Base("Super", cfg),
    postags("postags", cfg.postags()),
    posdict("posdict", cfg.posdict(), cfg.tagdict_min(), klasses, postags),
    use_surr3tags(cfg.surr3tags()), use_bitags(cfg.bitags()), use_bitags_far(cfg.bitags_far()),
    use_tritags(cfg.tritags()), use_tritags_far(cfg.tritags_far()),
    t_attribs(postags), pt_attribs(postags), ppt_attribs(postags),
    pppt_attribs(postags), nt_attribs(postags), nnt_attribs(postags),
    nnnt_attribs(postags), ppptppt_attribs(postags), pptpt_attribs(postags),
    ptt_attribs(postags), ptnt_attribs(postags), tnt_attribs(postags),
    ntnnt_attribs(postags), nntnnnt_attribs(postags), nnntnnnnt_attribs(postags), ppptpptpt_attribs(postags)    , pptptt_attribs(postags), pttnt_attribs(postags), tntnnt_attribs(postags),     ntnntnnnt_attribs(postags), nntnnntnnnnt_attribs(postags){
  create_model(cfg);
}

Super::Impl::~Impl(void){}

inline void
Super::Impl::add_surrounding_tags(const OffsetTags &tags, ulong i, PDF &dist) const {
  _add_attribute(pt_attribs(tags[i - 1]), dist);
  _add_attribute(ppt_attribs(tags[i - 2]), dist);
  _add_attribute(nt_attribs(tags[i + 1]), dist);
  _add_attribute(nnt_attribs(tags[i + 2]), dist);
  if(use_surr3tags){
    _add_attribute(nnnt_attribs(tags[i + 3]), dist);
    _add_attribute(pppt_attribs(tags[i - 3]), dist);
  }
}


inline void
Super::Impl::add_surrounding_bitags(const OffsetTags &tags, ulong i, PDF &dist) const {
  _add_attribute(pptpt_attribs(tags[i - 2], tags[i - 1]), dist);
  _add_attribute(ptt_attribs(tags[i - 1], tags[i]), dist);
  _add_attribute(ptnt_attribs(tags[i - 1], tags[i + 1]), dist);
  _add_attribute(tnt_attribs(tags[i], tags[i + 1]), dist);
  _add_attribute(ntnnt_attribs(tags[i + 1], tags[i + 2]), dist);
  if(use_bitags_far){
    _add_attribute(nntnnnt_attribs(tags[i + 2], tags[i + 3]), dist);
    _add_attribute(nnntnnnnt_attribs(tags[i + 3], tags[i + 4]), dist);
    _add_attribute(ppptppt_attribs(tags[i - 3], tags[i - 2]), dist);
  }
}

inline void
Super::Impl::add_surrounding_tritags(const OffsetTags &tags, ulong i, PDF &dist) const {
  _add_attribute(pptptt_attribs(tags[i - 2], tags[i - 1], tags[i]), dist);
  _add_attribute(pttnt_attribs(tags[i - 1], tags[i], tags[i + 1]), dist);
  _add_attribute(tntnnt_attribs(tags[i], tags[i + 1], tags[i + 2]), dist);
  if(use_tritags_far){
    _add_attribute(ppptpptpt_attribs(tags[i - 3], tags[i - 2], tags[i - 1]), dist); 
    _add_attribute(ntnntnnnt_attribs(tags[i + 1], tags[i + 2], tags[i + 3]), dist);
    _add_attribute(nntnnntnnnnt_attribs(tags[i + 2], tags[i + 3], tags[i + 4]), dist);
  }
}

void
Super::Impl::reg_attributes(void){
  Base::reg_attributes();

  registry.reg(Types::pppt, pppt_attribs);
  registry.reg(Types::ppt, ppt_attribs);
  registry.reg(Types::pt, pt_attribs);
  registry.reg(Types::t, t_attribs);
  registry.reg(Types::nt, nt_attribs);
  registry.reg(Types::nnt, nnt_attribs);
  registry.reg(Types::nnnt, nnnt_attribs);

  registry.reg(Types::pppt_ppt_b, ppptppt_attribs);
  registry.reg(Types::ppt_pt_b, pptpt_attribs);
  registry.reg(Types::pt_t_b, ptt_attribs);
  registry.reg(Types::pt_nt_b, ptnt_attribs);
  registry.reg(Types::t_nt_b, tnt_attribs);
  registry.reg(Types::nt_nnt_b, ntnnt_attribs);
  registry.reg(Types::nnt_nnnt_b, nntnnnt_attribs);
  registry.reg(Types::nnnt_nnnnt_b, nnntnnnnt_attribs);

  registry.reg(Types::pppt_ppt_pt_c, ppptpptpt_attribs);
  registry.reg(Types::ppt_pt_t_c, pptptt_attribs);
  registry.reg(Types::pt_t_nt_c, pttnt_attribs);
  registry.reg(Types::t_nt_nnt_c, tntnnt_attribs);
  registry.reg(Types::nt_nnt_nnnt_c, ntnntnnnt_attribs);
  registry.reg(Types::nnt_nnnt_nnnnt_c, nntnnntnnnnt_attribs);
    
  registry.reg(Types::pst, pk_attribs);
  registry.reg(Types::ppst, ppkpk_attribs);
}

void
Super::Impl::create_unknowns(const Tagger::Config &){
  // allow all supertags in the case where the POS tags is unknown
  unknown_klasses.reserve(klasses.size() - 2);
  for(ulong i = 2; i < klasses.size(); ++i)
    unknown_klasses.push_back(Tag(i));
}

void
Super::Impl::can_sentence(const Sentence &sent, State &state) const {
  if(sent.words.size() > maxwords)
    throw NLP::Exception("sentence length exceeds maximum number of words for supertagger");

  state.raws = sent.words;
  if(sent.words.size() != sent.pos.size())
    throw NLP::Exception("the number of words and POS tags in a sentence must be the same");
  lexicon.can(state.raws, state.words);
  postags.tag(sent.pos, state.pos);
}

const Tags &
Super::Impl::get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const {
  const Tags *tags = 0;
  if(state.words[i].freq() < DICT_CUTOFF)
    tags = &posdict[state.pos[i]];
  else
    tags = &tagdict[state.words[i]];

  if(!tags->size())
    tags = &unknown_klasses;
  return *tags;
}

void
Super::Impl::add_features(const State &state, ulong i, PDF &pdf) const {
  _add_attribute(w_attribs(Types::w, state.words[i]), pdf);
  _add_attribute(t_attribs(state.pos[i]), pdf);
  add_surrounding_words(state.words, i, pdf);
  add_surrounding_tags(state.pos, i, pdf);
  if(use_bitags || use_bitags_far)
    add_surrounding_bitags(state.pos, i, pdf);
  if(use_tritags || use_tritags_far)
    add_surrounding_tritags(state.pos, i, pdf);
}

void
Super::Impl::unpack_tags(State &state, Sentence &sent) const {
  _unpack_tags(state, sent.super, false);
}

void
Super::Impl::unpack_mtags(State &state, Sentence &sent, const double BETA) const {
  _unpack_mtags(state, sent.msuper, BETA, false);
}

Super::Super(Super::Config &cfg): Tagger(cfg, new Impl(cfg)){}
Super::Super(Super &other): Tagger(other){}
Super::~Super(void){}

TagSet
Super::postags(void) const {
  return dynamic_cast<const Super::Impl *>(impl_)->postags;
}

TagSetDict
Super::posdict(void) const {
  return dynamic_cast<const Super::Impl *>(impl_)->posdict;
}

} }
