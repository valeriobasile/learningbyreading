// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Tagger::NER
// takes POS/chunk tagged input and add named entity tags to the output
// there are two interfaces:
//   a input/output file interface
//   a single input sentence/POS tags/chunk tags interface

#include "tagger/_baseimpl.h"
#include "tagger/ner.h"

#include "analyser.h"
#include "gazetteers.h"
#include "wordtype.h"
#include "tagger/unigram.h"

namespace NLP { namespace Taggers {

NER::Config::Config(const OpPath *base, Mode mode,
		    const std::string &name, const std::string &desc)
  : Tagger::Config(name, desc, base, mode, 1.414, 400),
    postags(*this, "postags", "the POS tag set", "//postags", &path),
    chunktags(*this, "chunktags", "the chunk tag set", "//chunktags", &path),
    gazetteers(*this, "gazetteers", "the gazetteer configuration file", "//gazetteers", &path),
    types("NE tagger"){
  reg(types);
}

static const Affix empty("+");
static const Affix more("m");
static const Affix uc("uc"), mc("mc"), tc("tc"), lc("lc");
static const Affix numbers[16] = {
    Affix("0"), Affix("1"), Affix("2"), Affix("3"), Affix("4"),
    Affix("5"), Affix("6"), Affix("7"), Affix("8"), Affix("9"),
    Affix("10"), Affix("11"), Affix("12"), Affix("13"), Affix("14"),
    Affix("15")
};

typedef Tagger::Impl Base;

class NER::Impl: public Base {
public:
  const Types types;
  const NLP::TagSet postags;
  const NLP::TagSet chunktags;

  Impl(NER::Config &cfg);
  ~Impl(void);
protected:
  const Gazetteers gaz;
  Unigram unigrams;

  TagAttributes t_attribs;
  TagAttributes pt_attribs;
  TagAttributes ppt_attribs;
  TagAttributes nt_attribs;
  TagAttributes nnt_attribs;

  TagAttributes c_attribs;
  TagAttributes pc_attribs;
  TagAttributes ppc_attribs;
  TagAttributes nc_attribs;
  TagAttributes nnc_attribs;

  // orthographic types
  AffixAttributes a_attribs;

  BinAttributes dig_attribs;
  BinAttributes hyph_attribs;
  BinAttributes per_attribs;
  BinAttributes pun_attribs;
  BinAttributes uc_attribs;

  BinAttributes an_attribs;
  BinAttributes num_attribs;
  BinAttributes rom_attribs;
  BinAttributes ini_attribs;
  BinAttributes acr_attribs;

  // gazetteer types
  BinAttributes gc_attribs;
  BinAttributes gf_attribs;
  BinAttributes gs_attribs;
  BinAttributes gl_attribs;
  
  BinAttributes gpc_attribs;
  BinAttributes gpf_attribs;
  BinAttributes gps_attribs;
  BinAttributes gpl_attribs;

  BinAttributes gnc_attribs;
  BinAttributes gnf_attribs;
  BinAttributes gns_attribs;
  BinAttributes gnl_attribs;

  // gazetteer and sentence position types
  BinAttributes xbs_attribs;
  BinAttributes xms_attribs;

  TagAttributes last_attribs;

  TagAttributes nu_attribs;
  TagAttributes nnu_attribs;

  UniAttributes wt_attribs;

  void add_prefixes(const RawWord &raw, PDF &dist) const;
  void add_suffixes(const RawWord &raw, PDF &dist) const;
  void add_special(const RawWord &raw, PDF &dist) const;

  void add_surrounding_pos(const OffsetTags &tags, ulong i, PDF &dist) const;
  void add_surrounding_chunks(const OffsetTags &chunks, ulong i, PDF &dist) const;
  void add_wordtypes(const RawWords &sentence, ulong i, PDF &dist) const;

  void add_last(const State &state, const RawWord &raw, PDF &dist) const;
  void add_gazetteers(const RawWord &raw, ulong i, PDF &dist) const;
  void add_prev_gazetteers(const RawWord &raw, PDF &dist) const;
  void add_next_gazetteers(const RawWord &raw, PDF &dist) const;
  void add_unigrams(const OffsetWords &words, ulong i, PDF &dist) const;

  void reg_attributes(void);
  void create_unknowns(const Tagger::Config &cfg);
  void can_sentence(const Sentence &sent, State &state) const;
  const Tags &get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const;
  void add_features(const State &state, ulong i, PDF &pdf) const;
  void unpack_tags(State &state, Sentence &sent) const;
  void unpack_mtags(State &state, Sentence &sent, double BETA) const;
};

NER::Impl::Impl(NER::Config &cfg)
  : Base("NER", cfg), types(cfg.types),
    postags("POS tags", cfg.postags()),
    chunktags("chunk tags", cfg.chunktags()),
    gaz("gazetteers", cfg.path(), cfg.gazetteers()),
    unigrams("unigrams", cfg.tagdict(), klasses, lexicon),
    t_attribs(postags),
    pt_attribs(postags), ppt_attribs(postags),
    nt_attribs(postags), nnt_attribs(postags),
    c_attribs(chunktags),
    pc_attribs(chunktags), ppc_attribs(chunktags),
    nc_attribs(chunktags), nnc_attribs(chunktags),
    last_attribs(klasses),
    nu_attribs(klasses), nnu_attribs(klasses),
    wt_attribs(lexicon){
  create_model(cfg);
}

NER::Impl::~Impl(void){}

void
NER::Impl::add_prefixes(const RawWord &raw, PDF &dist) const {
  RawWord::const_iterator i = raw.begin();

  Affix affix(*i);
  if(_add_attribute(a_attribs(Types::pref, affix), dist) && ++i != raw.end())
    if(_add_attribute(a_attribs(Types::pref, affix += *i), dist) && ++i != raw.end())
      if(_add_attribute(a_attribs(Types::pref, affix += *i), dist) && ++i != raw.end())
        _add_attribute(a_attribs(Types::pref, affix += *i), dist);
}

void
NER::Impl::add_suffixes(const RawWord &raw, PDF &dist) const {
  RawWord::const_reverse_iterator i = raw.rbegin();

  Affix affix(*i);
  if(_add_attribute(a_attribs(Types::suff, affix), dist) && ++i != raw.rend())
    if(_add_attribute(a_attribs(Types::suff, affix += *i), dist) && ++i != raw.rend())
      if(_add_attribute(a_attribs(Types::suff, affix += *i), dist) && ++i != raw.rend())
        _add_attribute(a_attribs(Types::suff, affix += *i), dist);
}

void
NER::Impl::add_special(const RawWord &raw, PDF &dist) const {
  Analyser a(raw);

  if(types.use_length()){
    if(a.len < 16)
      _add_attribute(a_attribs(Types::length, numbers[a.len]), dist);
    else
      _add_attribute(a_attribs(Types::length, more), dist);
  }

  if(a.nhyphens && types.use_has_hyphen())
    _add_attribute(a_attribs(Types::has_hyphen, empty), dist);

  if(a.nperiods && types.use_has_period())
    _add_attribute(a_attribs(Types::has_period, empty), dist);

  if(a.npunct && types.use_has_punct())
    _add_attribute(a_attribs(Types::has_punct, empty), dist);

  if(a.ndigits){
    if(types.use_has_digit())
      _add_attribute(a_attribs(Types::has_digit, empty), dist);

    if(a.is_digits()){
      switch(a.len){
        case 1: if(types.use_one_digit())
                  _add_attribute(a_attribs(Types::digits, numbers[1]), dist);
                  break;
        case 2: if(types.use_two_digits())
                  _add_attribute(a_attribs(Types::digits, numbers[2]), dist);
                  break;
        case 3: if(types.use_three_digits())
                  _add_attribute(a_attribs(Types::digits, numbers[3]), dist);
                  break;
        case 4: if(types.use_four_digits())
                  _add_attribute(a_attribs(Types::digits, numbers[4]), dist);
                  break;
        default: if(types.use_digits())
                  _add_attribute(a_attribs(Types::digits, more), dist);
                  break;
      }
      if(types.use_number())
        _add_attribute(a_attribs(Types::number, empty), dist);
      return;
    }

    if(a.is_number()){
      if(types.use_number())
        _add_attribute(a_attribs(Types::number, empty), dist);
      return;
    }

    if(a.is_alphanum()){
      if(types.use_alphanum())
        _add_attribute(a_attribs(Types::alphanum, empty), dist);
      return;
    }
  }

  if(a.is_roman() && types.use_roman())
    _add_attribute(a_attribs(Types::roman, empty), dist);

  if(a.nupper){
    if(types.use_has_uppercase())
      _add_attribute(a_attribs(Types::has_uppercase, empty), dist);

    if(types.use_case()){
      if(a.fupper){
        if(a.uc())
          _add_attribute(a_attribs(Types::kase, uc), dist);
        else if(a.tc())
          _add_attribute(a_attribs(Types::kase, tc), dist);
        else if(a.mc())
          _add_attribute(a_attribs(Types::kase, mc), dist);
      }
    }

    if(a.is_initial()){
      if(types.use_initial())
        _add_attribute(a_attribs(Types::initial, empty), dist);
    }else if(a.is_acronym()){
      if(types.use_acronym())
        _add_attribute(a_attribs(Types::acronym, empty), dist);
    }
  }else if(a.lc() && types.use_case())
    _add_attribute(a_attribs(Types::kase, lc), dist);
}

void
NER::Impl::add_surrounding_pos(const OffsetTags &tags, ulong i, PDF &dist) const {
  if(!types.use_pos())
    return;

  _add_attribute(pt_attribs(tags[i - 1]), dist);
  _add_attribute(ppt_attribs(tags[i - 2]), dist);
  _add_attribute(nt_attribs(tags[i + 1]), dist);
  _add_attribute(nnt_attribs(tags[i + 2]), dist);
}

void
NER::Impl::add_surrounding_chunks(const OffsetTags &chunks, ulong i, PDF &dist) const {
  if(!types.use_chunks())
    return;

  _add_attribute(pc_attribs(chunks[i - 1]), dist);
  _add_attribute(ppc_attribs(chunks[i - 2]), dist);
  _add_attribute(nc_attribs(chunks[i + 1]), dist);
  _add_attribute(nnc_attribs(chunks[i + 2]), dist);
}


void
NER::Impl::add_last(const State &state, const RawWord &raw, PDF &dist) const {
  Tag tag = state.last_klass[raw];
  if(tag.value())
    _add_attribute(last_attribs(tag), dist);
}

void
NER::Impl::add_gazetteers(const std::string &word, ulong i, PDF &dist) const {
  ulong flags = gaz.lower(word);
  if(flags & Gazetteers::COMMON){
    if(types.use_gaz_common())
      _add_attribute(gc_attribs(), dist);

    if(types.use_composites()){
        if(i == 0)
          _add_attribute(xbs_attribs(), dist);
        else
          _add_attribute(xms_attribs(), dist);
    }
  }

  flags = gaz[word];
  if(types.use_gaz_first() && (flags & Gazetteers::FIRST))
    _add_attribute(gf_attribs(), dist);

  if(types.use_gaz_last() && (flags & Gazetteers::LAST))
    _add_attribute(gs_attribs(), dist);
}

void
NER::Impl::add_prev_gazetteers(const std::string &word, PDF &dist) const {
  ulong flags = gaz.lower(word);
  if(types.use_gaz_common() && (flags & Gazetteers::COMMON))
    _add_attribute(gpc_attribs(), dist);

  flags = gaz[word];
  if(types.use_gaz_first() && (flags & Gazetteers::FIRST))
    _add_attribute(gpf_attribs(), dist);
  if(types.use_gaz_last() && (flags & Gazetteers::LAST))
    _add_attribute(gps_attribs(), dist);
}

void
NER::Impl::add_next_gazetteers(const std::string &word, PDF &dist) const {
  ulong flags = gaz.lower(word);
  if(types.use_gaz_common() && (flags & Gazetteers::COMMON))
    _add_attribute(gnc_attribs(), dist);

  flags = gaz[word];
  if(types.use_gaz_first() && (flags & Gazetteers::FIRST))
    _add_attribute(gnf_attribs(), dist);
  if(types.use_gaz_last() && (flags & Gazetteers::LAST))
    _add_attribute(gns_attribs(), dist);
}

void
NER::Impl::add_unigrams(const OffsetWords &words, ulong i, PDF &dist) const {
  if(types.use_nu() && words[i + 1] && words[i + 1] != NLP::SENTINEL){
    Tag nut = unigrams[words[i + 1]];
    if(nut.value())
      _add_attribute(nu_attribs(nut), dist);
  }

  if(types.use_nnu() && words[i + 2] && words[i + 2] != NLP::SENTINEL){
    Tag nnut = unigrams[words[i + 2]];
    if(nnut.value())
      _add_attribute(nnu_attribs(nnut), dist);
  }
}

void
NER::Impl::add_wordtypes(const RawWords &sent, ulong i, PDF &dist) const {
  if(!types.use_wordtypes())
    return;

  WordType WT;

  Word value = lexicon[WT(sent[i])];
  if(value)
    _add_attribute(wt_attribs(Types::wt, value), dist);


  if(i > 0){
    if((value = lexicon[WT(sent[i - 1])])){
      _add_attribute(wt_attribs(Types::pwt, value), dist);
      if(types.use_bitypes() && (value = lexicon[WT(sent[i - 1], sent[i])]))
        _add_attribute(wt_attribs(Types::pbt, value), dist);
    }
    if(i > 1){
      if((value = lexicon[WT(sent[i - 2])])){
        _add_attribute(wt_attribs(Types::ppwt, value), dist);
        if(types.use_bitypes() && (value = lexicon[WT(sent[i - 2], sent[i - 1])]))
          _add_attribute(wt_attribs(Types::ppbt, value), dist);
        if(types.use_tritypes() && i > 2 &&
            (value = lexicon[WT(sent[i - 3], sent[i - 2], sent[i - 1])]))
          _add_attribute(wt_attribs(Types::ptt, value), dist);
      }
    }
  }

  size_t last = sent.size() - 1;
  if(i < last){
    if((value = lexicon[WT(sent[i + 1])]))
      _add_attribute(wt_attribs(Types::nwt, value), dist);
    if(types.use_bitypes()){
      if((value = lexicon[WT(sent[i], sent[i + 1])])){
        _add_attribute(wt_attribs(Types::nbt, value), dist);
        if(i > 0 && (value = lexicon[WT(sent[i - 1], sent[i + 1])]))
          _add_attribute(wt_attribs(Types::bt, value), dist);
      }
    }

    if(i + 1 < last){
      if((value = lexicon[WT(sent[i + 2])])){
        _add_attribute(wt_attribs(Types::nnwt, value), dist);
        if(types.use_bitypes() && (value = lexicon[WT(sent[i + 1], sent[i + 2])]))
          _add_attribute(wt_attribs(Types::nnbt, value), dist);
      if(types.use_tritypes() && i + 2 < last &&
          (value = lexicon[WT(sent[i + 1], sent[i + 2], sent[i + 3])]))
        _add_attribute(wt_attribs(Types::ntt, value), dist);
      }
    }
  }
}

void
NER::Impl::reg_attributes(void){
  Base::reg_attributes();

  // tag types
  registry.reg(Types::t, t_attribs);
  registry.reg(Types::pt, pt_attribs);
  registry.reg(Types::ppt, ppt_attribs);
  registry.reg(Types::nt, nt_attribs);
  registry.reg(Types::nnt, nnt_attribs);

  // chunk types
  registry.reg(Types::c, c_attribs);
  registry.reg(Types::pc, pc_attribs);
  registry.reg(Types::ppc, ppc_attribs);
  registry.reg(Types::nc, nc_attribs);
  registry.reg(Types::nnc, nnc_attribs);

  // NE types
  registry.reg(Types::pne, pk_attribs);
  registry.reg(Types::ppne, ppkpk_attribs);

  // prefix and suffix types
  registry.reg(Types::suff, a_attribs);
  registry.reg(Types::pref, a_attribs);

  // orthographic types
  registry.reg(Types::has_digit, dig_attribs);
  registry.reg(Types::has_hyphen, hyph_attribs);
  registry.reg(Types::has_period, per_attribs);
  registry.reg(Types::has_punct, pun_attribs);
  registry.reg(Types::has_uppercase, uc_attribs);

  registry.reg(Types::alphanum, an_attribs);
  registry.reg(Types::number, num_attribs);
  registry.reg(Types::roman, rom_attribs);
  registry.reg(Types::initial, ini_attribs);
  registry.reg(Types::acronym, acr_attribs);

  registry.reg(Types::kase, a_attribs);
  registry.reg(Types::digits, a_attribs);
  registry.reg(Types::length, a_attribs);

  // gazetteer types
  registry.reg(Types::gaz_common, gc_attribs);
  registry.reg(Types::gaz_first, gf_attribs);
  registry.reg(Types::gaz_last, gs_attribs);
  
  registry.reg(Types::pgaz_common, gpc_attribs);
  registry.reg(Types::pgaz_first, gpf_attribs);
  registry.reg(Types::pgaz_last, gps_attribs);

  registry.reg(Types::ngaz_common, gnc_attribs);
  registry.reg(Types::ngaz_first, gnf_attribs);
  registry.reg(Types::ngaz_last, gns_attribs);

  // gazetteer and sentence position types
  registry.reg(Types::xcommon_bs, xbs_attribs);
  registry.reg(Types::xcommon_ms, xms_attribs);

  // wordtype types
  registry.reg(Types::wt, wt_attribs);   // sc (12/04/05) changed from a
  registry.reg(Types::pwt, wt_attribs);  // was pa
  registry.reg(Types::ppwt, wt_attribs); // was ppa 
  registry.reg(Types::nwt, wt_attribs);  // was na
  registry.reg(Types::nnwt, wt_attribs); // was nna

  // bigram wordtype types
  registry.reg(Types::ppbt, wt_attribs);   // was ppapa sc: how do these correspond?
  registry.reg(Types::pbt, wt_attribs);    // was paa
  registry.reg(Types::bt, wt_attribs);     // was pana
  registry.reg(Types::nbt, wt_attribs);    // was ana
  registry.reg(Types::nnbt, wt_attribs);   // was nanna

  // trigram wordtype types
  registry.reg(Types::ptt, wt_attribs);  // was pppappapa
  registry.reg(Types::ntt, wt_attribs);  // was nannannna

  // miscellaneous special types
  registry.reg(Types::last, last_attribs);
  registry.reg(Types::nu, nu_attribs);
  registry.reg(Types::nnu, nnu_attribs);
}

void
NER::Impl::create_unknowns(const Tagger::Config &){
  unknown_klasses.reserve(klasses.size() - 2);
  for(ulong i = 2; i < klasses.size(); ++i)
    unknown_klasses.push_back(Tag(i));
}

void
NER::Impl::can_sentence(const Sentence &sent, State &state) const {
  if(sent.words.size() > maxwords)
    throw NLP::Exception("sentence length exceeds maximum number of words for NE tagger");

  state.raws = sent.words;
  lexicon.can(state.raws, state.words);
  if(types.use_pos()){
    if(sent.words.size() != sent.pos.size())
      throw NLP::Exception("the number of words and POS tags in a sentence must be the same");
    postags.tag(sent.pos, state.pos);
  }
  if(types.use_chunks()){
    if(sent.words.size() != sent.chunks.size())
      throw NLP::Exception("the number of words and chunk tags in a sentence must be the same");
    chunktags.tag(sent.chunks, state.chunks);
  }
}

const Tags &
NER::Impl::get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const {
  if(state.words[i].freq() >= DICT_CUTOFF){
    Tags &tags = tagdict[state.words[i]];
    if(tags.size())
      return tags;
  }

  return unknown_klasses;
}

void
NER::Impl::add_features(const State &state, ulong i, PDF &pdf) const {
  const RawWord &raw = state.raws[i];
  const Word word = state.words[i];

  if(word.freq() < rare_cutoff){
    add_prefixes(raw, pdf);
    add_suffixes(raw, pdf);
    add_special(raw, pdf);
  }

  if(types.use_words()){
    _add_attribute(w_attribs(Types::w, word), pdf);
    add_surrounding_words(state.words, i, pdf);
  }

  if(types.use_pos()){
    _add_attribute(t_attribs(state.pos[i]), pdf);
    add_surrounding_pos(state.pos, i, pdf);
  }

  if(types.use_chunks()){
    _add_attribute(c_attribs(state.chunks[i]), pdf);
    add_surrounding_chunks(state.chunks, i, pdf);
  }

  if(types.use_last())
    add_last(state, raw, pdf);

  add_unigrams(state.words, i, pdf);

  add_wordtypes(state.raws, i, pdf);

  if(types.use_gazetteers())
    add_gazetteers(raw, i, pdf);
  if(types.use_prev_gazetteers() && i > 0)
    add_prev_gazetteers(state.raws[i - 1], pdf);
  if(types.use_next_gazetteers() && (long)i < state.words.size() - 1)
    add_next_gazetteers(state.raws[i + 1], pdf);
}

void
NER::Impl::unpack_tags(State &state, Sentence &sent) const {
  _unpack_tags(state, sent.entities, types.use_last());
}

void
NER::Impl::unpack_mtags(State &state, Sentence &sent, const double BETA) const {
  _unpack_mtags(state, sent.mentities, BETA, types.use_last());
}

NER::NER(NER::Config &cfg): Tagger(cfg, new Impl(cfg)){}
NER::NER(NER &other): Tagger(other){}
NER::~NER(void){}

TagSet
NER::postags(void) const {
  return dynamic_cast<const NER::Impl *>(impl_)->postags;
}

TagSet
NER::chunktags(void) const {
  return dynamic_cast<const NER::Impl *>(impl_)->chunktags;
}

} }
