// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Extract::Super
// extracts features from supertagged training data
// saves the extracted model into the specified model directory
// which is then loaded by NLP::MaxEnt::GIS for estimating
// the parameters of the model, and NLP::Taggers::Super for tagging

#include "extract/_baseimpl.h"
#include "extract/tagger.h"

#include "tagger/tagsetdict.h"
#include "tagger/super.h"
#include "extract/super.h"

using namespace std;

namespace NLP { namespace Extract {

// private implementation, which is shared
class Super::_Impl: public _TaggerImpl {
protected:
  void reg_types(void);
  
  template <class TC>
  void _add_surrounding_words(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding_tags(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding2tags(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding3tags(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding2words(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding3words(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_history(TC &tc, const Sentence &sent, ulong i);

  template <class TC>
  void _generate(TC &tc, const Sentence &sent, ulong i);

  void _generate_class_counts(const Sentence &sent);
  void _generate_counts(const Sentence &sent);
  void _generate_features(const Sentence &sent);
  void _generate_contexts(const Sentence &sent);
  void _make_unknowns(void) const;

  void _pass0(NLP::IO::Reader &reader);
  void _pass1(NLP::IO::Reader &reader, bool save_klasses = true);
public:
  NLP::Taggers::Super::Config &cfg;

  Lexicon poscounts;  // POS tags and frequency counts
  Lexicon posdict;  // postag dictionary

  _Impl(NLP::Taggers::Super::Config &cfg, const std::string &PREFACE, bool VERBOSE);
  ~_Impl(void);

  virtual void extract(NLP::IO::Reader &reader, bool MKDIR);
};

void
Super::_Impl::reg_types(void){
  registry.reg(Types::pppt);
  registry.reg(Types::ppt);
  registry.reg(Types::pt);
  registry.reg(Types::t);
  registry.reg(Types::nt);
  registry.reg(Types::nnt);
  registry.reg(Types::nnnt);

  registry.reg(Types::pppt_ppt_b);
  registry.reg(Types::ppt_pt_b);
  registry.reg(Types::pt_t_b);
  registry.reg(Types::pt_nt_b);
  registry.reg(Types::t_nt_b);
  registry.reg(Types::nt_nnt_b);
  registry.reg(Types::nnt_nnnt_b);
  registry.reg(Types::nnnt_nnnnt_b);

  registry.reg(Types::pppw_ppw_b);
  registry.reg(Types::ppw_pw_b);
  registry.reg(Types::pw_w_b);
  registry.reg(Types::pw_nw_b);
  registry.reg(Types::w_nw_b);
  registry.reg(Types::nw_nnw_b);
  registry.reg(Types::nnw_nnnw_b);

  registry.reg(Types::pppt_ppt_pt_c);
  registry.reg(Types::ppt_pt_t_c);
  registry.reg(Types::pt_t_nt_c);
  registry.reg(Types::t_nt_nnt_c);
  registry.reg(Types::nt_nnt_nnnt_c);
  registry.reg(Types::nnt_nnnt_nnnnt_c);

  registry.reg(Types::pppw_ppw_pw_c);
  registry.reg(Types::ppw_pw_w_c);
  registry.reg(Types::pw_w_nw_c);
  registry.reg(Types::w_nw_nnw_c);
  registry.reg(Types::nw_nnw_nnnw_c);
  
  registry.reg(Types::pst);
  registry.reg(Types::ppst);
}

// count surrounding word features (3 words to left and right)
template <class TC>
void
Super::_Impl::_add_surrounding_words(TC &tc, const Sentence &sent, ulong i){
  if(i > 0){
    attributes(tc, Types::pw, sent.words[i - 1]);
    if(i > 1){
      attributes(tc, Types::ppw, sent.words[i - 2]);
      if(cfg.surr3words()){
        if(i > 2)
          attributes(tc, Types::pppw, sent.words[i - 3]);
        else
          attributes(tc, Types::pppw, SENTINEL);
      }
    }else{
      attributes(tc, Types::ppw, SENTINEL);
      if(cfg.surr3words()) 
        attributes(tc, Types::pppw, SENTINEL);
    }
  }else{
    attributes(tc, Types::pw, SENTINEL);
    attributes(tc, Types::ppw, SENTINEL);
    if(cfg.surr3words()) 
      attributes(tc, Types::pppw, SENTINEL);
  }

  size_t last = sent.words.size() - 1;
  if(i < last){
    attributes(tc, Types::nw, sent.words[i + 1]);
    --last;
    if(i < last){
      attributes(tc, Types::nnw, sent.words[i + 2]);
      --last;
      if(cfg.surr3words()){
        if(i < last)
          attributes(tc, Types::nnnw, sent.words[i + 3]);
        else
          attributes(tc, Types::nnnw, SENTINEL);
      }
    }else{
      attributes(tc, Types::nnw, SENTINEL);
      if(cfg.surr3words()) 
        attributes(tc, Types::nnnw, SENTINEL);
    }
  }else{
    attributes(tc, Types::nw, SENTINEL);
    attributes(tc, Types::nnw, SENTINEL);
    if(cfg.surr3words()) 
      attributes(tc, Types::nnnw, SENTINEL);
  }
}

// count surrounding POS tag features (3 tags to left and right)
template <class TC>
void
Super::_Impl::_add_surrounding_tags(TC &tc, const Sentence &sent, ulong i){
  if(i > 0){
    attributes(tc, Types::pt, sent.pos[i - 1]);
    if(i > 1){
      attributes(tc, Types::ppt, sent.pos[i - 2]);
      if(cfg.surr3tags()){
        if(i > 2)
          attributes(tc, Types::pppt, sent.pos[i - 3]);
        else
          attributes(tc, Types::pppt, SENTINEL);
      }
    }else{
      attributes(tc, Types::ppt, SENTINEL);
      if(cfg.surr3tags()) 
        attributes(tc, Types::pppt, SENTINEL);
    }
  }else{
    attributes(tc, Types::pt, SENTINEL);
    attributes(tc, Types::ppt, SENTINEL);
    if(cfg.surr3tags()) 
      attributes(tc, Types::pppt, SENTINEL);
  }

  size_t last = sent.words.size() - 1;
  if(i < last){
    attributes(tc, Types::nt, sent.pos[i + 1]);
    --last;
    if(i < last){
      attributes(tc, Types::nnt, sent.pos[i + 2]);
      --last;
      if(cfg.surr3tags()){
        if(i < last)
          attributes(tc, Types::nnnt, sent.pos[i + 3]);
        else
          attributes(tc, Types::nnnt, SENTINEL);
      }
    }else{
      attributes(tc, Types::nnt, SENTINEL);
      if(cfg.surr3tags()) 
        attributes(tc, Types::nnnt, SENTINEL);
    }
  }else{
    attributes(tc, Types::nt, SENTINEL);
    attributes(tc, Types::nnt, SENTINEL);
    if(cfg.surr3tags()) 
      attributes(tc, Types::nnnt, SENTINEL);
  }
}

// count surrounding2 word features (2 words to left and right)
template <class TC>
void
Super::_Impl::_add_surrounding2words(TC &tc, const Sentence &sent, ulong i){
  size_t last = sent.words.size() - 1;
  if(i > 0){
    attributes(tc, Types::pw_w_b, sent.words[i - 1], sent.words[i]);
    if(i > 1){
      attributes(tc, Types::ppw_pw_b, sent.words[i - 2], sent.words[i - 1]);
      if(cfg.biwords_far()){
        if(i > 2)
          attributes(tc, Types::pppw_ppw_b, sent.words[i - 3], sent.words[i - 2]);
        else
          attributes(tc, Types::pppw_ppw_b, SENTINEL, sent.words[i - 2]);
      }
    }else{
      attributes(tc, Types::ppw_pw_b, SENTINEL, sent.words[i - 1]);
      if(cfg.biwords_far()) 
        attributes(tc, Types::pppw_ppw_b, SENTINEL2);
    }
    
    if(i < last)
      attributes(tc, Types::pw_nw_b, sent.words[i - 1], sent.words[i + 1]);
    else
      attributes(tc, Types::pw_nw_b, sent.words[i - 1], SENTINEL);
  }else{
    attributes(tc, Types::pw_w_b, SENTINEL, sent.words[i]);
    attributes(tc, Types::ppw_pw_b, SENTINEL2);
    if(cfg.biwords_far()) 
      attributes(tc, Types::pppw_ppw_b, SENTINEL2);

    if(i < last)
      attributes(tc, Types::pw_nw_b, SENTINEL, sent.words[i + 1]);
    else
      attributes(tc, Types::pw_nw_b, SENTINEL2);
  }

  if(i < last){
    attributes(tc, Types::w_nw_b, sent.words[i], sent.words[i + 1]);
    --last;
    if(i < last){
      attributes(tc, Types::nw_nnw_b, sent.words[i + 1], sent.words[i + 2]);
      --last;
      if(cfg.biwords_far()){
        if(i < last)
          attributes(tc, Types::nnw_nnnw_b, sent.words[i + 2], sent.words[i + 3]);
        else
          attributes(tc, Types::nnw_nnnw_b, sent.words[i + 2], SENTINEL);
      }
    }else{
      attributes(tc, Types::nw_nnw_b, sent.words[i + 1], SENTINEL);
      if(cfg.biwords_far()) 
        attributes(tc, Types::nnw_nnnw_b, SENTINEL2);
    }
  }else{
    attributes(tc, Types::w_nw_b, sent.words[i], SENTINEL);
    attributes(tc, Types::nw_nnw_b, SENTINEL2);
    if(cfg.biwords_far()) 
      attributes(tc, Types::nnw_nnnw_b, SENTINEL2);
  }
}

// count surrounding2 POS tag features (2 tags to left and right)
template <class TC>
void
Super::_Impl::_add_surrounding2tags(TC &tc, const Sentence &sent, ulong i){
  size_t last = sent.pos.size() - 1;
  if(i > 0){
    attributes(tc, Types::pt_t_b, sent.pos[i - 1], sent.pos[i]);
    if(i > 1){
      attributes(tc, Types::ppt_pt_b, sent.pos[i - 2], sent.pos[i - 1]);
      if(cfg.bitags_far()){
        if(i > 2)
          attributes(tc, Types::pppt_ppt_b, sent.pos[i - 3], sent.pos[i - 2]);
        else
          attributes(tc, Types::pppt_ppt_b, SENTINEL, sent.pos[i - 2]);
      }
    }else{
      attributes(tc, Types::ppt_pt_b, SENTINEL, sent.pos[i - 1]);
      if(cfg.bitags_far()) 
        attributes(tc, Types::pppt_ppt_b, SENTINEL2);
    }
    if(i < last)
      attributes(tc, Types::pt_nt_b, sent.pos[i - 1], sent.pos[i + 1]);
    else
      attributes(tc, Types::pt_nt_b, sent.pos[i - 1], SENTINEL);
  }else{
    attributes(tc, Types::pt_t_b, SENTINEL, sent.pos[i]);
    attributes(tc, Types::ppt_pt_b, SENTINEL2);
    if(cfg.bitags_far()) 
      attributes(tc, Types::pppt_ppt_b, SENTINEL2);

    if(i < last)
      attributes(tc, Types::pt_nt_b, SENTINEL, sent.pos[i + 1]);
    else
      attributes(tc, Types::pt_nt_b, SENTINEL2);
  }

  if(i < last){
    attributes(tc, Types::t_nt_b, sent.pos[i], sent.pos[i + 1]);
    --last;
    if(i < last){
      attributes(tc, Types::nt_nnt_b, sent.pos[i + 1], sent.pos[i + 2]);
      --last;
      if(cfg.bitags_far()){
        if(i < last){
          attributes(tc, Types::nnt_nnnt_b, sent.pos[i + 2], sent.pos[i + 3]);
          --last;
          if(i < last)
            attributes(tc, Types::nnnt_nnnnt_b, sent.pos[i + 3], sent.pos[i + 4]);
          else
            attributes(tc, Types::nnnt_nnnnt_b, sent.pos[i + 3], SENTINEL);
        }
        else{
          attributes(tc, Types::nnt_nnnt_b, sent.pos[i + 2], SENTINEL);
          attributes(tc, Types::nnnt_nnnnt_b, SENTINEL2);
        }
      }
    }else{
      attributes(tc, Types::nt_nnt_b, sent.pos[i + 1], SENTINEL);
      if(cfg.bitags_far()){ 
        attributes(tc, Types::nnt_nnnt_b, SENTINEL2);
        attributes(tc, Types::nnnt_nnnnt_b, SENTINEL2);
      }
    }
  }else{
    attributes(tc, Types::t_nt_b, sent.pos[i], SENTINEL);
    attributes(tc, Types::nt_nnt_b, SENTINEL2);
    if(cfg.bitags_far()){
      attributes(tc, Types::nnt_nnnt_b, SENTINEL2);
      attributes(tc, Types::nnnt_nnnnt_b, SENTINEL2);
    }
  }
}

// count surrounding3 word features (3 words to left and right)
template <class TC>
void
Super::_Impl::_add_surrounding3words(TC &tc, const Sentence &sent, ulong i){
  size_t last = sent.words.size() - 1;
  if(i > 0){
    if(i > 1){
      attributes(tc, Types::ppw_pw_w_c, sent.words[i - 2], sent.words[i - 1], sent.words[i]);
      if(cfg.triwords_far()){
        if(i > 2)
          attributes(tc, Types::pppw_ppw_pw_c, sent.words[i - 3], sent.words[i - 2], sent.words[i - 1]);
        else
          attributes(tc, Types::pppw_ppw_pw_c, SENTINEL, sent.words[i - 2], sent.words[i - 1]);
      }
    }else{
      if(cfg.triwords_far()) 
        attributes(tc, Types::pppw_ppw_pw_c, SENTINEL2, sent.words[i - 1]);
      attributes(tc, Types::ppw_pw_w_c, SENTINEL, sent.words[i - 1], sent.words[i]);
    }
    if(i < last)
      attributes(tc, Types::pw_w_nw_c, sent.words[i - 1], sent.words[i], sent.words[i + 1]);
    else
      attributes(tc, Types::pw_w_nw_c, sent.words[i - 1], sent.words[i], SENTINEL);
  }
  else{
    if(cfg.triwords_far()) 
      attributes(tc, Types::pppw_ppw_pw_c, SENTINEL3);
    attributes(tc, Types::ppw_pw_w_c, SENTINEL2, sent.words[i]);
    if(i < last)
      attributes(tc, Types::pw_w_nw_c, SENTINEL, sent.words[i], sent.words[i + 1]);
    else
      attributes(tc, Types::pw_w_nw_c, SENTINEL, sent.words[i], SENTINEL);
  }

  if(i < last){
    --last;
    if(i < last){
      attributes(tc, Types::w_nw_nnw_c, sent.words[i], sent.words[i + 1], sent.words[i + 2]);
      --last;
      if(cfg.triwords_far()){
        if(i < last)
          attributes(tc, Types::nw_nnw_nnnw_c, sent.words[i + 1], sent.words[i + 2], sent.words[i + 3]);
        else
          attributes(tc, Types::nw_nnw_nnnw_c, sent.words[i + 1], sent.words[i + 2], SENTINEL);
      }
    }else{
      if(cfg.triwords_far()) 
        attributes(tc, Types::nw_nnw_nnnw_c, sent.words[i + 1], SENTINEL2);
      attributes(tc, Types::w_nw_nnw_c, sent.words[i], sent.words[i + 1], SENTINEL);
    }
  }else{
    if(cfg.triwords_far()) attributes(tc, Types::nw_nnw_nnnw_c, SENTINEL3);
    attributes(tc, Types::w_nw_nnw_c, sent.words[i], SENTINEL2);
  }
}

// count surrounding3 POS tag features (3 tags to left and right)
template <class TC>
void
Super::_Impl::_add_surrounding3tags(TC &tc, const Sentence &sent, ulong i){
  size_t last = sent.pos.size() - 1;
  if(i > 0){
    if(i > 1){
      attributes(tc, Types::ppt_pt_t_c, sent.pos[i - 2], sent.pos[i - 1], sent.pos[i]);  
      if(cfg.tritags_far()){
        if(i > 2)
          attributes(tc, Types::pppt_ppt_pt_c, sent.pos[i - 3], sent.pos[i - 2], sent.pos[i - 1]);
        else
          attributes(tc, Types::pppt_ppt_pt_c, SENTINEL, sent.pos[i - 2], sent.pos[i - 1]);
      }
    }else{
      if(cfg.tritags_far()) 
        attributes(tc, Types::pppt_ppt_pt_c, SENTINEL2, sent.pos[i - 1]);
      attributes(tc, Types::ppt_pt_t_c, SENTINEL, sent.pos[i - 1], sent.pos[i]);
    }
    if(i < last)
      attributes(tc, Types::pt_t_nt_c, sent.pos[i - 1], sent.pos[i], sent.pos[i + 1]);
    else
      attributes(tc, Types::pt_t_nt_c, sent.pos[i - 1], sent.pos[i], SENTINEL);
  }
  else{
    if(cfg.tritags_far()) 
      attributes(tc, Types::pppt_ppt_pt_c, SENTINEL3);
    attributes(tc, Types::ppt_pt_t_c, SENTINEL2, sent.pos[i]);
    if(i < last)
      attributes(tc, Types::pt_t_nt_c, SENTINEL, sent.pos[i], sent.pos[i + 1]);
    else
      attributes(tc, Types::pt_t_nt_c, SENTINEL, sent.pos[i], SENTINEL);
  }

  if(i < last){
    --last;
    if(i < last){
      attributes(tc, Types::t_nt_nnt_c, sent.pos[i], sent.pos[i + 1], sent.pos[i + 2]);
      --last;
      if(cfg.tritags_far()){
        if(i < last){
          attributes(tc, Types::nt_nnt_nnnt_c, sent.pos[i + 1], sent.pos[i + 2], sent.pos[i + 3]);
          --last;
          if(i < last)
            attributes(tc, Types::nnt_nnnt_nnnnt_c, sent.pos[i + 2], sent.pos[i + 3], sent.pos[i + 4]);
          else
            attributes(tc, Types::nnt_nnnt_nnnnt_c, sent.pos[i + 2], sent.pos[i + 3], SENTINEL);
        }
        else{
          attributes(tc, Types::nt_nnt_nnnt_c, sent.pos[i + 1], sent.pos[i + 2], SENTINEL);
          attributes(tc, Types::nnt_nnnt_nnnnt_c, sent.pos[i + 2], SENTINEL2);
        }
      }
    }else{
      if(cfg.tritags_far()){ 
        attributes(tc, Types::nt_nnt_nnnt_c, sent.pos[i + 1], SENTINEL2);
        attributes(tc, Types::nnt_nnnt_nnnnt_c, SENTINEL3);
      }
      attributes(tc, Types::t_nt_nnt_c, sent.pos[i], sent.pos[i + 1], SENTINEL);
    }
  }else{
    if(cfg.tritags_far()){ 
      attributes(tc, Types::nt_nnt_nnnt_c, SENTINEL3);
      attributes(tc, Types::nnt_nnnt_nnnnt_c, SENTINEL3);
    }
    attributes(tc, Types::t_nt_nnt_c, sent.pos[i], SENTINEL2);
  }
}

// count previously assigned tag features
// for the previous tag and the bigram (previous tag, prev-prev tag)
template <class TC>
void
Super::_Impl::_add_history(TC &tc, const Sentence &sent, ulong i){
  static std::string tmp;
  if(i > 0){
    if(!klasses[sent.super[i - 1]])
      return;

    attributes(tc, Types::pst, sent.super[i - 1]);

    if(i > 1){
      if(!klasses[sent.super[i - 2]])
        return;

      attributes(tc, Types::ppst, sent.super[i - 2], sent.super[i - 1]);
    }else
      attributes(tc, Types::ppst, SENTINEL, sent.super[i - 1]);
  }else{
    attributes(tc, Types::pst, SENTINEL);
    attributes(tc, Types::ppst, SENTINEL2);
  }
}

// count the number of tags, words and word/tag pairs in the given sentence
void
Super::_Impl::_generate_counts(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    if(counts[sent.super[i]]){
      lexicon.add(sent.words[i], 1);
      poscounts.add(sent.pos[i], 1);
      tagdict.add(sent.words[i] + ' ' + sent.super[i], 1);
      posdict.add(sent.pos[i] + ' ' + sent.super[i], 1);
    }else
      lexicon.add(sent.words[i], 0);
  }
  nevents += sent.words.size();
}

template <class TC>
void
Super::_Impl::_generate(TC &tc, const Sentence &sent, ulong i){
  // rare word features only switch on for infrequent words
  // with frequency less than config.rare
  attributes(tc, Types::w, sent.words[i]);
  attributes(tc, Types::t, sent.pos[i]);

  _add_surrounding_words(tc, sent, i);
  _add_surrounding_tags(tc, sent, i);
  if(cfg.biwords() || cfg.biwords_far())
    _add_surrounding2words(tc, sent, i);
  if(cfg.bitags() || cfg.bitags_far())
    _add_surrounding2tags(tc, sent, i);
  if(cfg.triwords() || cfg.triwords_far())
    _add_surrounding3words(tc, sent, i);
  if(cfg.tritags() || cfg.tritags_far())
    _add_surrounding3tags(tc, sent, i);
  _add_history(tc, sent, i);
}

// count the features for each training instance in the given sentence
void
Super::_Impl::_generate_features(const Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    Tag klass = klasses[sent.super[i]];
    if(!klass)
      continue;

    _generate(klass, sent, i);
  }
  nevents += sent.words.size();
}

// add the features for each training instance to the context
// and dump it model/contexts for the given sentence
void
Super::_Impl::_generate_contexts(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    context.resize(0);

    Tag klass = klasses[sent.super[i]];
    if(!klass)
      continue;

    _generate(context, sent, i);
    
    if(context.size() > 0){
      std::sort(context.begin(), context.end());
      contexts.add(klass, context);
    }
  }
  nevents += sent.words.size();
}

void
Super::_Impl::_make_unknowns(void) const {
  if(Cluster::rank != 0)
    return;

  ofstream stream(cfg.unknowns().c_str());
  if(!stream)
    throw NLP::IOException("could not open unknown_tags file for writing", cfg.unknowns());

  stream << PREFACE << '\n';
}

void
Super::_Impl::_generate_class_counts(const Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i)
    counts.add(sent.super[i], 1);
  nevents += sent.words.size();
}

// extract supertags and their frequency, and apply a cutoff so that
// we only keep supertags which appear at least MIN_CLASS_FREQ times
// in the training file.  This is dumped out to model/classes
void
Super::_Impl::_pass0(NLP::IO::Reader &reader){
  _apply(reader, "pass 0", this, &Super::_Impl::_generate_class_counts);

  // apply the cutoff, sort alphabetically and dump
  counts.merge();
  counts.bcast();
  counts.apply_cutoff(cfg.category_cutoff());
  if(Cluster::rank == 0){
    counts.sort_by_alpha();
    counts.save(cfg.model.klasses(), PREFACE);
  }
}

void
Super::_Impl::_pass1(NLP::IO::Reader &reader, bool){
  _TaggerImpl::_pass1(reader, false);

  // POS tags are sorted alphabetically and dumped
  poscounts.merge();
  if(Cluster::rank == 0){
    poscounts.sort_by_alpha();
    poscounts.save(cfg.postags(), PREFACE);
  }

  // postag dictionary is sorted alphabetically on postag-tag pairs and dumped
  posdict.merge();
  if(Cluster::rank == 0){
    posdict.sort_by_alpha();
    posdict.save(cfg.posdict(), PREFACE);
  }
}

// run the three passes and then save the information about the size
// of the model (number of classes, attributes, features etc)
void
Super::_Impl::extract(NLP::IO::Reader &reader, bool MKDIR){
  if(MKDIR && Cluster::rank == 0)
    Port::make_directory(cfg.path());
 
  Cluster::barrier();

  _pass0(reader);
  _TaggerImpl::extract(reader, false);
}

Super::_Impl::_Impl(NLP::Taggers::Super::Config &cfg, const std::string &PREFACE, bool VERBOSE)
  : _TaggerImpl(cfg, PREFACE, VERBOSE), cfg(cfg),
    poscounts("poscount"), posdict("posdict"){
  reg_types();
}

Super::_Impl::~_Impl(void) {}

Super::Super(NLP::Taggers::Super::Config &cfg, const std::string &PREFACE, bool VERBOSE)
  : _impl(new _Impl(cfg, PREFACE, VERBOSE)){}

Super::Super(const Super &other): _impl(share(other._impl)){}

Super::~Super(void){
  release(_impl);
}

ulong Super::nevents(void) const { return _impl->nevents; }
ulong Super::ncontexts(void) const { return _impl->ncontexts; }

TagSet Super::tagset(void) const { return _impl->klasses; }
Lexicon Super::lexicon(void) const { return _impl->lexicon; }

void Super::extract(NLP::IO::Reader &reader){
  _impl->extract(reader, true);
}

} }
