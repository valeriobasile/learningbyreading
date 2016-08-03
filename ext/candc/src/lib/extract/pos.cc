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

#include "extract/_baseimpl.h"
#include "extract/tagger.h"

#include "tagger/pos.h"
#include "extract/pos.h"

using namespace std;

namespace NLP { namespace Extract {

// private implementation, which is shared
class POS::_Impl: public _TaggerImpl {
protected:
  void reg_types(void);
  
  template <class TC>
  void _add_prefixes(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_suffixes(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_special(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_history(TC &tc, const Sentence &sent, ulong i);

  template <class TC>
  void _generate(TC &tc, const Sentence &sent, ulong i);

  void _generate_counts(const NLP::Sentence &sent);
  void _generate_features(const NLP::Sentence &sent);
  void _generate_contexts(const NLP::Sentence &sent);
  void _make_unknowns(void) const;
public:
  NLP::Taggers::POS::Config &cfg;

  _Impl(NLP::Taggers::POS::Config &cfg, const std::string &PREFACE, bool VERBOSE);
  virtual ~_Impl(void);
};

void
POS::_Impl::reg_types(void){
  _TaggerImpl::reg_types();

  // tag types
  registry.reg(Types::pt);
  registry.reg(Types::ppt);

  // prefix and suffix types
  registry.reg(Types::suff);
  registry.reg(Types::pref);

  // orthographic types
  registry.reg(Types::has_digit);
  registry.reg(Types::has_hyphen);
  registry.reg(Types::has_uppercase);
}
// count 1--4 character prefix features
template <class TC>
void
POS::_Impl::_add_prefixes(TC &tc, const NLP::Sentence &sent, ulong i){
  const std::string &word = sent.words[i];
  std::string::const_iterator j = word.begin();

  std::string affix;
  attributes(tc, Types::pref, affix += *j);
  if(++j == word.end())
    return;
  attributes(tc, Types::pref, affix += *j);
  if(++j == word.end())
    return;
  attributes(tc, Types::pref, affix += *j);
  if(++j == word.end())
    return;
  attributes(tc, Types::pref, affix += *j);
}

// count 1--4 character suffix features
template <class TC>
void
POS::_Impl::_add_suffixes(TC &tc, const NLP::Sentence &sent, ulong i){
  const std::string &word = sent.words[i];
  std::string::const_reverse_iterator j = word.rbegin();

  std::string affix;
  attributes(tc, Types::suff, affix += *j);
  if(++j == word.rend())
    return;
  attributes(tc, Types::suff, affix += *j);
  if(++j == word.rend())
    return;
  attributes(tc, Types::suff, affix += *j);
  if(++j == word.rend())
    return;
  attributes(tc, Types::suff, affix += *j);
}

// count has uppercase, has hyphen and has digit features
template <class TC>
void
POS::_Impl::_add_special(TC &tc, const NLP::Sentence &sent, ulong i){
  const std::string &word = sent.words[i];

  bool upper = false;
  bool digit = false;
  bool hyphen = false;

  for(std::string::const_iterator j = word.begin(); j != word.end(); ++j){
    if(isupper(*j)){
      if(!upper){
        attributes(tc, Types::has_uppercase, NONE);
        upper = true;
      }
    }else if(*j == '-'){
      if(!hyphen){
        attributes(tc, Types::has_hyphen, NONE);
        hyphen = true;
      }
    }else if(isdigit(*j)){
      if(!digit){
        attributes(tc, Types::has_digit, NONE);
        digit = true;
      }
    }
  }
}

// count surrounding word features (2 words to left and right)
template <class TC>
void
POS::_Impl::_add_surrounding(TC &tc, const NLP::Sentence &sent, ulong i){
  if(i > 0){
    attributes(tc, Types::pw, sent.words[i - 1]);
    if(i > 1)
      attributes(tc, Types::ppw, sent.words[i - 2]);
    else
      attributes(tc, Types::ppw, SENTINEL);
  }else{
    attributes(tc, Types::pw, SENTINEL);
    attributes(tc, Types::ppw, SENTINEL);
  }

  size_t last = sent.words.size() - 1;
  if(i < last){
    attributes(tc, Types::nw, sent.words[i + 1]);
    --last;
    if(i < last)
      attributes(tc, Types::nnw, sent.words[i + 2]);
    else
      attributes(tc, Types::nnw, SENTINEL);
  }else{
    attributes(tc, Types::nw, SENTINEL);
    attributes(tc, Types::nnw, SENTINEL);
  }
}

// count previously assigned tag features
// for the previous tag and the bigram (previous tag, prev-prev tag)
template <class TC>
void
POS::_Impl::_add_history(TC &tc, const NLP::Sentence &sent, ulong i){
  static std::string bitag;
  if(i > 0){
    attributes(tc, Types::pt, sent.pos[i - 1]);
    if(i > 1)
      attributes(tc, Types::ppt, sent.pos[i - 2], sent.pos[i - 1]);
    else
      attributes(tc, Types::ppt, SENTINEL, sent.pos[i - 1]);
  }else{
    attributes(tc, Types::pt, SENTINEL);
    attributes(tc, Types::ppt, SENTINEL2);
  }
}

template <class TC>
void
POS::_Impl::_generate(TC &tc, const NLP::Sentence &sent, ulong i){
  // rare word features only switch on for infrequent words
  // with frequency less than config.rare
  if(lexicon.freq(sent.words[i]) < cfg.rare_cutoff()){
    _add_prefixes(tc, sent, i);
    _add_suffixes(tc, sent, i);
    _add_special(tc, sent, i);
  }

  attributes(tc, Types::w, sent.words[i]);

  _add_surrounding(tc, sent, i);
  _add_history(tc, sent, i);
}

// count the number of tags, words and word/tag pairs in the given sentence
void
POS::_Impl::_generate_counts(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    counts.add(sent.pos[i], 1);
    lexicon.add(sent.words[i], 1);

    // build a word-POS tag pair to add to the tag dictionary
    tagdict.add(sent.words[i] + ' ' + sent.pos[i], 1);
  }
  nevents += sent.words.size();
}

// count the features for each training instance in the given sentence
void
POS::_Impl::_generate_features(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.pos.size(); ++i){
    Tag klass = klasses[sent.pos[i]];
    _generate(klass, sent, i);
  }
  nevents += sent.words.size();
}

// add the features for each training instance to the context
// and dump it model/contexts for the given sentence
void
POS::_Impl::_generate_contexts(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    context.resize(0);

    Tag klass = klasses[sent.pos[i]];

    _generate(context, sent, i);

    if(context.size() > 0){
      std::sort(context.begin(), context.end());
      contexts.add(klass, context);
    }
  }
  nevents += sent.words.size();
}

void
POS::_Impl::_make_unknowns(void) const {
  if(Cluster::rank != 0)
    return;
  
  ofstream stream(cfg.unknowns().c_str());
  if(!stream)
    throw NLP::IOException("could not open unknowns file for writing", cfg.unknowns());

  stream << PREFACE << '\n';

  // most of the Penn Treebank POS tags can be assigned
  // when the word has not been seen in the lexicon before
  // basically, the punctuation characters are removed but
  // everything else stays the same
  stream << "CC\nCD\nDT\nEX\nFW\nIN\nJJ\nJJR\nJJS\n";
  stream << "LS\nMD\nNN\nNNP\nNNPS\nNNS\nPDT\nPRP\n";
  stream << "PRP$\nRB\nRBR\nRBS\nRP\nSYM\nTO\nUH\n";
  stream << "VB\nVBD\nVBG\nVBN\nVBP\nVBZ\nWDT\nWP\nWP$\nWRB\n";
  stream.close();

  stream.open(cfg.number_unknowns().c_str());
  if(!stream)
    throw NLP::IOException("could not open number_unknowns file for writing", cfg.number_unknowns());

  stream << PREFACE << '\n';

  stream << "CD\nJJ\nNNP\n";
}

POS::_Impl::_Impl(NLP::Taggers::POS::Config &cfg,
		  const std::string &PREFACE, bool VERBOSE)
  : _TaggerImpl(cfg, PREFACE, VERBOSE), cfg(cfg){}

POS::_Impl::~_Impl(void){}

POS::POS(NLP::Taggers::POS::Config &cfg, const std::string &PREFACE, bool VERBOSE):
  _impl(new _Impl(cfg, PREFACE, VERBOSE)){}

POS::POS(const POS &other): _impl(share(other._impl)){}

POS::~POS(void){
  release(_impl);
}

ulong POS::nevents(void) const { return _impl->nevents; }
ulong POS::ncontexts(void) const { return _impl->ncontexts; }

TagSet POS::tagset(void) const { return _impl->klasses; }
Lexicon POS::lexicon(void) const { return _impl->lexicon; }

void POS::extract(NLP::Reader &reader){ _impl->extract(reader, true); }

} }
