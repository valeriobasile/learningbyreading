// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Extract::NER
// extracts features from named entity tagged training data
// saves the extracted model into the specified model directory
// which is then loaded by NLP::MaxEnt::GIS for estimating
// the parameters of the model, and NLP::Taggers::NER for tagging

#include "extract/_baseimpl.h"
#include "extract/tagger.h"

#include "tagger/ner.h"
#include "extract/ner.h"

#include "tagger/unigram.h"
#include "analyser.h"
#include "gazetteers.h"
#include "wordtype.h"

using namespace std;

namespace NLP { namespace Extract {

// length of current word is 1-14 and 15 and above ("0" ensures array is indexed by the length)
const static char *const lengths[] = { "0", "1", "2", "3", "4", "5", "6", "7",
                                       "8", "9", "10", "11", "12", "13", "14", "15" };

// private implementation, which is shared
class NER::_Impl: public _TaggerImpl {
protected:
  void reg_types(void);

  template <class TC>
  void _add_surrounding_words(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding_pos(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_surrounding_chunks(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_history(TC &tc, const Sentence &sent, ulong i);

  template <class TC>
  void _add_prefixes(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_suffixes(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_special(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_last(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_gazetteers(TC &tc, const Sentence &sentence, ulong i);
  template <class TC>
  void _add_prev_gazetteers(TC &tc, const Sentence &sent, ulong i);
  template <class TC>
  void _add_next_gazetteers(TC &tc, const Sentence &sent, ulong i);

  template <class TC>
  void _add_unigrams(TC &tc, const Sentence &sentence, ulong i);
  template <class TC>
  void _add_wordtypes(TC &tc, const Sentence &sentence, ulong i);

  template <class TC>
  void _generate(TC &tc, const Sentence &sent, ulong i);

  void _generate_counts(const Sentence &sent);
  void _generate_features(const Sentence &sent);
  void _generate_contexts(const Sentence &sent);
  void _make_unknowns(void) const;

  void _pass1(NLP::IO::Reader &reader, bool save_klasses=true);
  void _pass2(NLP::IO::Reader &reader);
  void _pass2_postload(NLP::IO::Reader &reader);
  void _pass3(NLP::IO::Reader &reader);
public:
  NLP::Taggers::NER::Config &cfg;
  const Types types;

  NLP::Taggers::TagHist last_klass;
  NLP::Taggers::Unigram unigrams;
  NLP::Gazetteers gaz;

  Lexicon poscounts;
  Lexicon chunkcounts;

  _Impl(NLP::Taggers::NER::Config &cfg, const std::string &PREFACE, bool VERBOSE);
  virtual ~_Impl(void);
};

void
NER::_Impl::reg_types(void){
  _TaggerImpl::reg_types();

  // tag types
  registry.reg(Types::t);
  registry.reg(Types::pt);
  registry.reg(Types::ppt);
  registry.reg(Types::nt);
  registry.reg(Types::nnt);

  // chunk types
  registry.reg(Types::c);
  registry.reg(Types::pc);
  registry.reg(Types::ppc);
  registry.reg(Types::nc);
  registry.reg(Types::nnc);

  // NE types
  registry.reg(Types::pne);
  registry.reg(Types::ppne);

  // prefix and suffix types
  registry.reg(Types::suff);
  registry.reg(Types::pref);

  // orthographic types
  registry.reg(Types::has_digit);
  registry.reg(Types::has_hyphen);
  registry.reg(Types::has_period);
  registry.reg(Types::has_punct);
  registry.reg(Types::has_uppercase);

  registry.reg(Types::alphanum);
  registry.reg(Types::number);
  registry.reg(Types::roman);
  registry.reg(Types::initial);
  registry.reg(Types::acronym);

  registry.reg(Types::kase);
  registry.reg(Types::digits);
  registry.reg(Types::length);

  // gazetteer types
  registry.reg(Types::gaz_common);
  registry.reg(Types::gaz_first);
  registry.reg(Types::gaz_last);
  
  registry.reg(Types::pgaz_common);
  registry.reg(Types::pgaz_first);
  registry.reg(Types::pgaz_last);

  registry.reg(Types::ngaz_common);
  registry.reg(Types::ngaz_first);
  registry.reg(Types::ngaz_last);

  // gazetteer and sentence position types
  registry.reg(Types::xcommon_bs);
  registry.reg(Types::xcommon_ms);

  // wordtype types
  registry.reg(Types::wt);   // sc (12/04/05) changed from a
  registry.reg(Types::pwt);  // was pa
  registry.reg(Types::ppwt); // was ppa 
  registry.reg(Types::nwt);  // was na
  registry.reg(Types::nnwt); // was nna

  // bigram wordtype types
  registry.reg(Types::ppbt);   // was ppapa sc: how do these correspond?
  registry.reg(Types::pbt);    // was paa
  registry.reg(Types::bt);     // was pana
  registry.reg(Types::nbt);    // was ana
  registry.reg(Types::nnbt);   // was nanna

  // trigram wordtype types
  registry.reg(Types::ptt);  // was pppappapa
  registry.reg(Types::ntt);  // was nannannna

  // miscellaneous special types
  registry.reg(Types::last);
  registry.reg(Types::nu);
  registry.reg(Types::nnu);
}
// count 1--4 character prefix features
template <class TC>
void
NER::_Impl::_add_prefixes(TC &tc, const NLP::Sentence &sent, ulong i){
  if(!types.use_prefix())
    return;

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
NER::_Impl::_add_suffixes(TC &tc, const NLP::Sentence &sent, ulong i){
  if(!types.use_suffix())
    return;

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

// add surrounding word features (2 words to left and right) to the context
template <class TC>
void
NER::_Impl::_add_surrounding_words(TC &tc, const Sentence &sent, ulong i){
  if(!types.use_words())
    return;

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

// add surrounding POS tag features (2 tags to left and right) to the context
template <class TC>
void
NER::_Impl::_add_surrounding_pos(TC &tc, const Sentence &sent, ulong i){
  if(!types.use_pos())
    return;

  if(i > 0){
    attributes(tc, Types::pt, sent.pos[i - 1]);
    if(i > 1)
      attributes(tc, Types::ppt, sent.pos[i - 2]);
    else
      attributes(tc, Types::ppt, SENTINEL);
  }else{
    attributes(tc, Types::pt, SENTINEL);
    attributes(tc, Types::ppt, SENTINEL);
  }

  size_t last = sent.pos.size() - 1;
  if(i < last){
    attributes(tc, Types::nt, sent.pos[i + 1]);
    --last;
    if(i < last)
      attributes(tc, Types::nnt, sent.pos[i + 2]);
    else
      attributes(tc, Types::nnt, SENTINEL);
  }else{
    attributes(tc, Types::nt, SENTINEL);
    attributes(tc, Types::nnt, SENTINEL);
  }
}

// add surrounding chunk tag features (2 chunks to left and right) to the context
template <class TC>
void
NER::_Impl::_add_surrounding_chunks(TC &tc, const Sentence &sent, ulong i){
  if(!types.use_chunks())
    return;

  if(i > 0){
    attributes(tc, Types::pc, sent.chunks[i - 1]);
    if(i > 1)
      attributes(tc, Types::ppc, sent.chunks[i - 2]);
    else
      attributes(tc, Types::ppc, SENTINEL);
  }else{
    attributes(tc, Types::pc, SENTINEL);
    attributes(tc, Types::ppc, SENTINEL);
  }

  size_t last = sent.chunks.size() - 1;
  if(i < last){
    attributes(tc, Types::nc, sent.chunks[i + 1]);
    --last;
    if(i < last)
      attributes(tc, Types::nnc, sent.chunks[i + 2]);
    else
      attributes(tc, Types::nnc, SENTINEL);
  }else{
    attributes(tc, Types::nc, SENTINEL);
    attributes(tc, Types::nnc, SENTINEL);
  }
}

// count a range of orthographic features including length
// has hyphen, has period, has punctuation, has digit
// digit patterns, is number, is alphanumeric, is roman numerals
// case features, etc
// uses the NLP::Analyser to efficiently extract features
template <class TC>
void
NER::_Impl::_add_special(TC &tc, const Sentence &sent, ulong i){
  Analyser a(sent.words[i]);

  if(types.use_length()){
    if(a.len < 16)
      attributes(tc, Types::length, lengths[a.len]);
    else
      attributes(tc, Types::length, "m");
  }

  if(a.nhyphens && types.use_has_hyphen())
    attributes(tc, Types::has_hyphen, NONE);

  if(a.nperiods && types.use_has_period())
    attributes(tc, Types::has_period, NONE);

  if(a.npunct && types.use_has_punct())
    attributes(tc, Types::has_punct, NONE);

  if(a.ndigits){
    if(types.use_has_digit())
      attributes(tc, Types::has_digit, NONE);

    if(a.is_digits()){
      switch(a.len){
        case 1: if(types.use_one_digit()) attributes(tc, Types::digits, "1"); break;
        case 2: if(types.use_two_digits()) attributes(tc, Types::digits, "2"); break;
        case 3: if(types.use_three_digits()) attributes(tc, Types::digits, "3"); break;
        case 4: if(types.use_four_digits()) attributes(tc, Types::digits, "4"); break;
        default: if(types.use_digits()) attributes(tc, Types::digits, "m"); break;
      }
      if(types.use_number())
        attributes(tc, Types::number, NONE);
      return;
    }

    if(a.is_number()){
      if(types.use_number())
        attributes(tc, Types::number, NONE);
      return;
    }

    if(a.is_alphanum()){
      if(types.use_alphanum())
        attributes(tc, Types::alphanum, NONE);
      return;
    }
  }

  if(a.is_roman() && types.use_roman())
    attributes(tc, Types::roman, NONE);

  if(a.nupper){
    if(types.use_has_uppercase())
      attributes(tc, Types::has_uppercase, NONE);

    if(types.use_case()){
      if(a.fupper){
        if(a.uc())
          attributes(tc, Types::kase, "uc");
        else if(a.tc())
          attributes(tc, Types::kase, "tc");
        else if(a.mc())
          attributes(tc, Types::kase, "mc");
      }
    }

    if(a.is_initial()){
      if(types.use_initial())
        attributes(tc, Types::initial, NONE);
    }else if(a.is_acronym()){
      if(types.use_acronym())
        attributes(tc, Types::acronym, NONE);
    }
  }else if(a.lc() && types.use_case())
    attributes(tc, Types::kase, "lc");
}

// count previously assigned tag features
// for the previous tag and the bigram (previous tag, prev-prev tag)
template <class TC>
void
NER::_Impl::_add_history(TC &tc, const Sentence &sent, ulong i){
  if(!types.use_history())
    return;

  if(i > 0){
    attributes(tc, Types::pne, sent.entities[i - 1]);
    if(i > 1)
      attributes(tc, Types::ppne, sent.entities[i - 2], sent.entities[i - 1]);
    else
      attributes(tc, Types::ppne, SENTINEL, sent.entities[i - 1]);
  }else{
    attributes(tc, Types::pne, SENTINEL);
    attributes(tc, Types::ppne, SENTINEL2);
  }
}

// add the 'history' feature (the tag this word was last assigned in the document)
template <class TC>
void
NER::_Impl::_add_last(TC &tc, const Sentence &sent, ulong i){
  Tag tag = last_klass[sent.words[i]];
  if(tag.value())
    attributes(tc, Types::last, klasses[tag]);
}

// count current word gazetteer features
// there are several feature types here:

// is the current word more frequently seen in lowercase than other case forms
// and the same with a marker indicating whether we are at the beginning
// of the sentence or not

// gazetteer features for first and last name
// three gazetteer features for three different location gazetteers
// if other gazetteers are loaded, then they will need to be added
// as features to this function

// this should really be generalised since we can now add new gazetteers
// without modifying any code
template <class TC>
void
NER::_Impl::_add_gazetteers(TC &tc, const Sentence &sent, ulong i){
  const std::string &word = sent.words[i];

  ulong flags = gaz.lower(word);
  if(flags & Gazetteers::COMMON){
    if(types.use_gaz_common())
      attributes(tc, Types::gaz_common, NONE);

    if(types.use_composites()){
        if(i == 0)
          attributes(tc, Types::xcommon_bs, NONE);
        else
          attributes(tc, Types::xcommon_ms, NONE);
    }
  }

  flags = gaz[word];
  if(types.use_gaz_first() && (flags & Gazetteers::FIRST))
    attributes(tc, Types::gaz_first, NONE);
  if(types.use_gaz_last() && (flags & Gazetteers::LAST))
    attributes(tc, Types::gaz_last, NONE);
}

// same as above for counting the previous word (except for leaving out
// the complex common gazetteer features)
template <class TC>
void
NER::_Impl::_add_prev_gazetteers(TC &tc, const Sentence &sent, ulong i){
  const std::string &word = sent.words[i];

  ulong flags = gaz.lower(word);
  if(types.use_gaz_common() && (flags & Gazetteers::COMMON))
    attributes(tc, Types::pgaz_common, NONE);

  flags = gaz[word];
  if(types.use_gaz_first() && (flags & Gazetteers::FIRST))
    attributes(tc, Types::pgaz_first, NONE);

  if(types.use_gaz_last() && (flags & Gazetteers::LAST))
    attributes(tc, Types::pgaz_last, NONE);
}

// counting next word gazetteer features
template <class TC>
void
NER::_Impl::_add_next_gazetteers(TC &tc, const Sentence &sent, ulong i){
  const std::string &word = sent.words[i];

  ulong flags = gaz.lower(word);
  if(types.use_gaz_common() && (flags & Gazetteers::COMMON))
    attributes(tc, Types::ngaz_common, NONE);

  flags = gaz[word];
  if(types.use_gaz_first() && (flags & Gazetteers::FIRST))
    attributes(tc, Types::ngaz_first, NONE);
  if(types.use_gaz_last() && (flags & Gazetteers::LAST))
    attributes(tc, Types::ngaz_last, NONE);
}

// counting forward looking unigram features
// i.e. the most frequently seen tag for the next word
// we look at the next word and the next-next word
template <class TC>
void
NER::_Impl::_add_unigrams(TC &tc, const Sentence &sent, ulong i){
  size_t last = sent.words.size() - 1;
  if(i < last){
    if(types.use_nu())
      attributes(tc, Types::nu, klasses[unigrams[lexicon[sent.words[i + 1]]]]);
    if(types.use_nnu()){
      if(i < last - 1)
        attributes(tc, Types::nnu, klasses[unigrams[lexicon[sent.words[i + 2]]]]);
      else
        attributes(tc, Types::nnu, SENTINEL);
    }
  }else{
    if(types.use_nu())
      attributes(tc, Types::nu, SENTINEL);
    if(types.use_nnu())
      attributes(tc, Types::nnu, SENTINEL);
  }
}

// count all word-type features around the current word in one go
// including single word, bigram and trigram word types
// the NLP::WordType class creates a word-type representation
// for these directly, so adding bigrams and trigrams is simple
template <class TC>
void
NER::_Impl::_add_wordtypes(TC &tc, const Sentence &sent, ulong i){
  if(!types.use_wordtypes())
    return;

  WordType WT;

  attributes(tc, Types::wt, WT(sent.words[i]));

  if(i > 0){
    attributes(tc, Types::pwt, WT(sent.words[i - 1]));
    if(types.use_bitypes())
      attributes(tc, Types::pbt, WT(sent.words[i - 1], sent.words[i]));
    if(i > 1){
      attributes(tc, Types::ppwt, WT(sent.words[i - 2]));
      if(types.use_bitypes())
        attributes(tc, Types::ppbt, WT(sent.words[i - 2], sent.words[i - 1]));
      if(types.use_tritypes() && i > 2)
        attributes(tc, Types::ptt, WT(sent.words[i - 3], sent.words[i - 2], sent.words[i - 1]));
    }
  }

  size_t last = sent.words.size() - 1;
  if(i < last){
    attributes(tc, Types::nwt, WT(sent.words[i + 1]));
    if(types.use_bitypes()){
      attributes(tc, Types::nbt, WT(sent.words[i], sent.words[i + 1]));
      if(i > 0)
        attributes(tc, Types::bt, WT(sent.words[i - 1], sent.words[i + 1]));
    }
    if(i < last - 1){
      attributes(tc, Types::nnwt, WT(sent.words[i + 2]));
      if(types.use_bitypes())
        attributes(tc, Types::nnbt, WT(sent.words[i + 1], sent.words[i + 2]));
      if(types.use_tritypes() && i < last - 2)
        attributes(tc, Types::ntt, WT(sent.words[i + 1], sent.words[i + 2], sent.words[i + 3]));
    }
  }
}

// count the number of tags, words and word/tag pairs in the given sentence
void
NER::_Impl::_generate_counts(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    lexicon.add(sent.words[i], 1);
    if(types.use_pos())
      poscounts.add(sent.pos[i], 1);
    if(types.use_chunks())
      chunkcounts.add(sent.chunks[i], 1);
    counts.add(sent.entities[i], 1);

    // build a word-POS tag pair to add to the tag dictionary
    tagdict.add(sent.words[i] + ' ' + sent.entities[i], 1);
  }
  nevents += sent.words.size();
}

template <class TC>
void
NER::_Impl::_generate(TC &tc, const Sentence &sent, ulong i){
  const ulong last = sent.words.size() - 1;

  // rare word features only switch on for infrequent words
  if(lexicon.freq(sent.words[i]) < cfg.rare_cutoff()){
    _add_prefixes(tc, sent, i);
    _add_suffixes(tc, sent, i);
    _add_special(tc, sent, i);
  }

  if(types.use_words())
    attributes(tc, Types::w, sent.words[i]);

  if(types.use_last())
    _add_last(tc, sent, i);

  if(types.use_pos())
    attributes(tc, Types::t, sent.pos[i]);

  if(types.use_chunks())
    attributes(tc, Types::c, sent.chunks[i]);

  if(types.use_gazetteers())
    _add_gazetteers(tc, sent, i);
  if(types.use_prev_gazetteers() && i > 0)
    _add_prev_gazetteers(tc, sent, i - 1);
  if(types.use_next_gazetteers() && i < last)
    _add_next_gazetteers(tc, sent, i + 1);

  _add_surrounding_words(tc, sent, i);
  _add_surrounding_pos(tc, sent, i);
  _add_surrounding_chunks(tc, sent, i);

  _add_unigrams(tc, sent, i);
  _add_history(tc, sent, i);
  _add_wordtypes(tc, sent, i);
}

// count the features for each training sentence in the given sentence
void
NER::_Impl::_generate_features(const Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    Tag klass = klasses[sent.entities[i]];

    _generate(klass, sent, i);

    last_klass.add(sent.words[i], klass);
  }
  nevents += sent.words.size();
}

// add the features for each training instance to the context
// and dump it model/contexts for the given sentence
void
NER::_Impl::_generate_contexts(const NLP::Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i){
    context.resize(0);

    Tag klass = klasses[sent.entities[i]];

    _generate(context, sent, i);

    if(context.size() > 0){
      std::sort(context.begin(), context.end());
      contexts.add(klass, context);
    }

    last_klass.add(sent.words[i], klass);
  }
  nevents += sent.words.size();
}

void
NER::_Impl::_make_unknowns(void) const {
  ofstream stream(cfg.unknowns().c_str());
  if(!stream)
    throw NLP::IOException("could not open unknown_tags file for writing", cfg.unknowns());

  stream << PREFACE << '\n';
}

void
NER::_Impl::_pass1(NLP::IO::Reader &reader, bool save_klasses){
  _TaggerImpl::_pass1(reader, save_klasses);

  // POS tags are sorted alphabetically and dumped
  poscounts.sort_by_alpha();
  poscounts.save(cfg.postags(), PREFACE);

  // postag dictionary is sorted alphabetically on postag-tag pairs and dumped
  chunkcounts.sort_by_alpha();
  chunkcounts.save(cfg.chunktags(), PREFACE);
}

// count the number of features and attributes extracted from the training data
// dump out to model/features and model/attributes
void
NER::_Impl::_pass2(NLP::IO::Reader &reader){
  last_klass.clear();
  _TaggerImpl::_pass2(reader);
}

void
NER::_Impl::_pass2_postload(NLP::IO::Reader &reader) {
  // load the counts from pass1 back into klasses and unigrams
  unigrams.load(cfg.tagdict());
  _TaggerImpl::_pass2_postload(reader);
}


// extract features, translate them to attribute IDs for context vectors
// dump context vectors to model/contexts and compress to merge duplicates
void
NER::_Impl::_pass3(NLP::IO::Reader &reader){
  last_klass.clear();
  _TaggerImpl::_pass3(reader);
}

NER::_Impl::_Impl(NLP::Taggers::NER::Config &cfg, const std::string &PREFACE, bool VERBOSE)
  : _TaggerImpl(cfg, PREFACE, VERBOSE), cfg(cfg), types(cfg.types),
    last_klass("last_klass"), unigrams("unigrams", klasses, lexicon),
    gaz("gazetteers", cfg.path(), cfg.gazetteers()),
    poscounts("poscounts"), chunkcounts("chunkcounts"){}

NER::_Impl::~_Impl(void) {}

NER::NER(NLP::Taggers::NER::Config &cfg, const std::string &PREFACE, bool VERBOSE)
  : _impl(new _Impl(cfg, PREFACE, VERBOSE)){}

NER::NER(const NER &other): _impl(share(other._impl)){}

NER::~NER(void){
  release(_impl);
}

ulong NER::nevents(void) const { return _impl->nevents; }
ulong NER::ncontexts(void) const { return _impl->ncontexts; }

TagSet NER::tagset(void) const { return _impl->klasses; }
Lexicon NER::lexicon(void) const { return _impl->lexicon; }

void NER::extract(NLP::IO::Reader &reader){ _impl->extract(reader, true); }

} }
