// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Tagger::TagDict
// for a given word stores the tags the tagger is permitted to
// hypothesise (i.e. assign) for that word

// the set of permissible tags are represented as a vector
// of Tag objects

#include "base.h"

#include "hashtable/base.h"
#include "hashtable/word.h"

#include "share.h"

#include "tagger/tagdict.h"

using namespace std;
using namespace NLP::HashTable;

namespace NLP { namespace Taggers {

static Tags EMPTY;

typedef Base<WordEntry<Tags>, const Word, MEDIUM, LARGE> _ImplBase;
class TagDict::_Impl: public _ImplBase, public Shared {
public:
  TagSet tagset;
  Lexicon lexicon;
  std::string PREFACE;

  _Impl(const string &name, const string &filename,
        const TagSet &tagset, const Lexicon &lexicon)
    : _ImplBase(name), tagset(tagset), lexicon(lexicon){
    load(filename, 1.0, 0);
  }

  _Impl(const string &name, const string &filename,
        double ratio, ulong min,
	const TagSet &tagset, const Lexicon &lexicon)
    : _ImplBase(name), tagset(tagset), lexicon(lexicon) {
    load(filename, ratio, min);
  }
  virtual ~_Impl(void){}

  static void add_tag(Tags &tags, Tag tag){
    for(Tags::iterator i = tags.begin(); i != tags.end(); ++i)
      if(*i == tag)
        return;
    tags.push_back(tag);
  }

  void load(const std::string filename, double ratio, ulong min){
    ulong nlines = 0;
    ifstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open wordtags file", filename);

    PREFACE += read_preface(filename, stream, nlines);

    std::string word_str, tag_str;
    ulong freq;
    while(stream >> word_str >> tag_str >> freq){
      ++nlines;
      if(stream.get() != '\n')
	throw IOException("expected newline after frequency in tagdict file", filename, nlines);

      // convert the word into a canonical string
      Word word = lexicon.can(word_str);

      // check to see if the word is eliminated by the cutoffs
      // The parameters for the cutoff are stored in NLP::MaxEnt::Config
      double cutoff = word.freq()/ratio;
      if(freq < min && freq < cutoff)
        continue;

      Tag tag = tagset[tag_str];
      if(!tag)
	continue;

      add_tag(add(word)->value, tag);
    }

    if(!stream.eof())
      throw IOException("could not parse class", filename, nlines);
  }
};

TagDict::TagDict(const std::string &name, const std::string &filename,
                 const TagSet &tagset, const Lexicon &lexicon)
  : _impl(new _Impl(name, filename, tagset, lexicon)),
    PREFACE(_impl->PREFACE){}

TagDict::TagDict(const std::string &name, const std::string &filename,
                 double ratio, ulong min, const TagSet &tagset, const Lexicon &lexicon)
  : _impl(new _Impl(name, filename, ratio, min, tagset, lexicon)),
    PREFACE(_impl->PREFACE){}

TagDict::TagDict(const TagDict &other)
  : _impl(share(other._impl)), PREFACE(_impl->PREFACE){}

TagDict::~TagDict(void){
  release(_impl);
}

TagDict &
TagDict::operator=(TagDict &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

const std::string &
TagDict::name(void) const {
  return _impl->name;
}

size_t
TagDict::size(void) const {
  return _impl->size;
}

Tags &
TagDict::tags(Word word) const {
  WordEntry<Tags> *entry = _impl->find(word);

  if(!entry)
    return EMPTY;

  return entry->value;
}

Tags &
TagDict::tags(const std::string &s) const {
  Word word = _impl->lexicon.can(s);
  if(!word)
    return EMPTY;

  WordEntry<Tags> *entry = _impl->find(word);

  if(!entry)
    return EMPTY;

  return entry->value;
}

bool
TagDict::exists(Word word, Tag tag) const {
  WordEntry<Tags> *entry = _impl->find(word);

  if(!entry)
    return false;

  const Tags &tags = entry->value;
  return std::find(tags.begin(), tags.end(), tag) != tags.end(); 
}

bool
TagDict::exists(const std::string &s, Tag tag) const {
  Word word = _impl->lexicon.can(s);
  if(!word)
    return false;

  WordEntry<Tags> *entry = _impl->find(word);
  if(!entry)
    return false;

  const Tags &tags = entry->value;
  return std::find(tags.begin(), tags.end(), tag) != tags.end(); 
}

} }
