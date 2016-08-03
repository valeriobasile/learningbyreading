// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Tagger::Unigram
// store the most probable tag for a word
// also called the the 'unigram probability'
// used by the unigram probability guess feature
// for the next word and the next-next word.

// this is loaded from the tag dictionary on startup

#include "base.h"

#include "hashtable/base.h"
#include "hashtable/word.h"

#include "tagger/unigram.h"

#include "share.h"

using namespace std;

namespace NLP { namespace Taggers {

using namespace HashTable;

typedef Base<WordEntry<Tag>, const Word, MEDIUM, LARGE> _ImplBase;
class Unigram::_Impl: public _ImplBase, public Shared {
public:
  TagSet tagset;
  Lexicon lexicon;
  std::string PREFACE;

  _Impl(const string &name, const TagSet &tagset, const Lexicon &lexicon)
    : _ImplBase(name), tagset(tagset), lexicon(lexicon){}

  _Impl(const string &name, const string &filename,
        const TagSet &tagset, const Lexicon &lexicon)
    : _ImplBase(name), tagset(tagset), lexicon(lexicon){
    load(filename);
  }

  virtual ~_Impl(void){}

  // load from the tag dictionary file
  // keeping the most frequent tag for each word
  void load(const std::string filename){
    ulong nlines = 0;
    ifstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open wordtags file " + filename);

    PREFACE += read_preface(filename, stream, nlines);

    std::string prev_word = "";
    std::string max_tag = "";
    ulong max_freq = 0;

    std::string word_str, tag_str;
    ulong freq;
    while(stream >> word_str >> tag_str >> freq){
      // if the word in the tag dictionary has changed
      // and we have a maximum frequency (i.e. we aren't
      // just reading the first line of the file)
      if(word_str != prev_word){
        if(max_freq){
          Word word = lexicon.can(prev_word);
	  add(word)->value = tagset[max_tag];
        }
        prev_word = word_str;
        max_tag = tag_str;
        max_freq = freq;
      }else if(freq > max_freq){
        max_tag = tag_str;
        max_freq = freq;
      }
    }

    // also load the last entry from the file
    if(max_freq){
      Word word = lexicon.can(prev_word);
      add(word)->value = tagset[max_tag];
    }

    // check that we have reached the end of the file
    if(!stream.eof())
      throw IOException("could not parse class", filename, size);
  }
};

Unigram::Unigram(const std::string &name, const TagSet &tagset,
		 const Lexicon &lexicon)
  : _impl(new _Impl(name, tagset, lexicon)){}

Unigram::Unigram(const std::string &name, const std::string &filename,
                 const TagSet &tagset, const Lexicon &lexicon)
  : _impl(new _Impl(name, filename, tagset, lexicon)){}

Unigram::Unigram(const Unigram &other)
  : _impl(share(other._impl)){}

Unigram::~Unigram(void){
  release(_impl);
}

const std::string
Unigram::name(void) const { return _impl->name; }

size_t
Unigram::size(void) const { return _impl->size; }

Tag
Unigram::tag(Word word) const {
  WordEntry<Tag> *entry = _impl->find(word);
  return entry ? entry->value : NONE;
}

void
Unigram::load(const std::string &filename){ _impl->load(filename); }

} }
