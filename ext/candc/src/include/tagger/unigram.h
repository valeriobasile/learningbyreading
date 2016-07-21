/* -*- Mode: C++; -*- */
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

namespace NLP {
  namespace Taggers {

    class Unigram {
    public:
      // create an empty Unigram hash table
      Unigram(const std::string &name, const TagSet &tagset, const Lexicon &lexicon);
      // load Unigram hash table from a file
      Unigram(const std::string &name, const std::string &src,
	      const TagSet &tagset, const Lexicon &lexicon);

      // shared, reference counted copy constructor
      Unigram(const Unigram &other);

      ~Unigram(void);

      const std::string name(void) const;
      size_t size(void) const;

      Tag tag(Word word) const;
      Tag operator[](Word word) const { return tag(word); };

      void load(const std::string &src);
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

  }
}
