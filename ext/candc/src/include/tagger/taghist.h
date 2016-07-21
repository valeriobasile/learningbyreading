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

// NLP::Tagger::TagHist
// store the tag that a word was last tagged with
// in the current 'document' for the history feature

namespace NLP {
  namespace Taggers {

    class TagHist {
    public:
      // create an empty tag history
      TagHist(const std::string &name = "taghist");

      // shared, reference counted copy constructor
      TagHist(const TagHist &other);

      ~TagHist(void);

      const std::string name(void) const;
      size_t size(void) const;

      // retrieve the previously assigned tag 
      // returns TagSet::NONE() if there has been no
      // previous occurrence of the word
      Tag get(const std::string &str) const;
      Tag operator[](const std::string &str) const { return get(str); };

      // if the word has been tagged before in the current document
      // change the tag, otherwise add a new entry for the current word
      void add(const std::string &str, const Tag tag);

      // reset the memory
      void clear(void);
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

  }
}
