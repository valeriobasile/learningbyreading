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

// NLP::Tagger::TagSetDict
// for a given word stores the tags the tagger is permitted to
// hypothesise (i.e. assign) for that word

// the set of permissible tags are represented as a vector
// of Tag objects

namespace NLP {
  namespace Taggers {

    class TagSetDict {
    public:
      // load a tag dictionary from file using the given tagset and another tagset
      TagSetDict(const std::string &name, const std::string &filename,
		 const NLP::TagSet &tagset, const NLP::TagSet &lexicon);

      // load a tag dictionary from file using the given tagset and lexicon
      // only load word-tag pairs that have a frequency greater than
      // min or have freq(word, tag)/freq(word) >= ratio
      TagSetDict(const std::string &name, const std::string &filename,
		 ulong min, const NLP::TagSet &tagset, const NLP::TagSet &lexicon);

      // shared, reference counted copy constructor
      TagSetDict(const TagSetDict &other);

      ~TagSetDict(void);

      // shared, reference counted assignment
      TagSetDict &operator=(TagSetDict &other);

      const std::string name(void) const;
      size_t size(void) const;

      // does the word-tag pair exist in the tag dictionary  
      bool exists(NLP::Tag word, NLP::Tag tag) const;
      
      // retrieve a reference to the set of tags for a given word
      NLP::Tags &tags(NLP::Tag word) const;
      
      NLP::Tags &operator[](NLP::Tag word) const { return tags(word); };
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

  }
}
