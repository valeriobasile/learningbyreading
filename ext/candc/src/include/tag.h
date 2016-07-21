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

// NLP::Tag
// represents a fixed set of tags or classes as an enumeration
// this representation allows for fast comparison of tags
// and fast lookup of tag related features (using the tag index)

// the representation has been kept as small as possible, but this
// forces us to use an external class NLP::TagSet to perform conversions
// between the enumerated and string representation of tags

namespace NLP {

  class Tag {
  protected:
    // tags are represented with a ushort
    // at most 65535 tag values (on most modern architectures),
    // which should be more than adequate for most applications
    ushort _id;

    // check before casting an unsigned long into a unsigned short
    ushort _check(ulong id){
      if(id > USHRT_MAX)
        throw NLP::Exception("id too large to convert to NLP::Tag");
      return static_cast<ushort>(id);
    }
  public:
    Tag(void): _id(0){}
    Tag(None): _id(0){}
    Tag(Sentinel): _id(1){}

    Tag(ushort id): _id(id){}
    explicit Tag(ulong id): _id(_check(id)){}
    Tag(const Tag &tag): _id(tag._id){}

    Tag &operator ++(void){ ++_id; return *this; }
    ushort value(void) const { return _id; }

    bool operator!(void) const { return _id == 0; }
  };

  inline bool operator==(Tag t1, Tag t2){ return t1.value() == t2.value(); }
  inline bool operator!=(Tag t1, Tag t2){ return t1.value() != t2.value(); }
  inline bool operator<(Tag t1, Tag t2){ return t1.value() < t2.value(); }

  inline bool operator==(Tag t, None){ return t.value() == None::val; }
  inline bool operator==(Tag t, Sentinel){ return t.value() == Sentinel::val; }

  inline bool operator!=(Tag t, None){ return t.value() != None::val; }
  inline bool operator!=(Tag t, Sentinel){ return t.value() != Sentinel::val; }

  struct ScoredTag {
    float score;
    Tag tag;
  };

  inline bool operator<(const ScoredTag &t1, const ScoredTag &t2){
    return t1.score > t2.score;
  }

  inline bool operator==(const ScoredTag &t1, const ScoredTag &t2){
    return t1.tag == t2.tag && t1.score == t2.score;
  }
}
