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


namespace NLP {

  struct ScoredRaw {
    float score;
    Raw raw;

    ScoredRaw(void)
      : score(0.0){}
    ScoredRaw(Raw raw, float score)
      : score(score), raw(raw){}
  };

  inline bool operator<(const ScoredRaw &t1, const ScoredRaw &t2){
    return t1.score > t2.score;
  }

  inline bool operator==(const ScoredRaw &t1, const ScoredRaw &t2){
    return t1.raw == t2.raw && t1.score == t2.score;
  }

  typedef std::vector<Raw> Raws;
  typedef std::vector<ScoredRaw> MultiRaw;
  typedef std::vector<MultiRaw> MultiRaws;

  typedef std::vector<RawWord> RawWords;
  typedef std::vector<RawTag> RawTags;

  typedef std::vector<Word> Words;
  typedef offset_vector<Word, 3, 3> OffsetWords;

  typedef std::vector<Tag> Tags;
  typedef offset_vector<Tag, 3, 4> OffsetTags;

  typedef std::vector<ScoredTag> MultiTag;
  typedef std::vector<MultiTag> MultiTags;

}
