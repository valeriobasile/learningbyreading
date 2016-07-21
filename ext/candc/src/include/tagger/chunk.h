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
  namespace Taggers {

    class Chunk: public Tagger {
    public:
      class Config: public Tagger::Config {
      public:
	OpPath postags;

      	Config(const OpPath *base = 0, Mode mode = DECODE,
	       const std::string &name = "chunk",
	       const std::string &desc = "chunk tagger config");
      };
    public:
      Chunk(Chunk::Config &config);
      Chunk(Chunk &other);

      virtual ~Chunk(void);

      // the set of POS tags the chunk tagger uses as features
      TagSet postags(void) const;
    protected:
      // private implementation trick
      class Impl;
    };

  }
}
