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

  using namespace IO;
  using namespace Config;

  namespace Taggers {

    // the internal tagger state information used during tagging
    class State;

    typedef int Algorithm;

    const static Algorithm VITERBI = 1;
    const static Algorithm NOSEQ = 2;
    const static Algorithm GREEDY = 3;
    const static Algorithm FWDBWD = 4;

    inline Algorithm str2alg(const std::string &s){
      if(s == "viterbi")
        return VITERBI;
      else if(s == "noseq")
        return NOSEQ;
      else if(s == "greedy")
        return GREEDY;
      else if(s == "fwdbwd")
        return FWDBWD;
      else
        throw NLP::Exception("unrecognised decoding algorithm '" + s + "' [viterbi, noseq, greedy, fwdbwd]");
    }

    inline void check_alg(const std::string &s){
      str2alg(s);
    }

    class Tagger {
    public:
      class Config: public Model::Config {
      public:
        OpPath tagdict;
        OpPath unknowns;

        Op<ulong> cutoff_default;
        Op<ulong> cutoff_words;
        Op<ulong> cutoff_attribs;

        Op<ulong> rare_cutoff;

        Op<ulong> beam_width;
        Op<double> beam_ratio;
        Op<double> forward_beam_ratio;

        Op<ulong> tagdict_min;
        Op<double> tagdict_ratio;

        Op<ulong> maxwords;

        Config(const std::string &name, const std::string &desc,
               const OpPath *base, Mode mode, double SIGMA,
               ulong NITER);
        virtual ~Config(void){ /* do nothing */ }
      };
    public:
      Config &cfg;

      virtual ~Tagger(void);

      NLP::TagSet tagset(void) const;
      NLP::Lexicon lexicon(void) const;
      TagDict tagdict(void) const;

      // the set of tags permissible for words not seen in the training data
      const NLP::Tags &unknown_tags(void) const;

      virtual State *create_state(void) const;
      virtual void begin_document(State *state = 0) const;

      // tag a single sentence
      virtual void tag(NLP::Sentence &sent, Algorithm alg, ulong DICT_CUTOFF,
                       State *state = 0) const;
      // tag from a reader and output to a writer
      virtual void tag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
                       Algorithm alg, ulong DICT_CUTOFF) const;

      // tag a single sentence
      virtual void mtag(NLP::Sentence &sent, Algorithm alg, ulong DICT_CUTOFF, double BETA,
                        State *state = 0) const;
      // tag from a reader and output to a writer
      virtual void mtag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
                        Algorithm alg, ulong DICT_CUTOFF, double BETA) const;

      // private implementation trick
      class Impl;
    protected:
      Tagger(Tagger::Config &cfg, Impl *impl);
      Tagger(Tagger &other);

      Impl *impl_;
    };

  }
}
