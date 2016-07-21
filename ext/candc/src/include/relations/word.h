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
  namespace Relations {

    class Word {
    public:
      NLP::Word word;
      const char *tag;
      const char *chunk;

      Word *attached;
      Word *prep;

      Word(NLP::Word w, const char *t, const char *c):
        word(w), tag(t), chunk(c), attached(0), prep(0) {};
      ~Word(void) {};

      bool is_adj(void) const { return tag[0] == 'J'; };
      bool is_adv(void) const { return tag[0] == 'R' && tag[1] == 'B'; };

      bool is_det(void) const { return tag[0] == 'D'; };
      bool is_prep(void) const { return tag[0] == 'I' || tag[0] == 'T'; };
      bool is_pos(void) const { return tag[0] == 'P' && tag[1] == 'O'; };

      bool is_noun(void) const { return tag[0] == 'N'; };
      bool is_cnoun(void) const { return tag[0] == 'N' && tag[2] != 'P'; };
      bool is_pnoun(void) const { return tag[0] == 'N' && tag[2] == 'P'; };

      bool is_verb(void) const { return tag[0] == 'V'; };
      bool is_pverb(void) const { return tag[0] == 'V' && tag[2] == 'G'; };

      bool is_to_be(void) const {
        switch(word[0]){
          case 'a': return (word[1] == 'm' && !word[2]) || (word[1] == 'r' && word[2] == 'e' && !word[3]);
          case 'b': return word[1] == 'e' && (!word[2] || (word[2] == 'e' && word[3] == 'n' && !word[4]));
          case 'h': return word[1] == 'a' && ((word[2] == 'v' && word[3] == 'e' && !word[4]) ||
                                              (word[2] == 's' && !word[3]));
          case 'i': return (word[1] == 's' && !word[2]);
          case 'w': return (word[1] == 'a' && word[2] == 's' && !word[3]) ||
                           (word[1] == 'e' && word[2] == 'r' && word[3] == 'e' && !word[4]) ||
                           (word[1] == 'i' && word[2] == 'l' && word[3] == 'l' && !word[4]);
          case '\'': return (word[1] == 'm' && !word[2]) ||
                            (word[1] == 's' && !word[2]) ||
                            (word[1] == 'r' && word[2] == 'e' && !word[3]);
          default: return false;
        }
      }

      bool is_being(void) const { return word[0] == 'b' && word[1] == 'e' && word[2] == 'i' &&
                                  word[3] == 'n' && word[4] == 'g'; }

      bool is_begin(void) const { return chunk[0] == 'B' || chunk[0] == 'C' || chunk[0] == 'O'; }
      bool is_pp(void) const { return chunk[2] == 'P' && chunk[3] == 'P'; }
      bool is_np(void) const { return chunk[2] == 'N'; }
      bool is_vp(void) const { return chunk[2] == 'V'; }
      bool is_cc(void) const { return (tag[0] == 'C' && tag[1] == 'C') || (chunk[2] == 'C'); }
      bool is_advp(void) const { return chunk[2] == 'A' && tag[4] == 'V'; }
    };

    typedef std::vector<Word> Words;

  }
}
