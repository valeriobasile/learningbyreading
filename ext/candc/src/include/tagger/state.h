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

    class State {
    public:
      Lattice current;      // the lattice up to the word we have just tagged
      NodeMatrix next;      // the new hypothetical tags for the current word
      PDF dist;             // pdf used inside the tight inner lattice loop
      PDF dist_word;        // pdf used before the tight inner lattice loop
                            // to cache the common feature weights

      Flattice flattice;    // the lattice for the forward/backward multi-tagging

      // the following sequences are tagger internal versions
      // of the input data from the Sentence object

      Raws raws;            // raw words
      OffsetWords words;    // canonized words
      OffsetTags pos;       // canonized POS tags
      OffsetTags chunks;    // canonized chunks
      OffsetTags entities;  // canonized named entities

      TagHist last_klass;   // used by the named entity recogniser
      
      State(ulong NKLASSES, ulong MAXWORDS)
        : next(NKLASSES), dist(NKLASSES), dist_word(NKLASSES, 1.0),
        flattice(MAXWORDS, NKLASSES){
        current.reserve(NKLASSES*NKLASSES);
      }

      // apply the beam and eliminate the paths which can no
      // longer be the most probable in the Markov window
      void reduce(const ulong BEAM_WIDTH, const float BEAM_RATIO){
        current.clear();
        next.clear();
        for(PDF::iterator i = dist_word.begin() + 2; i != dist_word.end(); ++i)
          *i = 1.0;

        swap(current, next.used);

        if(BEAM_WIDTH < current.size()){
          Lattice::iterator beam_end = current.begin() + BEAM_WIDTH;
          partial_sort(current.begin(), beam_end, current.end(), NodeGTCmp());
          while(--beam_end > current.begin() && (*beam_end)->sum - current.front()->sum < BEAM_RATIO)
            ;
          beam_end++;
          for(Lattice::iterator i = beam_end; i != current.end(); ++i)
            next.free(*i);

          current.erase(beam_end, current.end());
        }
      }

      // method to reset state before tagging a sentence
      void begin_sentence(void){
        current.clear();
        next.reset();
      }

      // method to reset state before tagging a document
      void begin_document(void){
        last_klass.clear();
      }
    };

  }
}
