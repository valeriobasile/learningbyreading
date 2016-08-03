// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_printer.h"
#include "parser/print_deps.h"

using namespace std;

namespace NLP { namespace CCG {

void
DepsPrinter::header(const std::string &PREFACE){
  out.stream << PREFACE << endl;
  log.stream << PREFACE << endl;
}

void
DepsPrinter::derivation(const SuperCat *sc, Sentence &sent){
  if(sc->left){
    assert(sc->left->max);
    derivation(sc->left->max, sent);
    if(sc->right){
      assert(sc->right->max);
      derivation(sc->right->max, sent);
    }
  }
  sc->print_filled(out.stream, cats.markedup, cats.relations, sent.words, sent.words,
		   FORMAT & FMT_JULIA_SLOTS);
  //sc->print_filled(out);

  //store the lexical categories for printing
  if(!sc->left)
    sent.cats.push_back(sc->cat);
}

void
DepsPrinter::lexical(Sentence &sent){
  const ulong NWORDS = sent.words.size();

  if((FORMAT & FMT_LEMMA) && sent.lemmas.size() != NWORDS)
    throw NLP::Exception("not enough lemmas for the sentence");

  // don't need to check POS tags since they must already exist

  if((FORMAT & FMT_CHUNK) && sent.chunks.size() != NWORDS)
    throw NLP::Exception("not enough chunks for the sentence");

  if((FORMAT & FMT_NER) && sent.entities.size() != NWORDS)
    throw NLP::Exception("not enough named entities for the sentence");

  const bool HAS_CORRECT_STAG = sent.cats.size();

  if(FORMAT & FMT_WORDS){
    out.stream << "<c>";
    for(ulong i = 0; i < NWORDS; ++i){
      out.stream << ' ' << sent.words[i];
      if(FORMAT & FMT_LEMMA)
	out.stream << '|' << sent.lemmas[i];
      if(FORMAT & FMT_POS)
	out.stream << '|' << sent.pos[i];
      if(FORMAT & FMT_CHUNK)
	out.stream << '|' << sent.chunks[i];
      if(FORMAT & FMT_NER)
	out.stream << '|' << sent.entities[i];
      if(FORMAT & FMT_SUPER){
        out.stream << '|';
        if(HAS_CORRECT_STAG)
          sent.cats[i]->out_novar_noX(out.stream, false);
        else
          out.stream << sent.msuper[i][0].raw;
      }
      if(FORMAT & FMT_CAT){
        out.stream << '|';
        if(HAS_CORRECT_STAG)
          sent.cats[i]->out(out.stream);
        else
          out.stream << "none";
      }
    }
    out.stream << '\n';
  }
}

} }
