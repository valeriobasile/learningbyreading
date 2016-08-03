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
#include "parser/print_boxer.h"

using namespace std;

namespace NLP { namespace CCG {

// replace the # comment symbol with % for Prolog
static std::string
prolog_comment(const std::string comment){
  std::string res = comment;
  bool newline = true;
  for(std::string::iterator s = res.begin(); s != res.end(); ++s){
    if(*s == '\n')
      newline = true;
    else{
      if(*s == '#' && newline)
	*s = '%';
      newline = false;
    }
  }

  return res;
}


static std::string
escape(const std::string &word){
  string result;
  for(string::const_iterator i = word.begin(); i != word.end(); ++i)
    switch(*i){
      case '\\': /* fall through */
      case '\"': /* fall through */
      case '\'': result += '\\'; /* fall through */
      default: result += *i;
    }
  return result;
}

typedef StreamPrinter SP;

static
void
check_format(const Sentence &sent, const SP::Format FORMAT){
  const ulong NWORDS = sent.words.size();

  if((FORMAT & SP::FMT_LEMMA) && sent.lemmas.size() != NWORDS)
    throw NLP::Exception("not enough lemmas for the sentence");

  // don't need to check POS tags since they must already exist

  if((FORMAT & SP::FMT_CHUNK) && sent.chunks.size() != NWORDS)
    throw NLP::Exception("not enough chunks for the sentence");

  if((FORMAT & SP::FMT_NER) && sent.entities.size() != NWORDS)
    throw NLP::Exception("not enough named entities for the sentence");
}

void
BoxerPrinter::header(const std::string &PREFACE){
  log.stream << PREFACE << endl;
  out.stream << prolog_comment(PREFACE) << '\n';
  out.stream << ":- op(601, xfx, (/)).\n";
  out.stream << ":- op(601, xfx, (\\)).\n";

  out.stream << ":- multifile ccg/2, id/2.\n";
  out.stream << ":- discontiguous ccg/2, id/2.\n";
  out.stream << '\n';
}

void
BoxerPrinter::leaf(const SuperCat *sc, Feature parent, const Sentence &sent, ulong i){
  out.stream << "t(";

  if(sc)
    sc->cat->out_boxer(out.stream, parent);
  else
    out.stream << "np"; // hack for unary sentences

  out.stream << ", '" << escape(sent.words[i]);

  if(FORMAT & FMT_LEMMA)
    out.stream << "', '" << escape(sent.lemmas[i]);
  if(FORMAT & FMT_POS)
    out.stream << "', '" << sent.pos[i];
  if(FORMAT & FMT_CHUNK)
    out.stream << "', '" << sent.chunks[i];
  if(FORMAT & FMT_NER)
    out.stream << "', '" << sent.entities[i];

  out.stream << "')";
}

void
BoxerPrinter::unary(Sentence &sent){
  check_format(sent, FORMAT);

  out.stream << "ccg(" << nsentences << ',';
  if(FORMAT & FMT_WS)
    out.stream << "\n  ";
  leaf(0, Features::NONE, sent, 0);
  out.stream << ").\n\n";
}

void
BoxerPrinter::recurse(const SuperCat *sc, Feature parent, Sentence &sent, int depth){
  if(FORMAT & FMT_WS){
    for(int i = 0; i < depth; ++i)
      out.stream << ' ';
  }

  if(sc->left){
    assert(sc->left->max);
    sc->out_boxer(out.stream, parent);
    if(FORMAT & FMT_WS)
      out.stream << '\n';

    recurse(sc->left->max, sc->feature.override(parent), sent, depth + 1);

    if(sc->right){
      out.stream << ',';
      if(FORMAT & FMT_WS)
	out.stream << '\n';
      assert(sc->right->max);
      recurse(sc->right->max, sc->feature.override(parent), sent, depth + 1);
      out.stream << ")";
    }else
      out.stream << ")";
  }else{
    Position pos = (sc->vars[sc->cat->var]).pos();
    leaf(sc, parent, sent, pos - 1);
  }
}

void
BoxerPrinter::derivation(const SuperCat *sc, Sentence &sent){
  check_format(sent, FORMAT);

  out.stream << "ccg(" << nsentences << ',';
  if(FORMAT & FMT_WS)
    out.stream << '\n';
  recurse(sc, Features::NONE, sent, 1);
  out.stream << ").\n";
}

} }
