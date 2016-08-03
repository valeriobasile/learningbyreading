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
#include "parser/print_prolog.h"

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

void
PrologPrinter::header(const std::string &PREFACE){
  log.stream << PREFACE << endl;
  out.stream << prolog_comment(PREFACE) << endl;

  if(FORMAT & FMT_WORDS){
    ulong arity = 3;
    if(FORMAT & FMT_LEMMA) ++arity;
    if(FORMAT & FMT_POS) ++arity;
    if(FORMAT & FMT_CHUNK) ++arity;
    if(FORMAT & FMT_NER) ++arity;
    if(FORMAT & FMT_SUPER) ++arity;

    out.stream << ":- multifile w/" << arity << ", ccg/2, id/2.\n";
    out.stream << ":- discontiguous w/" << arity << ", ccg/2, id/2.\n";
    out.stream << ":- dynamic w/" << arity << ", ccg/2, id/2.\n";
  }else{
    out.stream << ":- multifile ccg/2, id/2.\n";
    out.stream << ":- discontiguous ccg/2, id/2.\n";
    out.stream << ":- dynamic ccg/2, id/2.\n";
  }
  out.stream << endl;
}

void
PrologPrinter::unary(Sentence &){
  out.stream << "ccg(" << nsentences << ',';
  if(FORMAT & FMT_WS)
    out.stream << '\n';
  out.stream << "  lf(" << nsentences << ", 1, 'NP')";
  out.stream << ").\n\n";
}

void
PrologPrinter::recurse(const SuperCat *sc, Sentence &sent, int depth){
  if(FORMAT & FMT_WS){
    for(int i = 0; i < depth; ++i)
      out.stream << ' ';
  }

  if(sc->left){
    assert(sc->left->max);
    out.stream << sc->flags2str() << '(';
    sc->lex_info(out.stream);         // provides information about lexical rules
    sc->conj_info(out.stream);        // provides information about conj rules
    out.stream << '\'';
    sc->cat->out_novar(out.stream, false);
    out.stream << "\',";
    if(FORMAT & FMT_WS)
      out.stream << '\n';

    recurse(sc->left->max, sent, depth + 1);

    if(sc->right){
      out.stream << ',';
      if(FORMAT & FMT_WS)
	out.stream << '\n';
      assert(sc->right->max);
      recurse(sc->right->max, sent, depth + 1);
      out.stream << ")";
    }else
      out.stream << ")";
  }else{
    // leaf case
    Position pos = (sc->vars[sc->cat->var]).pos();
    out.stream << "lf(" << nsentences << ',' << (ulong)pos << ",\'"; 
    sc->cat->out_novar(out.stream, false);
    out.stream << "\')";

    sent.cats.push_back(sc->cat);
  }
}

void
PrologPrinter::derivation(const SuperCat *sc, Sentence &sent){
  out.stream << "ccg(" << nsentences << ',';
  if(FORMAT & FMT_WS)
    out.stream << '\n';
  recurse(sc, sent, 1);
  out.stream << ").\n\n";
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

void
PrologPrinter::lexical(Sentence &sent){
  const ulong NWORDS = sent.words.size();

  if(!(FORMAT & FMT_WORDS))
    return;

  if((FORMAT & FMT_LEMMA) && sent.lemmas.size() != NWORDS)
    throw NLP::Exception("not enough lemmas for the sentence");

  // don't need to check POS tags since they must already exist

  if((FORMAT & FMT_CHUNK) && sent.chunks.size() != NWORDS)
    throw NLP::Exception("not enough chunks for the sentence");

  if((FORMAT & FMT_NER) && sent.entities.size() != NWORDS)
    throw NLP::Exception("not enough named entities for the sentence");

  const bool HAS_CORRECT_STAG = sent.cats.size();

  for(ulong i = 0; i < NWORDS; ++i){
    out.stream << "w(" << nsentences << ", " << i + 1 << ", '" << escape(sent.words[i]);
    if(FORMAT & FMT_LEMMA)
      out.stream << "', '" << escape(sent.lemmas[i]);
    if(FORMAT & FMT_POS)
      out.stream << "', '" << sent.pos[i];
    if(FORMAT & FMT_CHUNK)
      out.stream << "', '" << sent.chunks[i];
    if(FORMAT & FMT_NER)
      out.stream << "', '" << sent.entities[i];

    const Cat *cat = 0;
    if(HAS_CORRECT_STAG)
      cat = sent.cats[i];
    else
      cat = cats.markedup[sent.msuper[i][0].raw];

    if(FORMAT & FMT_SUPER){
      out.stream << "', '";
      cat->out_novar(out.stream, false);
      out.stream << "').\n";
    }else{
      out.stream << "'). % ";
      cat->out_novar(out.stream, false);
      out.stream << '\n';
    }
  }
}

} }
