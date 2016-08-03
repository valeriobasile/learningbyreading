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
#include "parser/print_xml.h"

using namespace std;

namespace NLP { namespace CCG {

/*
// replace the # comment symbol with XML comment block
static std::string
xml_comment(const std::string comment){
  std::string res = "<!-- \n";
  bool newline = true;

  for(std::string::const_iterator s = comment.begin(); s != comment.end(); ++s){
    if(*s == '\n'){
      newline = true;
      res += '\n';
    }else{
      if(*s == '#' && newline)
	res += "  ";
      else
	res += *s;
      newline = false;
    }
  }

  res += "  -->\n";

  return res;
}
*/

void
XMLPrinter::header(const std::string &PREFACE){
  log.stream << PREFACE << endl;
  out.stream << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  out.stream << "<?xml-stylesheet type=\"text/xsl\" href=\"candc.xml\"?>\n\n";

  //  out.stream << xml_comment(PREFACE) << endl;

  out.stream << "<candc>\n";
}

void
XMLPrinter::footer(void){
  out.stream << "</candc>\n";
}

void
XMLPrinter::unary(Sentence &sent){
  out.stream << "<ccg>";
  if(FORMAT & FMT_WS)
    out.stream << '\n';
  leaf(sent, 0);
  out.stream << "</ccg>\n";
}

void
XMLPrinter::recurse(const SuperCat *sc, Sentence &sent, int depth){
  if(FORMAT & FMT_WS){
    for(int i = 0; i < depth; ++i)
      out.stream << ' ';
  }

  if(sc->left){
    out.stream << "<rule type=\"";
    out.stream << sc->flags2str() << "\" cat=\"";
    //    sc->lex_info(out.stream);         // provides information about lexical rules
    //    sc->conj_info(out.stream);        // provides information about conj rules
    sc->cat->out_novar(out.stream, false);
    out.stream << "\">";
    if(FORMAT & FMT_WS)
      out.stream << '\n';

    recurse(sc->left->max, sent, depth + 1);

    if(sc->right){
      recurse(sc->right->max, sent, depth + 1);
    }
    if(FORMAT & FMT_WS){
      for(int i = 0; i < depth; ++i)
	out.stream << ' ';
    }
    out.stream << "</rule>";
    if(FORMAT & FMT_WS)
      out.stream << '\n';
  }else{
    // leaf case
    sent.cats.push_back(sc->cat);
    Position pos = (sc->vars[sc->cat->var]).pos();
    leaf(sent, pos - 1, sc->cat);
  }
}

void
XMLPrinter::derivation(const SuperCat *sc, Sentence &sent){
  out.stream << "<ccg>";
  if(FORMAT & FMT_WS)
    out.stream << '\n';
  recurse(sc, sent, 1);
	out.stream << "</ccg>\n";
	out.stream << "<deps>\n<![CDATA[\n" << deps.str() << "]]></deps>";
}

static std::string
escape(const std::string &word){
  string result = "";
  for(string::const_iterator i = word.begin(); i != word.end(); ++i)
    switch(*i){
      case '&':
        result += "&amp;";
	break;
      case '<':
        result += "&lt;";
	break;
      case '>':
        result += "&gt;";
        break;
      case '\"':
        result += "&quot;";
	break;
      default:
	result += *i;
    }
  return result;
}

void
XMLPrinter::leaf(Sentence &sent, ulong i, const Cat *cat){
  out.stream << "<lf start=\"" << i << "\" span=\"1\"";
  out.stream << " word=\"" << escape(sent.words[i]) << '\"';

  if(FORMAT & FMT_LEMMA)
    out.stream << " lemma=\"" << escape(sent.lemmas[i]) << '\"';
  if(FORMAT & FMT_POS)
    out.stream << " pos=\"" << sent.pos[i] << '\"';
  if(FORMAT & FMT_CHUNK)
    out.stream << " chunk=\"" << sent.chunks[i] << '\"';
  if(FORMAT & FMT_NER)
    out.stream << " entity=\"" << sent.entities[i] << '\"';

  if(FORMAT & FMT_SUPER){
    out.stream << " cat=\"";
    if(cat)
      cat->out_novar_noX(out.stream, false);
    else
      out.stream << sent.msuper[i][0].raw;
    out.stream << '\"';
  }
  out.stream << " />\n";
}

void
XMLPrinter::error(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	out.stream << "<error beta=\"" << BETA << "\" dictcutoff=\""
						 << DICT_CUTOFF << "\">" << REASON << "</error>";
	StreamPrinter::error(REASON, sent, BETA, DICT_CUTOFF);
}

void
XMLPrinter::failed(const std::string &REASON, Sentence &sent, double BETA, ulong DICT_CUTOFF){
	out.stream << "<failed beta=\"" << BETA << "\" dictcutoff=\""
						 << DICT_CUTOFF << "\">" << REASON << "</failed>";
	StreamPrinter::failed(REASON, sent, BETA, DICT_CUTOFF);
}

} }
