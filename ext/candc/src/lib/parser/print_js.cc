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
#include "parser/print_js.h"

using namespace std;

namespace NLP { namespace CCG {

void JSPrinter::header(const string &){}
void JSPrinter::footer(void){}

static string escape(const string &word){
  string result = "";
  for(string::const_iterator i = word.begin(); i != word.end(); ++i)
    switch(*i) {
      case '\"':
        result += "\\\"";
	      break;
	    case '\\':
        result += "\\\\";
        break;
      default:
	      result += *i;
    }
  return result;
}

void JSPrinter::unary(Sentence &sent){
  out.stream << "tree = new ParseTree;\n";
  string rootNode = "tree.rootNode";
  leaf(sent, 0, rootNode);
  out.stream << "treeView = new TreeView(tree);";
}

void JSPrinter::recurse(const SuperCat *sc, Sentence &sent, int depth, string &parent){
  if(sc->left){
    // there's a better way to do this... just need to know more c++ :)
    nodeCount++;
    stringstream nodeName;
    nodeName << "node" << nodeCount;
    string node = nodeName.str();
    
    out.stream << node << " = new ParseNode(" << parent << ", {type: \"";
    out.stream << sc->flags2str() << "\", cat:\"";
    stringstream category;
    sc->cat->out_novar_noX(category, false);
    out.stream << escape(category.str());
    out.stream << "\"});\n";
    recurse(sc->left->max, sent, depth + 1, node);
    if(sc->right){
      recurse(sc->right->max, sent, depth + 1, node);
    }    
  } else {
    // leaf case
    sent.cats.push_back(sc->cat);
    Position pos = (sc->vars[sc->cat->var]).pos();
    leaf(sent, pos - 1, parent, sc->cat);
  }
}

void JSPrinter::derivation(const SuperCat *sc, Sentence &sent){
  out.stream << "tree = new ParseTree;";
  if(FORMAT & FMT_WS)
    out.stream << '\n';
  string rootNode = "tree.rootNode";
  recurse(sc, sent, 1, rootNode);
  out.stream << "treeView = new TreeView(tree);";
}

void JSPrinter::leaf(Sentence &sent, ulong i, string &parent, const Cat *cat){
  nodeCount++;
  stringstream nodeName;
  nodeName << "node" << nodeCount;
  out.stream << nodeName.str() << " = new ParseNode(" << parent << ", {span: 1, ";
  out.stream << "word: \"" << escape(sent.words[i]) << '\"';

  if(FORMAT & FMT_LEMMA)
    out.stream << ", lemma:\"" << escape(sent.lemmas[i]) << '\"';
  if(FORMAT & FMT_POS)
    out.stream << ", pos:\"" << sent.pos[i] << '\"';
  if(FORMAT & FMT_CHUNK)
    out.stream << ", chunk:\"" << sent.chunks[i] << '\"';
  if(FORMAT & FMT_NER)
    out.stream << ", entity:\"" << sent.entities[i] << '\"';

  if(FORMAT & FMT_SUPER){
    out.stream << ", cat:\"";
    if(cat) {
      stringstream category;
      cat->out_novar_noX(category, false);
      out.stream << escape(category.str());
    } else {
      out.stream << escape(sent.msuper[i][0].raw);
    }
    out.stream << '\"';
  }
  out.stream << "});\n";
}

} }
