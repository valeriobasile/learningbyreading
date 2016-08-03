// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "utils.h"

#include "pool.h"

#include "parser/treebank.h"

namespace NLP { namespace CCG {

using namespace std;

TBNode::TBNode(const std::string &line){
  istringstream input(line);
  string tmp;
  input >> tmp;
  if(tmp == "(<T")
    type = INTERNAL;
  else if(tmp == "(<L")
    type = LEAF;
  else
    throw NLP::Exception("expected internal or leaf node in " + line);

  input >> tmp; // ignore *** or *H*
  if(tmp[0] != '*')
    throw NLP::Exception("expected *** or *H* thingy in " + line);

  input >> cat;
  catNoBrack = cat;

  bool conj = false;
  if(cat.size() > 6 && cat.compare(cat.size() - 6, 6, "[conj]") == 0){
    conj = true;
    cat =  cat.substr(0, cat.size() - 6);
  }

  for(ulong i = 0; i < cat.size(); ++i)
    if(cat[i] == '\\' || cat[i] == '/'){
      cat = '(' + cat + ')';
      break;
    }

  if(conj)
    cat = "(" + cat + "\\" + cat + ")";
  input >> pos;

  if(type == INTERNAL){
    input >> tmp; // ignore [0 or 1]
    if(!isdigit(tmp[0]))
      throw NLP::Exception("expected digit in internal node " + line);

    input >> nchildren;
  }else if(type == LEAF){
    input >> word;
    word.resize(word.size() - 1);
  }else
    assert(!"expected type to be LEAF or INTERNAL");
}

} }
