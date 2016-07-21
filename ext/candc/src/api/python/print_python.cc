// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <boost/python.hpp>

#include "parser/_printer.h"

using namespace std;

#include "boost.h"

#include "print_python.h"

namespace NLP { namespace CCG {

std::string cat2str(const Cat *cat){
  ostringstream out;
  cat->out_novar_noX(out, false);
  return out.str();
}

void
PythonPrinter::unary(Sentence &sent){
  deps = py::list();
  deriv = py::make_tuple(sent.msuper[0][0].raw, "lex", sent.words[0], sent.pos[0]);
}

py::tuple
PythonPrinter::recurse(const SuperCat *sc, Sentence &sent){
  py::tuple result;

  if(sc->left){
    assert(sc->left->max);
    py::tuple left_deriv = recurse(sc->left->max, sent);
    if(sc->right){
      assert(sc->right->max);
      py::tuple right_deriv = recurse(sc->right->max, sent);
      result = py::make_tuple(cat2str(sc->cat), sc->flags2str(), left_deriv, right_deriv);
    }else
      result = py::make_tuple(cat2str(sc->cat), sc->flags2str(), left_deriv);
  }else{
    Position pos = (sc->vars[sc->cat->var]).pos();
    result = py::make_tuple(cat2str(sc->cat), "lex", sent.words[pos - 1], sent.pos[pos - 1]);
  }

  if(!sc->left)
    sent.cats.push_back(sc->cat);

  sc->get_grs(grs_, cats.relations, filled, sent);
  sc->get_filled(deps_, cats.relations, sent, true);

  return result;
}

static object
arg2obj(const Argument &arg){
  if(arg.pos >= 0)
    return make_tuple(arg.raw, arg.pos);
  else
    return str(arg.raw);
}

static py::tuple
gr2tuple(const GR &gr){
  // make_tuple calls need to be hard coded
  switch(gr.args.size()){
  case 0: return py::make_tuple(gr.label);
  case 1: return py::make_tuple(gr.label, arg2obj(gr.args[0]));
  case 2: return py::make_tuple(gr.label, arg2obj(gr.args[0]), arg2obj(gr.args[1]));
  case 3: return py::make_tuple(gr.label, arg2obj(gr.args[0]),
                                arg2obj(gr.args[1]), arg2obj(gr.args[2]));
  default:
    throw NLP::Exception("could not convert GR " + gr.label + " to a tuple");
  }
}

void
PythonPrinter::derivation(const SuperCat *sc, Sentence &sent){
  sent.cats.clear();
  deriv = recurse(sc, sent);

  grs = py::list();
  for(GRs::const_iterator i = grs_.begin(); i != grs_.end(); ++i)
    grs.append(gr2tuple(*i));

  deps = py::list();
  for(GRs::const_iterator i = deps_.begin(); i != deps_.end(); ++i)
    deps.append(gr2tuple(*i));

  grs_.clear();
  deps_.clear();
  filled.clear();
}

} }
