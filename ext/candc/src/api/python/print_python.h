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
  namespace CCG {

    namespace py = boost::python;

    class PythonPrinter: public Printer {
    protected:
      virtual void unary(Sentence &sent);
      virtual void derivation(const SuperCat *sc, Sentence &sent);
      virtual void lexical(Sentence &){}
      virtual py::tuple recurse(const SuperCat *sc, Sentence &sent);

      FilledDeps filled;
      GRs grs_;
      GRs deps_;
    public:

      py::tuple deriv;
      py::list deps;
      py::list grs;

      PythonPrinter(Categories &cats)
	: Printer(cats){}

      virtual ~PythonPrinter(void){ /* do nothing */ }

      virtual void preface(const std::string &){}
    };

  }
}
