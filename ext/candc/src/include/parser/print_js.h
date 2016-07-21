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

using namespace std;

namespace NLP {
  namespace CCG {

    class JSPrinter: public StreamPrinter {
    protected:
      void recurse(const SuperCat *sc, Sentence &sent, int depth, string &parent);
      void leaf(Sentence &sent, ulong i, string &parent, const Cat *cat = 0);
      virtual void unary(Sentence &sent);
      virtual void derivation(const SuperCat *sc, Sentence &sent);
      unsigned int nodeCount;
    public:
      JSPrinter(Categories &cats, const Format FORMAT, IO::Output &out, IO::Log &log): StreamPrinter(cats, FORMAT, out, log){ nodeCount = 0; }
      virtual ~JSPrinter(void){ /* do nothing */ }
      virtual void header(const std::string &PREFACE);
      virtual void footer(void);
    };

  }
}
