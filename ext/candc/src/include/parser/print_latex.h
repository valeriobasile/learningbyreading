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

    class LatexPrinter : public StreamPrinter {
    protected:
      class LatexCell;
      LatexCell *cells;
      unsigned int cells_width;
      unsigned int cells_height;

      LatexCell *get_cell(unsigned int row, unsigned int col);
      void output_table(Sentence &sent);
      unsigned int find_height(const SuperCat *sc);

      void recurse(const SuperCat *sc, Sentence &sent, unsigned int depth, unsigned int &start, unsigned int &end);

      virtual void unary(Sentence &);
      virtual void derivation(const SuperCat *sc, Sentence &sent);

    public:
      LatexPrinter(Categories &cats, const Format FORMAT, IO::Output &out, IO::Log &log) : StreamPrinter(cats, FORMAT, out, log) { }
      virtual ~LatexPrinter(void) { /* do nothing */ }

      virtual void header(const std::string &PREFACE);
      virtual void footer(void);
    };

  }
}
