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
#include "parser/print_latex.h"

using namespace std;

namespace NLP { namespace CCG {


class LatexPrinter::LatexCell {
public:
  static const unsigned int EMPTY   = 0;
  static const unsigned int LEADING = 1;
  static const unsigned int FILLED  = 2;

  unsigned int state;
  unsigned int span;
  const SuperCat *sc;

  LatexCell(void) : state(LatexCell::EMPTY), span(1), sc(NULL) { }
  ~LatexCell(void) { /* do nothing */ }

  void update(unsigned int _state, unsigned int _span, const SuperCat *_sc) {
    state = _state;
    span  = _span;
    sc    = _sc;
  }
};


inline
LatexPrinter::LatexCell *
LatexPrinter::get_cell(unsigned int row, unsigned int col) {
  return cells + row*cells_width + col;
}

void
LatexPrinter::header(const std::string &PREFACE) {
  log.stream << PREFACE << endl;
}


void
LatexPrinter::footer(void) { }


void
LatexPrinter::unary(Sentence &) {
  out.stream << "Error: Do not know how to handle a call to unary(Sentence &)\n";
}


void
LatexPrinter::recurse(const SuperCat *sc, Sentence &sent, unsigned int depth, unsigned int &start, unsigned int &end) {
  if (sc->left) {
    // process the left and right of the tree
    unsigned int ls, le, rs, re;
    recurse(sc->left->max, sent, depth + 1, ls, le);
    rs = re = le;
    if (sc->right)
      recurse(sc->right->max, sent, depth + 1, rs, re);
    // propogate back up our start and end coverage points back up the recusion stack
    start = ls;
    end   = re;
    // update the cells table
    get_cell(depth, start)->update(LatexCell::LEADING, end - start, sc);
    for (unsigned int i = start + 1; i < end; ++i)
      get_cell(depth, i)->state = LatexCell::FILLED;
  }
  else {
    // leaf node
    sent.cats.push_back(sc->cat);
    Position pos = (sc->vars[sc->cat->var]).pos();
    get_cell(cells_height - 1, pos - 1)->update(LatexCell::LEADING, 1, sc);
    start = pos - 1;
    end = start + 1;
  }
}


unsigned int
LatexPrinter::find_height(const SuperCat *sc) {
  if (sc->left) {
    const unsigned int dl = find_height(sc->left->max);
    if (sc->right) {
      const unsigned int dr = find_height(sc->right->max);
      return 1 + ((dl < dr) ? dr : dl);
    }
    return 1 + dl;
  }
  else
    return 1;
}


void
LatexPrinter::derivation(const SuperCat *sc, Sentence &sent) {
  // work out the size of the latex table
  cells_height = find_height(sc);
  cells_width  = sent.words.size();
  cells = new LatexCell[cells_height * cells_width];

  // recursivly compute
  unsigned int start = 0, end = cells_width;
  recurse(sc, sent, 0, start, end);

  // output
  out.stream << "%%";
  for (RawWords::const_iterator it = sent.words.begin(); it != sent.words.end(); ++it)
    out.stream << " " << *it;
  out.stream << std::endl;
  output_table(sent);
  delete[] cells;
}


void
LatexPrinter::output_table(Sentence &sent) {
  out.stream << "\\deriv{" << cells_width << "}{\n";

  // output the tokens
  for (unsigned int col = 0; col != cells_width; ++col) {
    if (col != 0)
      out.stream << " & ";
    out.stream << "\\rm " << sent.words[col];
  }
  out.stream << "\\\\\n";

  // output each derivation
  for (int row = cells_height - 1; row >= 0; --row) {
    // the CCG combinators
    for (unsigned int col = 0; col < cells_width; ) {
      if (col != 0)
        out.stream << " & ";
      if (row == (int)(cells_height - 1)) {
        out.stream << "\\uline{1}";
        col++;
        continue;
      }

      LatexCell *cell = get_cell(row, col);
      if (cell->state == LatexCell::LEADING) {
        if (cell->sc->flags & SuperCat::FWD_APP)
          out.stream << "\\fapply{" << cell->span << "}";
        else if (cell->sc->flags & SuperCat::BWD_APP)
          out.stream << "\\bapply{" << cell->span << "}";
        else if (cell->sc->flags & SuperCat::FWD_COMP)
          out.stream << "\\fcomp{" << cell->span << "}";
        else if (cell->sc->flags & SuperCat::BWD_COMP)
          out.stream << "\\bcomp{" << cell->span << "}";
        else if (cell->sc->flags & SuperCat::BWD_CROSS)
          out.stream << "\\bxcomp{" << cell->span << "}";
        else if (cell->sc->flags & SuperCat::TR) {
          if (cell->sc->cat->is_fwd())
            out.stream << "\\ftype{" << cell->span << "}";
          else
            out.stream << "\\btype{" << cell->span << "}";
        }
        else if (cell->sc->flags & SuperCat::LEX)
          out.stream << "\\uline{" << cell->span << "}";
        else if (cell->sc->flags & SuperCat::CONJ)
          out.stream << "\\conj{" << cell->span << "}";
        else if (cell->sc->flags) // catch all
          out.stream << "\\uline{" << cell->span << "}";
        col += cell->span;
      }
      else
        col += 1;
    }
    out.stream << "\\\\\n";

    // the CCG categories
    for (unsigned int col = 0; col < cells_width; ) {
      if (col != 0)
        out.stream << " & ";
      LatexCell *cell = get_cell(row, col);
      if (cell->state == LatexCell::LEADING) {
        out.stream << "\\mc{" << cell->span << "}{\\cf{";

        // write the category to a temp string so we can escape the \'s in the derivations
        stringstream ss(stringstream::in | stringstream::out);
        cell->sc->cat->out_novar_noX(ss, false);
        string str(ss.str());
        for (string::const_iterator c = str.begin(); c != str.end(); ++c)
          if (*c == '\\')
            out.stream << "\\bs ";
          else
            out.stream << *c;

        out.stream << "}}";
        col += cell->span;
      }
      else
        col += 1;
    }
    out.stream << "\\\\\n";
  }

  out.stream << "}\n";
}

} }
