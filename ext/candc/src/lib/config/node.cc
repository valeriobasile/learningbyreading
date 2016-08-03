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

#include "config/node.h"

using namespace std;

namespace NLP { namespace Config {

void
Node::write_help(const std::string &filename, const std::string &PREFACE, bool full) const {
  ofstream out(filename.c_str());
  if(!out)
    throw NLP::IOException("could not open " + NAME + " file for writing help", filename);

  out << PREFACE << '\n';
  write_help(out, "", full);
}

void
Node::write_config(const std::string &filename, const std::string &PREFACE, bool root) const {
  ofstream out(filename.c_str());
  if(!out)
    throw NLP::IOException("could not open " + NAME + " file for writing config", filename);

  out << PREFACE << '\n';
  write_config(out, "", root);
}

void
Node::write_preface(const std::string &filename, const std::string &PREFACE, bool root) const {
  ofstream out(filename.c_str());
  if(!out)
    throw NLP::IOException("could not open " + NAME + " file for writing preface", filename);

  out << PREFACE << '\n';
  write_preface(out, "", root);
}

} }
