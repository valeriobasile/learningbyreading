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

namespace NLP { namespace CCG {

set<ulong> rejects;

void
load_rejects(const std::string &filename){
  ifstream in(filename.c_str());
  if(!in)
    throw NLP::IOException("could not open rejects file for reading", filename);

  ulong sentence;
  while(in >> sentence)
    rejects.insert(sentence);
}

} }
