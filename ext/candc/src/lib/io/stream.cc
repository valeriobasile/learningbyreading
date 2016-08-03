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

using namespace std;

namespace NLP { namespace IO {

const string STDIN = "<stdin>";
const string STDOUT = "<stdout>";
const string STDERR = "<stderr>";

Input::Input(const std::string &uri)
  : uri(uri), stream(uri == STDIN ? cin : fstream){
  if(uri != STDIN){
    fstream.open(uri.c_str());
    if(!fstream)
      NLP::IOException("could not open input file", uri);
  }
}

Output::Output(const std::string &uri)
  : uri(uri), stream(uri == STDOUT ? cout : fstream){
  if(uri != STDOUT){
    fstream.open(uri.c_str());
    if(!fstream)
      NLP::IOException("could not open output file", uri);
  }
}

Log::Log(const std::string &uri)
  : uri(uri), stream(uri == STDERR ? cerr : fstream){
  if(uri != STDERR){
    fstream.open(uri.c_str());
    if(!fstream)
      NLP::IOException("could not open output file", uri);
  }
}

} }
