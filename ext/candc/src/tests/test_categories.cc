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

#include "config/config.h"

#include "pool.h"

#include "parser/parser.h"

const char *PROGRAM_NAME = "test_categories";

using namespace std;
using namespace NLP;
using namespace NLP::CCG;

int
run(int argc, char** argv){
  if(argc != 3){
    cerr << "usage: test_categories <cats> <markedup>" << endl;
    return 1;
  }

  const std::string CATS = argv[1];
  const std::string MARKEDUP = argv[2];

  Categories cats(CATS, MARKEDUP);

  return 0;
}

#include "main.h"
