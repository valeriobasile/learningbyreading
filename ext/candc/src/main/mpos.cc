// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

const char *PROGRAM_NAME = "mpos";

#include "tagger/main.h"
#include "main.h"

#include "tagger/pos.h"

int
run(int argc, char **argv){
  return run_tag<Taggers::POS>(true, 5, 0.1, "%w \n", "%w\t%P\n\n\n", argc, argv);
}
