// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "utils.h"
#include "thesaurus/libraries.h"

bool Libraries::loadlib(char *filename) {
  void *handle = dlopen(filename, RTLD_LAZY|RTLD_GLOBAL);
  if(handle == NULL){
    file_msg(filename, "error: %s importing %s", dlerror(), filename);
    return false;
  }
  push_back(handle);
  return true;
}

bool Libraries::loadfile(char *filename) {
  ReadBuffer buffer(filename, 1024);
  while(buffer.read()){
    strchomp(buffer.data());
    if(!loadlib(buffer.data()))
      return false;
  }
  return true;
}

Libraries::~Libraries(void){
  for(iterator i = begin(); i != end(); i++)
    dlclose(*i);
}
