// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <cmath>
#include <string>
#include <vector>
#include <map>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <numeric>
#include <limits>
#include <algorithm>
#include <stdexcept>

#include <csignal>

using namespace std;

#include "except.h"
#include "utils.h"

#include "thesaurus/options.h"
#include "thesaurus/extractor.h"
#include "thesaurus/answer.h"

using namespace NLP::Thesaurus;

void
handle_quit(int value){
  cerr << "thesaurus: quitting with code " << value << "..." << endl;

  exit(0);
}

void run(Extractor extractor, const Options &op, std::istream &in){
  char buffer[1024];

  while(in.getline(buffer, sizeof(buffer) - 1)){
    istringstream command(buffer);
    answer(extractor, command, cout);
  }
}

int
main(int argc, char **argv){
  signal(SIGQUIT, handle_quit);
  signal(SIGINT, handle_quit);
  signal(SIGPIPE, SIG_IGN);

  try {
    Options op(argc, argv);
    cerr << op;

    Extractor extractor(op);

    if(op.test != ""){
      cerr << "testing thesaurus on " << op.test << endl;
      extractor.thesaurus(op.test, op.print_limit);
    }else if(op.all != ""){
      cerr << "generating " << op.measure_name << " measure thesaurus out to " << op.all << endl;
      extractor.thesaurus(op.all);
    }else{
      if(op.script != ""){
        std::ifstream input(op.script.c_str());
        if(!input){
          perror("thesaurus");
          op.usage("script file '" + op.script + "' could not be opened");
        }
        cerr << "reading from script " << op.script << endl;
        run(extractor, op, input);
      }else{
        cerr << "waiting for stdin" << endl;
        run(extractor, op, cin);
      }
    }
  }catch(NLP::Exception e){
    cerr << "maxent.exception: " << e.what() << endl;
    return 1;
  }catch(NLP::IOException e){
    cerr << "maxent.ioexception: " << e.what() << endl;
    return 1;
  }

  return 0;
}
