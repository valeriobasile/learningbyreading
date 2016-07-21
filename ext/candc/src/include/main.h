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


int run(int argc, char **argv);

int main(int argc, char **argv){
  try {
    return run(argc, argv);
  }catch(NLP::ConfigError &e){
    std::cerr << NLP::Port::BOLD << NLP::Port::RED << PROGRAM_NAME << ':';
    std::cerr << e.option << ':' << e.msg;
    if(e.uri != ""){
      std::cerr << ':' << e.uri;
      if(e.line)
	std::cerr << ':' << e.line;
    }
  }catch(NLP::IOException &e){
    std::cerr << NLP::Port::BOLD << NLP::Port::RED << PROGRAM_NAME << ':';
    std::cerr << e.msg << ':' << e.uri;
    if(e.line)
      std::cerr << ':' << e.line;
  }catch(NLP::Exception &e){
    std::cerr << NLP::Port::BOLD << NLP::Port::RED << PROGRAM_NAME << ':';
    std::cerr << e.msg;
  }
  std::cerr << NLP::Port::OFF << std::endl;
  return 1;
}
