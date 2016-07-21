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
  Cluster::init(argc, argv, true);

  try {
    int retcode = run(argc, argv);
    Cluster::finalize();
    return retcode;
  }catch(NLP::ConfigError &e){
    std::cerr << PROGRAM_NAME << ':';
    std::cerr << e.option << ':' << e.msg;
    if(e.uri != ""){
      std::cerr << ':' << e.uri;
      if(e.line)
	std::cerr << ':' << e.line;
    }
  }catch(NLP::IOException &e){
    std::cerr << PROGRAM_NAME << ':';
    std::cerr << e.msg << ':' << e.uri;
    if(e.line)
      std::cerr << ':' << e.line;
  }catch(NLP::Exception &e){
    std::cerr << PROGRAM_NAME << ':';
    std::cerr << e.msg;
  }
  std::cerr << std::endl;
  Cluster::finalize();

  return 1;
}
