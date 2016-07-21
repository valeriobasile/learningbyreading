// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

const char *PROGRAM_NAME = "soap_client";

#include "base.h"

#include "config/config.h"

using namespace std;
using namespace NLP;
using namespace NLP::Config;

#include "soapStub.h"
#include "candc_binding.nsmap"

std::string
call(struct soap &soap, const std::string &URL, std::ostringstream &input){
  static bool first = true;

  std::string INPUT = input.str();
  std::string OUTPUT;

  if(!INPUT.size())
    return "";

  if(soap_call_ns1__parse_USCOREstring(&soap, URL.c_str(), "", INPUT, first, OUTPUT) != SOAP_OK){
    soap_print_fault(&soap, stderr);
    soap_end(&soap);
    exit(1);
  }
  first = false;
  
  input.str("");

  return OUTPUT;
}

int
run(int argc, char **argv){
  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);
  Config::Op<std::string> url(cfg, SPACE, "url", "the URL of the C&C SOAP server (e.g. http://localhost:9000)");

  Config::Op<std::string> input(cfg, SPACE, "input", "the input file to read from", IO::STDIN);
  Config::Op<std::string> output(cfg, "output", "the output file to write to", IO::STDOUT);

  Config::Op<ulong> maxlines(cfg, SPACE, "maxlines", "the max number of input lines to send at a time", 500); 

  cfg.parse(argc, argv);
  cfg.check();

  IO::Input in(input());
  IO::Output out(output());

  struct soap soap;
  soap_init(&soap); // need to initialize only once

  ostringstream s;
  std::string INPUT;
  std::string OUTPUT;

  ulong nlines = 0;
  while(getline(in.stream, INPUT)){
    ++nlines;

    s << INPUT << '\n';
    if(nlines == maxlines()){
      out.stream << call(soap, url(), s);
      nlines = 0;
    }
  }

  out.stream << call(soap, url(), s);

  soap_end(&soap);

  return 0;
}

#include "main.h"
