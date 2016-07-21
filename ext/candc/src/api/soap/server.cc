// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

const char *PROGRAM_NAME = "soap_server";

#include "candc.h"

using namespace NLP;
using namespace std;

#include "soapH.h"
#include "candc_binding.nsmap"

// global parser object required for soap bindings
NLP::CandC *candc = 0;
IO::Log *server_log = 0;

int
ns1__parse_USCOREstring(struct soap *soap, std::string _text, bool start, std::string &_result){
  IO::Log &log = *server_log;

  if(_text[_text.size() - 1] != '\n')
    _text += '\n';

  log.stream << "read " << _text.size() << " characters of input" << endl;
  try {
    istringstream input(_text);
    ostringstream output;

    IO::Input in("<soap input>", input);
    IO::Output out("<soap output>", output);

    candc->parse(in, out, log, start, "");

    _result = output.str();
  }catch(NLP::IOException e){
    log.stream << "candc:ioerror: " << e.msg << ':'
	       << e.uri << ':' << e.line << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }catch(NLP::ParseError e){
    log.stream << "candc:parse error: " << e.msg << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }catch(NLP::Exception e){
    log.stream << "candc:error: " << e.msg << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }
  log.stream << "done" << endl;

  return SOAP_OK;
}

int
ns1__parse_USCOREsentence(struct soap *soap, std::string _text, std::string _printer, std::string &_result){
  IO::Log &log = *server_log;

  if(_text[_text.size() - 1] != '\n')
    _text += '\n';

  log.stream << "read " << _text.size() << " characters of input" << endl;
  try {
    istringstream input(_text);
    ostringstream output;

    IO::Input in("<soap input>", input);
    IO::Output out("<soap output>", output);

    candc->parse(in, out, log, true, _printer);

    _result = output.str();
  }catch(NLP::IOException e){
    log.stream << "candc:ioerror: " << e.msg << ':'
	       << e.uri << ':' << e.line << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }catch(NLP::ParseError e){
    log.stream << "candc:parse error: " << e.msg << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }catch(NLP::Exception e){
    log.stream << "candc:error: " << e.msg << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }
  log.stream << "done" << endl;

  return SOAP_OK;
}

int
ns1__parse_USCOREoracle(struct soap *soap, std::string _text, std::string _constraints,
                        std::string _printer, std::string &_result){
	IO::Log &log = *server_log;

  if(_text[_text.size() - 1] != '\n')
    _text += '\n';

  log.stream << "read " << _text.size() << " characters of input" << endl;
  try {
    istringstream input(_text);
    istringstream constraints(_constraints);
    ostringstream output;

		IO::Input in("<soap input>", input);
		IO::Input cons("<soap constraints>", constraints);
		IO::Output out("<soap output>", output);

    candc->oracle(in, cons, out, log, true, _printer);

    _result = output.str();
  }catch(NLP::IOException e){
    log.stream << "candc:ioerror: " << e.msg << ':'
               << e.uri << ':' << e.line << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }catch(NLP::ParseError e){
    log.stream << "candc:parse error: " << e.msg << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }catch(NLP::Exception e){
    log.stream << "candc:error: " << e.msg << endl;
    return soap_receiver_fault(soap, "ccg exception", e.what());
  }
  log.stream << "done" << endl;

  return SOAP_OK;
}

int
run_server(const std::string &host, const std::string &log_file){
  IO::Log log(log_file);
  server_log = &log;

  char *HOST = strdup(host.c_str());
  char *port = strchr(HOST, ':');
  if(!port)
    throw NLP::ConfigError("server must be specified by a host:port pair", "server");
  *port++ = '\0';

  if(!*HOST)
    HOST = strdup("localhost");

  int PORT = atoi(port);
  if(PORT < 1024)
    throw NLP::ConfigError("server port must be an integer >= 1024", "server");

  struct soap soap;
  soap_init(&soap);
  soap.socket_flags = Port::SOCK_FLAGS;
  soap.bind_flags = Port::BIND_FLAGS;

  if(soap_bind(&soap, HOST, PORT, 100) < 0){
    soap_print_fault(&soap, stderr);
    return 1;
  }
  
  while(1){
    log.stream << "waiting for connections on " << HOST << ':' << PORT << std::endl;
  
    if(soap_accept(&soap) < 0){
      soap_print_fault(&soap, stderr);
      break;
    }else{
      soap_serve(&soap);
      soap_destroy(&soap);
      soap_end(&soap);
    }
  }

  server_log = 0;
  soap_done(&soap);

  return 0;
}

int
run_local(const std::string &in_file, const std::string &out_file, const std::string &log_file){
  IO::Input in(in_file);
  IO::Output out(out_file);
  IO::Log log(log_file);

  log.stream << "# reading text from " << in_file << std::endl;
  log.stream << "# writing to " << out_file << std::endl;
  log.stream << "# writing log to " << log_file << std::endl;

  candc->parse(in, out, log);

  return 0;
}

int
run(int argc, char **argv){
  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);
  CandC::Config candc_cfg;

  Config::Alias alias_candc(cfg, SPACE, candc_cfg, "models", "candc");
  Op<std::string> server(cfg, "server", "run the C&C tools as a server on host:port");

  Config::Op<std::string> log_file(cfg, SPACE, "log", "the log file to write to", STDERR);

  cfg.reg(candc_cfg, SPACE);

  cfg.parse(argc, argv);
  cfg.check();

  candc = new CandC(candc_cfg, PREFACE.str());

  return run_server(server(), log_file());
}

#include "main.h"
