// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <string>
#include <iostream>
#include <sstream>

using namespace std;

#include "utils.h"
#include "thesaurus/options.h"

namespace NLP { namespace Thesaurus {

void
Options::_defaults(void){
  measure_name = "Jaccard";
  weightfn_name = "TTestCut";
  meta = false;
  meta_name = "RelWeight";
  heuristic_name = "TTestCutLog";
  verbose = false;
  split = false;
  rank = 0;
  optimize = false;
  heuristic = false;
  heuristic_size = 100;
  heuristic_fmin = 5;
  heuristic_anmax = 10000;
  cutoff_minimum = 5;
  heuristic_verbs = true;
  print_limit = 200; 
}

void
Options::_usage(std::ostream &out) const {
  out << "usage: relations [options] <relationfile>\n";
  out << "where options are one or more of:\n";
  out << "  -a construct a complete thesaurus\n";
  out << "  -s creates a server on a given host or port\n";
  out << "  -f read input from a script file containing thesaurus commands\n";
  out << "  -S specifies split relations files\n";
  out << "  -o save output and log to outprefix.thes outprefix.log\n";
  out << "  -z optimize the representation after weighting\n";
  out << "  -v verbose output\n";
  out << "use the 'h' command to see the available commands\n";
  out << flush;
}

void
Options::usage(void) const {
  _usage(cerr);
  exit(0);
}

void
Options::usage(const std::string &msg) const {
  cerr << "thesaurus: " << msg << endl;
  _usage(cerr);
  exit(1);
}

std::string
Options::_build_cmd_line(int argc, char **argv){
  std::string buffer(argv[0]);
  for(int i = 1; i < argc; i++){
    buffer += ' ';
    buffer += argv[i];
  }

  return buffer;
}

Options::Options(const std::string &filename): command_line("./thesaurus " + filename) {
  _defaults();
  relations_file = filename;
}

Options::Options(int argc, char **argv):
    command_line(_build_cmd_line(argc, argv)){
  _defaults();

  bool print_options = false;

  int c = 0;
  opterr = 0;
  while ((c = getopt(argc, argv, "a:m:s:f:t:o:g:w:W:S:d:zhp:Xx:c:k:j:vq:")) != EOF)
    switch (c) {
      case 'a':
        if(script.size() || serverport.size())
          usage("full thesaurus construction is mutually exclusive with other modes");
        all = optarg;
        break;
      case 'c':
        if(!str2ulong(optarg, cutoff_minimum))
          usage("cutoff minimum argument could not be converted");
        break;
      case 'm':
        measure_name = optarg;
        break;
      case 'w':
        weightfn_name = optarg;
        break;
      case 'W':
        meta = true;
        meta_name = optarg;
        break;
      case 's':
        if(script.size() || all.size())
          usage("scriptmode mode is a mutually exclusive mode");
        serverport = optarg;
        break;
      case 'f':
        if(serverport.size() || all.size())
          usage("server mode is a mutually exclusive mode");
        script = optarg;
        break;
      case 'j':
        if(!str2ulong(optarg, heuristic_anmax))
          usage("canonical maximum cutoff argument could not be converted");
        break;
      case 'k':
        if(!str2ulong(optarg, heuristic_size))
          usage("canonical set size argument could not be converted");
        break;
      case 't':
        test = optarg;
        break;
      case 'S':
        split = true;
        if(!str2ulong(optarg, rank))
          usage("split rank argument could not be converted");
        break;
      case 'v':
        verbose = true;
        break;
      case 'q':
        common_file = optarg;
        break;
      case 'X':
        heuristic = true;
        break;
      case 'x':
        heuristic = true;
        heuristic_name = optarg;
        break;
      case 'z':
        optimize = true;
        break;
      case 'p':
        if(!str2ulong(optarg, print_limit))
          usage("print limit argument could not be converted");
        break;
      case 'h':
        usage();
        break;
      case '?':
        if(optopt == 'a' || optopt == 'm' || optopt == 's' ||
           optopt == 'f' || optopt == 't' || optopt == 'o' || optopt == 'q'){
          string msg = "missing argument for '-";
          msg += optopt;
          msg += "' option";
          usage(msg);
        }else{
          string msg = "unrecognised option '-";
          msg += optopt;
          msg += '\'';
          usage(msg);
        }
        break;
    }

  if(optind < argc - 1)
    usage("too many arguments, expected relation file only");

  if(optind != argc - 1)
    usage("missing relations file argument");

  relations_file = argv[optind];
  if(split){
    attributes_file = relations_file + ".attribs";
    globals_file = relations_file + ".globals";
    if(common_file == "")
      common_file = relations_file + ".common";

    ostringstream tmp;
    tmp << relations_file << '.' << rank;
    relations_file = tmp.str();
  }

  if(print_options)
    to_stream(cerr);
}

void
Options::to_stream(std::ostream &out) const {
  out << command_line << endl;

  out << "thesaurus.weightfn_name = " <<  weightfn_name << endl;
  out << "thesaurus.meta_name = " << meta_name << endl;
  out << "thesaurus.heuristic_name = " <<  heuristic_name << endl;
  out << "thesaurus.measure_name = " <<  measure_name << endl;
  out << "thesaurus.cutoff_minimum = " <<  cutoff_minimum << endl;
  out << "thesaurus.verbose = " << bool2str(verbose) << endl;
  out << "thesaurus.split = " << bool2str(split) << endl;
  out << "thesaurus.optimize = " <<  bool2str(optimize) << endl;
  out << "thesaurus.meta = " << bool2str(meta) << endl;
  out << "thesaurus.heuristic = " <<  bool2str(heuristic) << endl;
  out << "thesaurus.heuristic_size = " <<  heuristic_size << endl;
  out << "thesaurus.heuristic_fmin = " <<  heuristic_fmin << endl;
  out << "thesaurus.heuristic_anmax = " <<  heuristic_anmax << endl;
  out << "thesaurus.heuristic_verbs = " <<  bool2str(heuristic_verbs) << endl;
  out << "thesaurus.print_limit = " <<  print_limit << endl;
  out << "thesaurus.relations_file = " <<  relations_file << endl;
  if(split){
    out << "thesaurus.attributes_file = " << attributes_file << endl;
    out << "thesaurus.globals_file = " << globals_file << endl;
  }
  if(common_file.length() != 0)
    out << "thesaurus.common_file = " << common_file << endl;
}

std::string
Options::to_string(void) const {
  ostringstream out;
  to_stream(out);
  return out.str();
}

} }
