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

#include "cluster.h"

#include "config/config.h"

#include "licence.h"

using namespace std;

namespace NLP { namespace Config {

const std::string Option::REQUIRED = string(" (") + Port::BOLD + "REQUIRED" + Port::OFF + ')';

Option::Option(const std::string &name, const std::string &desc, Flags flags)
  : Node(name, desc, flags){}

Option::Option(Cfg &cfg, const std::string &name, const std::string &desc, Flags flags)
  : Node(name, desc, flags){
  cfg.reg(*this);
}

void
Option::check(void){
  if(!is_valid())
    die("required option argument is undefined");
}

void
Option::write_config(std::ostream &out, std::string prefix, bool) const {
  out << prefix << NAME;
}

void
Option::write_preface(std::ostream &out, std::string prefix, bool root) const {
  out << "#    ";
  write_config(out, prefix, root);
}

void
Option::write_help(std::ostream &out, std::string prefix, bool full) const {
  out << prefix << NAME;
  
  if(needs_arg())
    out << " <arg>";
  
  out << ": ";
  write_desc(out, full);
}

void
Option::writeln_preface(std::ostream &out, std::string prefix, bool root) const {
  if(!has_changed())
    return;

  write_preface(out, prefix, root);
  out << '\n';
}

Alias::Alias(Node &node, const std::string &name, const std::string &other)
  : Node(name, "(alias for " + other + ')', HIDE_CONFIG | HIDE_PREFACE),
    node_(node), other_(other){}

Alias::Alias(Cfg &cfg, Node &node, const std::string &name, const std::string &other)
  : Node(name, "(alias for " + other + ')', HIDE_CONFIG | HIDE_PREFACE),
    node_(node), other_(other){
  cfg.reg(*this);
}

Alias::Alias(Cfg &cfg, Flags flags, Node &node, const std::string &name, const std::string &other)
  : Node(name, "(alias for " + other + ')', flags | HIDE_CONFIG | HIDE_PREFACE),
    node_(node), other_(other){
  cfg.reg(*this);
}

void
Alias::write_help(std::ostream &out, std::string prefix, bool full) const {
  out << prefix << NAME;
  
  if(needs_arg())
    out << " <arg>";
  
  out << ": ";
  write_desc(out, full);
}

void
Alias::write_desc(std::ostream &out, bool full) const {
  node_.write_desc(out, full);
  if(full && node_.NAME != NAME)
    out << " (alias for --" << other_ << ')';
}

void
Alias::check(void){
  try {
    node_.check();
  }catch(ConfigError e){
    throw ConfigError(e, NAME);
  }
}

void
OpVersion::set(const std::string &){
  cerr << Main::PROGRAM_NAME << ' ' << VERSION << ' ' << BUILD << '\n';
  exit(0);
}

void
OpVersion::write_config(std::ostream &out, std::string prefix, bool) const {
  out << prefix << NAME << " = " << VERSION << ' ' << BUILD << '\n';
}

void
OpLicence::set(const std::string &){
  cerr << LICENCE;
  exit(0);
}

void
OpHelp::usage(int exitcode, bool full) const {
  cerr << "usage: " << Main::PROGRAM_NAME << " [options]\n\n";
  cfg->write_help(cerr, "  --", full);
  cerr << '\n';
  exit(exitcode);
}

void
OpHelp::set(const std::string &){
  usage(0, false);
}

void
OpMoreHelp::set(const std::string &){
  usage(0, true);
}

void
OpCfg::set(const std::string &value_str){
  assert(cfg);

  ifstream in(value_str.c_str());
  if(!in)
    NLP::IOException("could not open configuration file for reading", value_str);

  cfg->load(in, value_str);
}

std::string
OpPath::get_value(void) const {
  if(BASE && value[0] == Port::DIR_SEP && value[1] == Port::DIR_SEP)
		return (*BASE)() + Port::DIR_SEP + value.substr(2);
  else
    return value;
}

} }
