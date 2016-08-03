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

using namespace std;

namespace NLP { namespace Config {

std::string Main::PROGRAM_NAME;

static bool
split_assignment(const std::string &s, std::string &path, std::string &value_str){
  std::string::size_type i = s.find(ASSIGN);
  if(i == std::string::npos){
    path = s;
    return false;
  }

  path = s.substr(0, i);
  value_str = s.substr(i + 1);
  return true;
}

typedef std::map<std::string, int> RecursiveCheck;

template <class Cfg>
void
load_config(Cfg &cfg, RecursiveCheck &recursive_check,
            std::istream &in, const std::string &uri){

  if(recursive_check.count(uri)){
    int pnlines = recursive_check[uri];
    throw ConfigError("the configuration file has included itself recursively", uri, pnlines);
  }

  int nlines = 0;
  std::string buffer;
  while(std::getline(in, buffer)){
    ++nlines;
    recursive_check[uri] = nlines;

    // ignore blank lines and comments
    if(buffer.size() == 0 || buffer[0] == '#')
      continue;

    std::string name, value_str;
    split_assignment(buffer, name, value_str);

    name = strip_whitespace(name);
    value_str = strip_whitespace(strip_quotes(value_str));

    if (cfg.ignore_additional() && !cfg.has(name))
      continue;

    try {
      cfg.get(name).set(value_str);
    }
    catch(ConfigError e) {
      // add the configuration file information to this exception
      throw ConfigError(e.msg, e.option, uri, nlines);
    }
  }

  recursive_check.erase(uri);
}

std::string
Cfg::derived_path(const OpPath &base, const std::string &filename) const {
  return base() + Port::DIR_SEP + filename;
}

std::string
Cfg::derived_temp_path(const OpPath &base, const OpPath &temp,
                       const std::string &filename) const {
  if(Cluster::USE_MPI)
    return temp() + Port::DIR_SEP + filename + '.' + Cluster::rank_str;
  return base() + Port::DIR_SEP + filename;
}

Cfg::Cfg(const std::string &name, const std::string &desc, Flags flags, ushort order)
  : Node(name, desc, flags, order),
    _option_help(*this), _option_more_help(*this), _option_config(*this){}

Cfg::Cfg(Cfg &cfg, const std::string &name, const std::string &desc, Flags flags, ushort order)
  : Node(name, desc, flags, order),
    _option_help(*this), _option_config(*this){
  cfg.reg(*this);
}

void
Cfg::reg(Node &child, Flags flags){
  for(Nodes::iterator i = children.begin(); i != children.end(); ++i){
    if(*i && (*i)->order > child.order){
      children.insert(i, &child);
      return;
    }
  }

  children.push_back(&child);
  child.flags |= flags;
}

bool
Cfg::has(const std::string &path) const {
  try {
    std::string tail;

    for(Nodes::const_iterator i = children.begin(); i != children.end(); ++i){
      if(*i && (*i)->match(path, tail))
        return tail == "" || (*i)->has(tail);
    }

    return false;
  }catch(ConfigError e){
    if(NAME == "")
      throw;
    throw ConfigError(e, NAME + SEP + e.option);
  }
}

Node &
Cfg::get(const std::string &path){
  try {
    std::string tail;

    for(Nodes::const_iterator i = children.begin(); i != children.end(); ++i)
      if(*i && (*i)->match(path, tail)){
        if(tail != "")
          return (*i)->get(tail);
        return **i;
      }

    if(ignore_missing())
      // return a dummy option if we are ignoring missing values
      return _option_dummy;
    else{
      NLP::Config::die("option does not exist", path);
      // we never get here, but this stops the compiler warning
      return *static_cast<Node *>(0);
    }
  }catch(ConfigError e){
    if(NAME == "")
      throw;
    throw ConfigError(e, NAME + SEP + e.option);
  }
}

void
Cfg::check(void){
  try {
    for(Nodes::iterator i = children.begin(); i != children.end(); ++i)
      if(*i)
        (*i)->check();
  }catch(ConfigError e){
    if(NAME == "")
      throw;
    throw ConfigError(e, NAME + SEP + e.option);
  }
}

void
Cfg::load(std::istream &in, const std::string &uri){
  RecursiveCheck recursive_check;
  NLP::Config::load_config(*this, recursive_check, in, uri);
}

void
Cfg::load(const std::string &uri, bool ignore_ioerror){
  ifstream in(uri.c_str());
  if(!in){
    if(ignore_ioerror)
      return;

    throw NLP::IOException("could not open model configuration file for reading", uri);
  }

  load(in, uri);
}

void
Cfg::write_config(std::ostream &out, std::string prefix, bool root) const {
  if(!root)
    prefix += NAME + SEP;

  for(Nodes::const_iterator i = children.begin(); i != children.end(); ++i){
    assert(*i);

    if((*i)->hide_config())
      continue;

    if((*i)->add_space())
      out << '\n';

    out << "# " << (*i)->DESC << '\n';
    (*i)->writeln_config(out, prefix, false);
  }
}

void
Cfg::write_preface(std::ostream &out, std::string prefix, bool root) const {
  if(!root)
    prefix += NAME + SEP;

  for(Nodes::const_iterator i = children.begin(); i != children.end(); ++i){
    assert(*i);

    if((*i)->hide_preface())
      continue;

    if((*i)->add_space())
      out << "#\n";

    (*i)->writeln_preface(out, prefix, false);
  }
}

void
Cfg::write_help(std::ostream &out, std::string prefix, bool full) const {
  if(!NAME.empty())
    prefix += NAME + SEP;

  for(Nodes::const_iterator i = children.begin(); i != children.end(); ++i){
    assert(*i);

    if((*i)->hide_help() || (!full && (*i)->has_children()))
      continue;

    if((*i)->add_space())
      out << '\n';

    (*i)->writeln_help(out, prefix, full);
  }
}

bool
valid_option(const char *s){
  if(!s)
    die("illegal null string in command line arguments", "(null)");

  return *s == SEP;
}

void
Main::check(void){
  try {
    Cfg::check();
  }catch(ConfigError &e){
    cerr << Port::BOLD << Port::RED << PROGRAM_NAME << ':';
    cerr << e.option << ':' << e.msg;
    if(e.uri != ""){
      cerr << ':' << e.uri;
      if(e.line)
        cerr << ':' << e.line;
    }
    cerr << Port::OFF << "\n\n";
    _option_help.usage(1, false);
  }
}

void
Main::parse(const int argc, char * const *argv){
  for(int i = 1; i != argc; ++i){
    if(!valid_option(argv[i]))
      NLP::Config::die("unrecognised command line option", argv[i]);

    std::string arg = argv[i];

    std::string path;
    std::string value_str = "true";
    bool assigned_arg = split_assignment(arg, path, value_str);

    if(path.size() == 2)
      NLP::Config::die("short options not implemented yet", path);
    else
      path = path.substr(1 + (path[1] == SEP));

    Node &node = get(path);
    bool force_arg = node.needs_arg() && !assigned_arg;

    if(i + 1 == argc){
      if(force_arg)
        NLP::Config::die("command line option requires an argument", arg);
    }else{
      if(!valid_option(argv[i + 1]) || force_arg)
        value_str = argv[++i];
    }

    node.set(value_str);
  }
}

Directory::Directory(const std::string &name, const std::string &desc,
                     const OpPath *base, Flags flags)
  : Cfg(name, desc, flags),
    loaded_(false),
    path(name, "the " + name + " model(s) directory", base){
  if(base)
    path.set_default("//" + name);
}

Directory::Directory(Cfg &cfg, const std::string &name, const std::string &desc,
                     const OpPath *base, Flags flags)
  : Cfg(cfg, name, desc, flags),
    loaded_(false),
    path(name, "the " + name + " model(s) directory", base){
  if(base)
    path.set_default("//" + name);
}

void
Directory::set(const std::string &value_str){
  std::string config_file = value_str + Port::DIR_SEP + "config";
  load(config_file, ignore_missing());
  loaded_ = true;
  path.set(value_str);
}

void
Directory::check(void){
  path.check();
  Cfg::check();
}

void
Directory::reload(void){
  load(config(), true);
  loaded_ = true;
}

void
Directory::write_help(std::ostream &out, std::string prefix, bool full) const{
  path.writeln_help(out, prefix, full);
  Cfg::write_help(out, prefix, full);
}

void
Directory::write_desc(std::ostream &out, bool full) const {
  path.write_desc(out, full);
}

} }
