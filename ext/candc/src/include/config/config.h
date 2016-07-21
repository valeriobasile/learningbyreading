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

#include "config/options.h"

namespace NLP {
  namespace Config {

    const static char COMMENT = '#';
    const static char DELIMITER = ',';
    const static char ESCAPE = '\\';
    const static char ASSIGN = '=';

    class Cfg: public Node {
    public:
      typedef std::vector<Node *> Nodes;

      std::string PREFACE;
      Nodes children;
    protected:
      OpHelp _option_help;
      OpMoreHelp _option_more_help;
      OpCfg _option_config;
      OpDummy _option_dummy;
    public:
      Cfg(const std::string &name, const std::string &desc, Flags flags = SHOW_ALL, ushort order = 0);
      Cfg(Cfg &cfg, const std::string &name, const std::string &desc, Flags flags = SHOW_ALL, ushort order = 0);
      virtual ~Cfg(void){ /* do nothing */ }

      std::string derived_path(const OpPath &base, const std::string &filename) const;
      std::string derived_temp_path(const OpPath &base, const OpPath &temp, const std::string &filename) const;

      virtual void reg(Node &child, Flags flags = 0);

      virtual bool has(const std::string &name) const;
      virtual Node &get(const std::string &name);
      virtual void set(const std::string &){ die("cannot set the value of a Config"); }
      virtual void check(void);

      virtual void load(std::istream &in, const std::string &uri);
      virtual void load(const std::string &uri, bool ignore_ioerror);

      bool ignore_missing(void) const { return flags & IGNORE_MISSING; }

      virtual bool needs_arg(void) const { return false; }
      virtual bool has_children(void) const { return true; }

      using Node::write_config;
      using Node::write_preface;
      using Node::write_help;

      virtual void write_help(std::ostream &out, std::string prefix = "", bool full = false) const;
      virtual void write_config(std::ostream &out, std::string prefix = "", bool root = true) const;
      virtual void write_preface(std::ostream &out, std::string prefix = "", bool root = true) const;

      virtual void writeln_help(std::ostream &out, std::string prefix = "", bool full = false) const {
        out << DESC << " options:\n";
        write_help(out, prefix, full);
      }
      virtual void writeln_config(std::ostream &out, std::string prefix = "", bool root = true) const {
        write_config(out, prefix, root);
      }
      virtual void writeln_preface(std::ostream &out, std::string prefix = "", bool root = true) const {
        write_preface(out, prefix, root);
      }
    };

    class Main: public Cfg {
    public:
      static std::string PROGRAM_NAME;
    protected:
      OpVersion _option_version;
      OpLicence _option_licence;
    public:
      Main(const std::string &name)
        : Cfg("", "main program", SHOW_ALL),
          _option_version(*this), _option_licence(*this){
        PROGRAM_NAME = name;
      }
      virtual ~Main(void) { /* do nothing */ }

      virtual void check(void);

      using Cfg::set;
      void set(const std::string &path, const std::string &value_str){ get(path).set(value_str); }
      void parse(const int argc, char * const *argv);
    };

    class Directory: public Cfg {
    protected:
      bool loaded_;
    public:
      OpPath path;

      std::string config(void) const { return derived_path(path, "config"); }

      Directory(const std::string &name, const std::string &desc,
                const OpPath *base = 0, Flags flags = SHOW_ALL);
      Directory(Cfg &cfg, const std::string &name, const std::string &desc,
                const OpPath *base = 0, Flags flags = SHOW_ALL);

      virtual ~Directory(void){ /* do nothing */ }

      virtual void reload(void);

      // override so we can set a model directory
      virtual bool needs_arg(void) const { return true; }

      virtual void set(const std::string &val);
      virtual void check(void);

      virtual void write_help(std::ostream &out, std::string prefix = "", bool full = false) const;
      virtual void write_desc(std::ostream &out, bool full) const;
    };

  }
}
