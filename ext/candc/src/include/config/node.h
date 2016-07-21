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

namespace NLP {
  namespace Config {

    typedef ushort Flags;

    const static Flags HIDE_HELP    = 1 << 0;
    const static Flags HIDE_CONFIG  = 1 << 1;
    const static Flags HIDE_PREFACE = 1 << 2;
    const static Flags HAS_DEFAULT  = 1 << 3;
    const static Flags HAS_CHANGED  = 1 << 4;
    const static Flags IS_DEFINED   = 1 << 5;
    const static Flags IS_OPTIONAL  = 1 << 6;
    const static Flags SPACE        = 1 << 7;
    const static Flags IGNORE_MISSING    = 1 << 8; // used by Config::Cfg -- eventually we want to remove this
    const static Flags IGNORE_ADDITIONAL = 1 << 9; // used by Model::Config::ESTIMATE

    const static Flags SHOW_ALL = 0;
    const static Flags HIDE_ALL = HIDE_HELP | HIDE_CONFIG | HIDE_PREFACE;
    const static Flags OPTIONAL = IS_OPTIONAL | HIDE_CONFIG | HIDE_PREFACE;

    const static char SEP = '-';

    inline void die(const std::string &msg, const std::string &name){
      throw NLP::ConfigError(msg, name);
    }
    inline void die(const std::string &msg, const std::string &name,
                    const std::string &uri, ulong nlines){
      throw NLP::ConfigError(msg, name, uri, nlines);
    }

    class Node {
    public:
      const std::string NAME;
      const std::string DESC;
      mutable Flags flags;
      ushort order;
    public:
      Node(const std::string &name, const std::string &desc, Flags flags=SHOW_ALL, ushort order=0)
        : NAME(name), DESC(desc), flags(flags), order(order){}
      virtual ~Node(void){ /* do nothing */ }

      bool has_default(void) const { return flags & HAS_DEFAULT; }
      bool has_changed(void) const { return flags & HAS_CHANGED; }
      bool is_defined(void) const { return flags & IS_DEFINED; }
      bool is_optional(void) const { return flags & IS_OPTIONAL; }
      bool ignore_additional(void) const { return flags & IGNORE_ADDITIONAL; }

      void die(const std::string &msg) const { NLP::Config::die(msg, NAME); }

      virtual void reg(Node &child, Flags flags = 0) = 0;

      bool match(const std::string &path, std::string &tail){
        std::string::size_type len = NAME.size();

        if(path.substr(0, len) != NAME)
          return false;

        if(path.size() == len){
          tail = "";
          return true;
        }

        if(path[len] == SEP){
          tail = path.substr(len + 1);
          return true;
        }

        return false;
      }

      virtual bool has(const std::string &name) const = 0;
      virtual Node &get(const std::string &name) = 0;
      virtual void set(const std::string &val) = 0;
      virtual void check(void) = 0;

      virtual bool has_children(void) const = 0;
      virtual bool needs_arg(void) const = 0;

      bool hide_help(void) const { return flags & HIDE_HELP; }
      bool hide_config(void) const { return flags & HIDE_CONFIG; }
      bool hide_preface(void) const { return flags & HIDE_PREFACE; }
      bool add_space(void) const { return flags & SPACE; }

      void write_help(const std::string &filename, const std::string &PREFIX, bool full=false) const;
      void write_config(const std::string &filename, const std::string &PREFIX, bool root=true) const;
      void write_preface(const std::string &filename, const std::string &PREFIX, bool root=true) const;

      virtual void write_help(std::ostream &out, std::string prefix = "", bool full=false) const = 0;
      virtual void write_config(std::ostream &out, std::string prefix = "", bool root=true) const = 0;
      virtual void write_preface(std::ostream &out, std::string prefix = "", bool root=true) const = 0;

      virtual void write_desc(std::ostream &out, bool) const {
        out << DESC;
      }

      virtual void writeln_help(std::ostream &out, std::string prefix = "", bool full=false) const {
        write_help(out, prefix, full);
        out << '\n';
      }

      virtual void writeln_config(std::ostream &out, std::string prefix = "", bool root=true) const {
        write_config(out, prefix, root);
        out << '\n';
      }

      virtual void writeln_preface(std::ostream &out, std::string prefix = "", bool root=true) const {
        write_preface(out, prefix, root);
        out << '\n';
      }
    };

  }
}
