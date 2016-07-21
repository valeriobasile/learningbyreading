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
  namespace Thesaurus {

    class Options {
    private:
      void _defaults(void);
      void _usage(std::ostream &out) const;

      std::string _build_cmd_line(int argc, char **argv);
    public:
      const std::string command_line;

      std::string measure_name;
      std::string weightfn_name;
      std::string meta_name;
      std::string heuristic_name;
      std::string serverport;
      std::string script;
      std::string all;
      std::string test;

      std::string relations_file;
      std::string attributes_file;
      std::string globals_file;
      std::string common_file;

      bool verbose;
      bool split;
      bool optimize;
      bool heuristic;
      bool heuristic_verbs;
      bool meta;

      ulong rank;

      ulong cutoff_minimum;
      ulong heuristic_size;
      ulong heuristic_fmin;
      ulong heuristic_anmax;

      ulong print_limit;

      Options(const std::string &filename);
      Options(int argc, char **argv);

      void usage(void) const;
      void usage(const std::string &msg) const;
      void to_stream(std::ostream &out) const;
      std::string to_string(void) const;
    };

    inline std::ostream &operator<<(std::ostream &out, const Options &op){
      op.to_stream(out);
      return out;
    }
  }
}
