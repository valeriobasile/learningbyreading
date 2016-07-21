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
  namespace IO {

    extern const std::string STDIN;
    extern const std::string STDOUT;
    extern const std::string STDERR;

    class Input {
    private:
      std::ifstream fstream;
    public:
      const std::string uri;
      std::istream &stream;

      Input(const std::string &uri);
      Input(const std::string &uri, std::istream &s)
	: uri(uri), stream(s){}
    };

    class Output {
    private:
      std::ofstream fstream;
    public:
      const std::string uri;
      std::ostream &stream;

      Output(const std::string &uri);
      Output(const std::string &uri, std::ostream &s)
	: uri(uri), stream(s){}
    };

    class Log {
    private:
      std::ofstream fstream;
    public:
      const std::string uri;
      std::ostream &stream;

      Log(const std::string &uri);
      Log(const std::string &uri, std::ostream &s)
	: uri(uri), stream(s){}
    };

  }
}
