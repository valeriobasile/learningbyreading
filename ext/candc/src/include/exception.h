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

// NLP::Exception and NLP::IOException
// exception classes for reporting general and I/O errors
// these tend to only be caught in the main program

namespace NLP {

  // general errors with an error message
  class Exception: public std::exception {
  public:
    const std::string msg;

    Exception(const std::string &msg)
      : msg(msg){}
    Exception(const Exception &other)
      : std::exception(other), msg(other.msg){}

    virtual ~Exception(void) throw(){}

    virtual const char* what(void) const throw() { return msg.c_str(); }
  };

  // I/O errors with an error message, filename, and line number
  // normally these occur when files are missing or file reading
  // when the text does not match the expected input format
  class IOException: public Exception {
  public:
    const std::string uri;
    const int line;

    IOException(const std::string &msg)
      : Exception(msg), line(0){}
    IOException(const std::string &msg, const std::string &uri, int line = 0)
      : Exception(msg), uri(uri), line(line){}

    IOException(const IOException &other)
      : Exception(other), uri(other.uri), line(other.line){}

    virtual ~IOException(void) throw(){}
  };

  // Configuration system errors (illegal values etc)
  class ConfigError: public IOException {
  public:
    const std::string option;

    ConfigError(const std::string &msg, const std::string &option) throw()
      : IOException(msg), option(option){}

    ConfigError(const std::string &msg, const std::string &option,
		const std::string &uri, int line = 0)
      : IOException(msg, uri, line), option(option){}

    ConfigError(const std::string &msg, const std::string &uri, int line)
      : IOException(msg, uri, line){}

    ConfigError(const ConfigError &other, const std::string &option)
      : IOException(other), option(option){}

    virtual ~ConfigError(void) throw(){}
  };

  // CCG parser errors, with a message, and sentence number
  class ParseError: public Exception {
  public:
    const int sentence;

    ParseError(const std::string &msg, int sentence = 0)
      : Exception(msg), sentence(sentence){}
    virtual ~ParseError(void) throw(){}
  };

}
