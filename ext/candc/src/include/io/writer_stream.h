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

    class StreamWriter: public Writer {
    protected:
      std::ostream &out;
      int nlines;

      virtual void die(std::ostream &msg) const;

      const std::string &check(const std::string &token, char SEP) const {
	if(token.find(SEP) != std::string::npos)
	  die(msg << "token " << token << " should not contain sep '" << SEP << '\'');
	return token;
      }
    public:
      StreamWriter(std::ostream &out, const std::string &uri,
		   const std::string &name)
	: Writer(uri, name), out(out), nlines(0){}

      virtual ~StreamWriter(void){ /* do nothing */ }

      virtual void write_preface(const std::string &preface);
    };

  }
}
