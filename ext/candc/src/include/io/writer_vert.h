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

    class VWriter: public StreamWriter {
    protected:
      const std::string BEGIN;
      const std::string END;
    public:
      const char SEP;

      VWriter(std::ostream &out, const std::string &uri, char SEP = '\t',
	      const std::string &BEGIN = "", const std::string &END = "\n",
	      const std::string &name = "VWriter(w)")
	: StreamWriter(out, uri, name), BEGIN(BEGIN), END(END), SEP(SEP){}
      virtual ~VWriter(void){ /* do nothing */ }

      virtual void next(NLP::Sentence &sent);
    };

  }
}
