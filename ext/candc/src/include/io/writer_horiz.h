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

    class HWriter: public StreamWriter {
    public:
      const char SEP;

      HWriter(std::ostream &out, const std::string &uri, char SEP = '|',
	      const std::string &name = "HWriter(w)"):
	StreamWriter(out, uri, name), SEP(SEP){}
      virtual ~HWriter(void){ /* do nothing */ }

      virtual void next(NLP::Sentence &sent);
    };

  }
}
