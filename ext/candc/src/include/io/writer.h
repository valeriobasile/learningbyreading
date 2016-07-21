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

    class Writer {
    protected:
      mutable std::ostringstream msg;

      // this actually needs to be an ostringstream but we need to accept
      // ostreams so that we can go die(msg << ...)
      virtual void die(std::ostream &out) const;
    public:
      const std::string &name;
      const std::string uri;

      Writer(const std::string &uri, const std::string &name)
	: name(name), uri(uri){}
      virtual ~Writer(void){ /* do nothing */ }

      virtual void write_preface(const std::string &preface) = 0;
      virtual void next(NLP::Sentence &sent) = 0;
    };

  }
}
