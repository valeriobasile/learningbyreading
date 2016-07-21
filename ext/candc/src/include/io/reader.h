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

class Reader {
protected:
  mutable std::ostringstream msg;

  // this actually needs to be an ostringstream but we need to accept
  // ostreams so that we can go die(msg << ...)
  virtual void die(std::ostream &msg) const;

  bool check(bool add, bool expect, bool res) const {
    if(!add || res == expect)
      return res;

    if(res)
      die(msg << "expected end of input for alignment with group members");
    else
      die(msg << "unexpected end of input for alignment with group members");

    // some compilers give a warning otherwise
    return false;
  }
public:
  const std::string name;
  const std::string uri;
  std::string PREFACE;

  Reader(const std::string &uri, const std::string &name): name(name), uri(uri){}
  virtual ~Reader(void){ /* do nothing */ }

  virtual void reset(void) = 0;
  virtual bool next(NLP::Sentence &sent, bool add = false, bool expect = false) = 0;
};

  }
}
