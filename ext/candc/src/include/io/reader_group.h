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

class GroupReader: public Reader {
public:
  typedef std::vector<Reader *> Members;
  Members members;

  GroupReader(const std::string &uri, const std::string &name = "GroupReader"):
    Reader(uri, name){}
  virtual ~GroupReader(void){ /* do nothing */ }

  void join(Reader *reader){ members.push_back(reader); }

  virtual void reset(void);
  virtual bool next(NLP::Sentence &sent, bool add = false, bool expect = false);
};

  }
}
