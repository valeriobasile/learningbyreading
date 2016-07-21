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

class GroupWriter: public Writer {
public:
  typedef std::vector<Writer *> Members;
  Members members;

  GroupWriter(const std::string &uri, const std::string &name = "GroupWriter")
    : Writer(uri, name){}
  virtual ~GroupWriter(void){ /* do nothing */ }

  void join(Writer *writer){ members.push_back(writer); }

  virtual void write_preface(const std::string &preface);
  virtual void next(NLP::Sentence &sent);
};

  }
}
