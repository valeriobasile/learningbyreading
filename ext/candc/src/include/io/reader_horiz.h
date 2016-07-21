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

class HReader: public Reader {
protected:
  const static ulong SBUFFER = 1024*1024;

  char prebuffer;
  char buffer[SBUFFER];
  ulong len;
  ulong nlines;

  const char SEP;

  std::istream &in;

  bool next_line(void);

  virtual void die(std::ostream &msg) const;
public:
  HReader(std::istream &in, const std::string &uri, char SEP = '|',
	  const std::string &name = "HReader(w)");
  virtual ~HReader(void);

  virtual void reset(void);
  virtual bool next(NLP::Sentence &sent, bool add = false, bool expect = false);
};

  }
}
