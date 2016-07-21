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

    class ReaderFactory: public Reader {
    private:
      Input input;
      Reader *reader;
    public:
      ReaderFactory(const std::string &uri, const Format &fmt);
      virtual ~ReaderFactory(void){ delete reader; }

      virtual void reset(void){ reader->reset(); }
      virtual bool next(NLP::Sentence &sent, bool add = false, bool expect = false){
	return reader->next(sent, add, expect);
      }
    };

  }
}
