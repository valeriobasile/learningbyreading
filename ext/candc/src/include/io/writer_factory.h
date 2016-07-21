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

    class WriterFactory: public Writer {
    private:
      Output output;
      Writer *writer;
    public:
      WriterFactory(const std::string &uri, const Format &fmt);
      virtual ~WriterFactory(void){ delete writer; }

      virtual void write_preface(const std::string &preface){
	writer->write_preface(preface);
      }
      virtual void next(NLP::Sentence &sent){
	writer->next(sent);
      }
    };

  }
}
