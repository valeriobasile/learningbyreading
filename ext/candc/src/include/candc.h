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

#include "base.h"

#include "config/config.h"

#include "pool.h"

#include "model/model.h"
#include "model/types.h"

#include "io/format.h"
#include "config/format.h"

#include "io/reader.h"
#include "io/writer.h"

#include "io/reader_factory.h"
#include "io/writer_factory.h"

#include "tagger/tagdict.h"
#include "tagger/tagsetdict.h"

#include "tagger/tagger.h"
#include "tagger/pos.h"
#include "tagger/chunk.h"
#include "tagger/ner.h"
#include "tagger/super.h"

#include "parser/parser.h"
#include "parser/decoder_factory.h"

#include "parser/printer.h"
#include "parser/print_stream.h"
#include "parser/print_factory.h"

#include "parser/integration.h"

#include "relations/morpha.h"

namespace NLP {

  using namespace NLP::Config;
  using namespace NLP::CCG;
  using namespace NLP::Taggers;

class CandC {
public:
  class Config: public Directory {
  public:
    Op<bool> verbose;
    Op<bool> comments;
    OpPath morph;

    Op<std::string> printer;
    Op<std::string> decoder;

    Op<bool> compact;

    Op<ulong> maxwords;
    Restricted<std::string> maxwords_policy;
    Op<bool> skip_quotes;
    Op<bool> trans_brackets;

    POS::Config pos;
    Chunk::Config chunk;
    NER::Config ner;
    Super::Config super;
    Parser::Config parser;
    Integration::Config integration;

    Op<ulong> pos_dict_cutoff;
    Op<ulong> chunk_dict_cutoff;
    Op<ulong> ner_dict_cutoff;

    Config(const OpPath *base = 0,
	   const std::string &name = "candc",
	   const std::string &desc = "C&C tools configuration");

    void set(const std::string &val){
      Directory::set(val);
      pos.reload();
      chunk.reload();
      ner.reload();
      super.reload();
      parser.reload();
    }

    void check(void);
  };

public:
  char buffer[65536];

  const Config &cfg;
  const std::string PREFACE;

  Sentence sent;

  std::string meta;
  std::vector<ulong> meta_sentids;

  int START;
  std::vector<double> BETAS;
  std::vector<ulong> CUTOFFS;

  POS pos;
  Chunk chunk;
  NER ner;
  Integration integration;

  DecoderFactory decoder;

  ulong nsents;
  ulong nlines;
  ulong nparsed;

  CandC(Config &cfg, const std::string &PREFACE);
  virtual ~CandC(void);

  void print_meta(std::ostream &out, const StreamPrinter::Format){
    if(meta_sentids.size() == 0 || meta == "")
      return;

    // TODO add restriction so this only gets called for prolog mode
    out << "id(" << meta << ", [" << meta_sentids[0];
    for(ulong i = 1; i < meta_sentids.size(); ++i)
      out << ", " << meta_sentids[i];
    out << "]).\n\n";
  }

  void set_meta(void){
    meta = buffer + 6;
    meta_sentids.resize(0);
  }

  void add_id(ulong nsentences){
    meta_sentids.push_back(nsentences);
  }

  void reset(void){
    meta = "";
    meta_sentids.resize(0);
  }

  bool
  read_buffer(std::istream &in){
    in.getline(buffer, sizeof(buffer));
    nlines++;

    ulong len = in.gcount();
    if(in.eof())
      return false;
    if(!in)
      throw NLP::IOException("unexpected file error");
    if(len == sizeof(buffer) - 1)
      throw NLP::IOException("sentence too long or not terminated by newline");

    return true;
  };

  bool
  is_meta(std::string &){
    if(strncmp(buffer, "<META>", 6))
      return false;

    return true;
  }

  bool load_sentence(std::ostream &out);
	bool load_oracle(std::ostream &out);
  bool load_lemmas(void);

  double parse(IO::Input &in, IO::Output &out, IO::Log &log,
               bool START = true, const std::string &printer = "");

	double oracle(IO::Input &in, IO::Input &constraints, IO::Output &out, IO::Log &log,
                bool START = true, const std::string &printer = "");
protected:
  Taggers::State *const pos_state;
  Taggers::State *const chunk_state;
  Taggers::State *const ner_state;
};

}
