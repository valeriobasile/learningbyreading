// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "candc.h"

#include "prob.h"
#include "tagger/taghist.h"
#include "tagger/nodepool.h"
#include "tagger/lattice.h"
#include "tagger/flattice.h"
#include "tagger/state.h"

using namespace std;

namespace NLP {

static
void
policy_check(const std::string &s){
  if(s != "ignore" && s != "warn" && s != "error")
    throw NLP::Exception("unrecognised policy '" + s + "' [ignore, warn, error]");
}

CandC::Config::Config(const OpPath *base,
		      const std::string &name,
		      const std::string &desc)
  : Directory(name, desc, base),
    verbose(*this, "verbose", "verbose output", false),
    comments(*this, "comments", "comment the output", false),
    morph(*this, "morph", "directory containing the morphological analyser info", "//verbstem.list", &path),
    printer(*this, "printer", "parser printing style [deps, grs, prolog, boxer, ccgbank, xml, debug, js]", "grs"),
    decoder(*this, "decoder", "parser decoder [deps, derivs, random]", "derivs"),
    compact(*this, SPACE, "compact", "use compact output representation if it exists", false),
    maxwords(*this, SPACE, "maxwords", "maximum sentence length the parser will accept", 250),
    maxwords_policy(*this, "maxwords_policy", "parser behaviour on sentences longer than maxwords [ignore, warn, error]",
		    &policy_check, "ignore"),
    skip_quotes(*this, "skip_quotes", "skip over all quote and double quote tokens", false),
    trans_brackets(*this, "trans_brackets", "translate all brackets to Penn Treebank notation", false),
    pos(&path), chunk(&path), ner(&path), super(&path), parser(&path),
    pos_dict_cutoff(pos, "dict_cutoff", "POS tagger tag dictionary cutoff", 5),
    chunk_dict_cutoff(chunk, "dict_cutoff", "chunker tag dictionary cutoff", 5),
    ner_dict_cutoff(ner, "dict_cutoff", "NER tag dictionary cutoff", 5){

  reg(pos, SPACE);
  reg(chunk, SPACE);
  reg(ner, SPACE);
  reg(super, SPACE);
  reg(parser, SPACE);
  reg(integration, SPACE);
}

void
CandC::Config::check(void){
  pos.maxwords.set_value(maxwords());
  chunk.maxwords.set_value(maxwords());
  ner.maxwords.set_value(maxwords());
  super.maxwords.set_value(maxwords());
  parser.maxwords.set_value(maxwords());

  Directory::check();
}

CandC::~CandC(void){}

CandC::CandC(Config &cfg, const std::string &PREFACE)
  : cfg(cfg),
    PREFACE(PREFACE),
    // load taggers
    pos(cfg.pos),
    chunk(cfg.chunk),
    ner(cfg.ner),
    // load integrated supertagger and parser
    integration(cfg.integration, cfg.super, cfg.parser, sent),
    decoder(cfg.decoder()),
    pos_state(pos.create_state()),
    chunk_state(chunk.create_state()),
    ner_state(ner.create_state()){
  morph_initialise(cfg.morph().c_str());
}

static bool
is_quote(const std::string &s){
  switch(s[0]){
    case '`': return s == "``" || s == "`";
    case '\'': return s == "''" || s == "'";
    case '"': return true;
  }
  return false;
}

static void
ptb_bracket(std::string &s){
  if(s.size() != 1)
    return;

  switch(s[0]){
  case '(': s = "-LRB-"; return;
  case ')': s = "-RRB-"; return;
  case '{': s = "-LCB-"; return;
  case '}': s = "-RCB-"; return;
  case '[': s = "-LSB-"; return;
  case ']': s = "-RSB-"; return;
  }
}

void
split_token(std::string token, std::string &word, std::string &pos, std::string &super){
	word = "";
	pos = "";
	super = "";

	int state = 0;
	for(string::iterator i = token.begin(); i != token.end(); ++i){
		if(*i == '|'){
			++state;
			continue;
		}
		switch(state){
			case 0: word += *i; break;
			case 1: pos += *i; break;
			case 2: super += *i; break;
			default: 
				throw NLP::Exception("input token should only have 2 pipes -- word|pos|super");
		}
	}
}

bool
CandC::load_sentence(std::ostream &log){
  sent.reset();

  std::istringstream in(buffer);
  std::string s;
  while(in >> s){
    if(cfg.skip_quotes() && is_quote(s))
	continue;
    if(cfg.trans_brackets())
      ptb_bracket(s);
    sent.words.push_back(s);
  }

  if(sent.words.size() == 0){
    if(cfg.verbose())
      log << "ignoring blank on line " << nlines << endl;

    return false;
  }

  nsents++;

  if(sent.words.size() > cfg.maxwords()){
    if(cfg.maxwords_policy() == "error")
      throw NLP::Exception("sentence length exceeds maximum number of words");

    if(cfg.maxwords_policy() == "ignore")
      return false;

    if(cfg.verbose() || cfg.maxwords_policy() == "warn")
      log << "ignoring long sentence (length " << sent.words.size() << " words) on line "
	  << nlines << endl;

    return false;
  }

  return true;
}

bool
CandC::load_oracle(std::ostream &log){
	sent.reset();

	std::istringstream in(buffer);
	std::string token, word, pos, super;
	while(in >> token){
		cout << "reading a token " << token << endl;

		split_token(token, word, pos, super);
		if(cfg.skip_quotes() && is_quote(word))
			continue;
		if(cfg.trans_brackets())
			ptb_bracket(word);

		sent.words.push_back(word);
		sent.pos.push_back(pos);
		sent.super.push_back(super);
	}

	if(sent.words.size() == 0){
		if(cfg.verbose())
			log << "ignoring blank on line " << nlines << endl;
		return false;
	}

	nsents++;

	if(sent.words.size() > cfg.maxwords()){
		if(cfg.maxwords_policy() == "error")
			throw NLP::Exception("sentence length exceeds maximum number of words");

		if(cfg.maxwords_policy() == "ignore")
			return false;

		if(cfg.verbose() || cfg.maxwords_policy() == "warn")
			log << "ignoring long sentence (length " << sent.words.size() << " words) on line "
					<< nlines << endl;

		return false;
	}

	sent.copy_multi('s', 'S');

	return true;
}

const static ulong MAX_MORPHA_LEN = 32;

bool
use_morpha(const std::string &word, const std::string &pos){
  if(word.size() >= MAX_MORPHA_LEN)
    return false;

  if(pos == "NNP" || pos == "NNPS")
    return false;

  for(std::string::const_iterator i = word.begin(); i != word.end(); ++i)
    if(!(isalpha(*i) || *i == '-'))
      return false;

  return true;
}

bool
CandC::load_lemmas(void){
  int len = sent.words.size();

  sent.lemmas.resize(0);
  sent.lemmas.reserve(len);
  for(int i = 0; i < len; ++i){
    
    if(use_morpha(sent.words[i], sent.pos[i])){
      std::string in = sent.words[i];
      in += '_';
      in += sent.pos[i];

      char out[MAX_MORPHA_LEN];

      morph_analyse(out, in.c_str(), true);
      sent.lemmas.push_back(out);
    }else
      sent.lemmas.push_back(sent.words[i]);
  }

  return true;
}

double
CandC::parse(IO::Input &in, IO::Output &out, IO::Log &log, bool START,
             const std::string &override_printer){
  reset();

  StreamPrinter::Format FORMAT = StreamPrinter::FMT_ALL;
  if(cfg.compact())
    FORMAT &= ~StreamPrinter::FMT_WS;

  std::string printer_name = cfg.printer();
  if(override_printer != "")
    printer_name = override_printer;

  PrinterFactory printer(printer_name, out, log, integration.cats, FORMAT);

  if(START){
    nlines = 0;
    nsents = 0;
    nparsed = 0;

    printer.header(PREFACE);
  }

  while(read_buffer(in.stream)){
    if(is_meta(meta)){
      print_meta(out.stream, StreamPrinter::FMT_ALL);
      // set the new meta tag and clear sentence ids
      set_meta();
      // reset the last tag feature of the NE recogniser
      ner.begin_document(ner_state);
      continue;
    }

    if(!load_sentence(log.stream))
      continue;

    add_id(nsents);

    pos.tag(sent, VITERBI, cfg.pos_dict_cutoff(), pos_state);
    if(FORMAT & (StreamPrinter::FMT_CHUNK | StreamPrinter::FMT_NER))
      chunk.tag(sent, VITERBI, cfg.chunk_dict_cutoff(), chunk_state);
    if(FORMAT & StreamPrinter::FMT_NER)
      ner.tag(sent, VITERBI, cfg.ner_dict_cutoff(), ner_state);

    if(FORMAT & StreamPrinter::FMT_LEMMA)
      load_lemmas();

    integration.parse(sent, decoder, printer);
  }

  printer.footer();
  print_meta(out.stream, FORMAT);

  return nsents ? double(nparsed)/nsents : nsents;
}

double
CandC::oracle(IO::Input &in, IO::Input &constraints, IO::Output &out, IO::Log &log,
							bool START, const std::string &override_printer){
	reset();
	
	StreamPrinter::Format FORMAT = StreamPrinter::FMT_DEV;
	if(cfg.compact())
		FORMAT &= ~StreamPrinter::FMT_WS;

	std::string printer_name = cfg.printer();
	if(override_printer != "")
		printer_name = override_printer;

	PrinterFactory printer(printer_name, out, log, integration.cats, FORMAT);

	if(START){
		nlines = 0;
		nsents = 0;
		nparsed = 0;
		
		printer.header(PREFACE);
	}

	while(read_buffer(in.stream)){
		if(is_meta(meta)){
			print_meta(out.stream, StreamPrinter::FMT_DEV);
			// set the new meta tag and clear sentence ids
			set_meta();
			// reset the last tag feature of the NE recogniser
			ner.begin_document();
			continue;
		}

		if(!load_oracle(log.stream))
			continue;

		std::string line;
		while(getline(constraints.stream, line)){
			if(line.size() == 0)
				break;
			sent.constraints.push_back(integration.cats.constraint(line));
		}

		add_id(nsents);

		integration.parse(sent, decoder, printer, false); // don't use the supertagger
	}

	printer.footer();
	print_meta(out.stream, FORMAT);
	
	return nsents ? double(nparsed)/nsents : nsents;
}

}
