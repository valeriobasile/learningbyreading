// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_parser.h"
#include "parser/printer.h"
#include "parser/print_stream.h"
#include "parser/print_deps.h"
#include "parser/decoder_derivs.h"
#include "parser/decoder_deps_recall.h"
#include "parser/decoder_derivs_random.h"
#include "parser/generator.h"

#include "io/format.h"
#include "config/format.h"

#include "io/reader.h"
#include "io/reader_horiz.h"
#include "io/reader_multi_horiz.h"
#include "io/reader_factory.h"

#include "io/writer.h"
#include "io/writer_factory.h"

#include "model/model.h"

#include "tagger/tagdict.h"
#include "tagger/tagsetdict.h"
#include "tagger/tagger.h"

#include "tagger/super.h"

#include "parser/integration.h"

#include "cluster.h"

const char *PROGRAM_NAME = "forests";

using namespace std;
using namespace NLP;
using namespace NLP::IO;
using namespace NLP::Taggers;
using namespace NLP::CCG;

inline bool
read_features(const string filename, istream &in, Parser &parser,
	      vector<ulong> &ids, vector<long> &rules){
  ids.resize(0);
  rules.resize(0);
  std::string line;
  while(getline(in, line)){
    if(line.length() == 0)
      break;

    ulong id = parser.get_feature(filename, line, rules);
    if(id)
      ids.push_back(id - 1);
  }
  sort(ids.begin(), ids.end());
  return ids.size() > 0;
}

// adds the gold standard supertag if it is missing and
// increases gold standard supertag probability if it is not
// ranked #1 so it is always included in the supertags passed
// to the parser
void
add_gold(MultiRaw &mtags, const Raw &gold){
  for(MultiRaw::iterator j = mtags.begin(); j != mtags.end(); ++j)
    if(j->raw == gold){
      if(j != mtags.begin())
	j->score = 1.0;
      return;
    }

  ScoredRaw sr(gold, 1.0);
  mtags.push_back(sr);
}

void
force_gold_super(Sentence &sent){
  for(ulong i = 0; i < sent.words.size(); ++i)
    add_gold(sent.msuper[i], sent.super[i]);
}

int
run(int argc, char **argv){
  ulong START = 0;

  std::vector<double> BETAS;
  BETAS.push_back(0.01);
  BETAS.push_back(0.05);
  BETAS.push_back(0.1);

  std::vector<ulong> DICT_CUTOFFS;
  DICT_CUTOFFS.push_back(20);
  DICT_CUTOFFS.push_back(20);
  DICT_CUTOFFS.push_back(20);

  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);
  Super::Config super_cfg;
  Parser::Config parser_cfg;
  Integration::Config int_cfg("int");

  int_cfg.start.set_default(START);
  int_cfg.betas.set_default(BETAS);
  int_cfg.dict_cutoffs.set_default(DICT_CUTOFFS);

  ulong K = 20;

  Config::Op<ulong> TOTAL_NSENTS(cfg, SPACE, "nsents", "the total number of input sentences");
  Config::OpPath DATA(cfg, "data", "the base directory of the training data");
  Config::Op<std::string> FORESTS(cfg, "forests", "the base path for forests (node numbered .out/.log files)");

  Config::OpPath SENTS_DIR(cfg, "sents", "the directory containing split sentences", "//stag", &DATA);
  Config::OpPath FEATS_DIR(cfg, "feats", "the directory containing split gold standard features", "//feats", &DATA);
  Config::OpPath DEPS_DIR(cfg, "deps", "the directory containing split gold standard dependencies", "//deps", &DATA);

  Config::Alias start_alias(cfg, SPACE, int_cfg.start, "start_level", "int-start_level");
  Config::Alias betas_alias(cfg, int_cfg.betas, "betas", "int-betas");
  Config::Alias dict_cutoff_alias(cfg, int_cfg.dict_cutoffs, "dict_cutoffs", "int-dict_cutoffs");

  Config::Op<bool> print_lengths(cfg, SPACE, "print_lengths", "print the sentence length instead of id (for perceptron)", false);

  cfg.reg(int_cfg, SPACE);
  cfg.reg(parser_cfg, SPACE);
  cfg.reg(super_cfg, SPACE);

  cfg.parse(argc, argv);
  cfg.check();

  if(TOTAL_NSENTS() == 0){
    cerr << "number of sentences and machines must be positive integers" << endl;
    cerr << "usage: forests " << endl;
    return 0;
  }

  const ulong NMACHINES = Cluster::size;
  const ulong NSENTS = TOTAL_NSENTS()/NMACHINES + 1;
  const ulong start = Cluster::rank*NSENTS;

  ostringstream line;
  line << "forests " << Cluster::rank << " of " << Cluster::size << " at sentence " << start;
  line << " on " << Cluster::processor << " pid " << getpid() << '\n';
  cerr << line.str() << flush;

  ulong nparsed = 0;
  ulong nsentences = 0;

  Sentence sent;
  NLP::MultiTags multitags;

  Integration integration(int_cfg, super_cfg, parser_cfg, sent, Parser::LOAD_FEATURES);

  // open the various data files for reading
  const std::string SENTS = SENTS_DIR() + '/' + Cluster::rank_str;
  const std::string FEATS = FEATS_DIR() + '/' + Cluster::rank_str;
  const std::string DEPS = DEPS_DIR() + '/' + Cluster::rank_str;
  const std::string LOG = FORESTS() + "." + Cluster::rank_str + ".log";
  const std::string OUT = FORESTS() + "." + Cluster::rank_str + ".out";

  ulong nlines = 0;
  ifstream sents(SENTS.c_str());
  if(!sents)
    throw NLP::IOException("could not open sentences file ", SENTS);
  const std::string SENTS_PREF = read_preface(SENTS, sents, nlines);

  ifstream feats(FEATS.c_str());
  if(!feats)
    throw NLP::IOException("could not open gold standard features for reading", FEATS);
  const std::string FEATS_PREF = read_preface(FEATS, feats, nlines);

  ifstream deps(DEPS.c_str());
  if(!deps)
    throw NLP::IOException("could not open gold standard dependencies for reading", DEPS);
  const std::string DEPS_PREF = read_preface(DEPS, deps, nlines);

  PREFACE << SENTS_PREF << FEATS_PREF << DEPS_PREF;

  ofstream out(OUT.c_str());
  if(!out)
    throw NLP::IOException("could not open output file for writing", OUT);
  out << PREFACE.str() << endl;

  ofstream log(LOG.c_str());
  if(!log)
    throw NLP::IOException("could not open log file for writing", LOG);
  log << PREFACE.str() << endl;

  CCG::InsideOutside inside_outside(integration.parser.is_partial_gold());

  vector<ulong> gold_feats;
  vector<long> gold_derivs;

  START = int_cfg.start();
  BETAS = int_cfg.betas();
  DICT_CUTOFFS = int_cfg.dict_cutoffs();

  NLP::IO::MultiHReader reader(sents, SENTS, "wps");
  while(reader.next(sent)){
    nsentences++;

    if(sent.words.size() == 0){
      log << "end of input" << endl;
      break;
    }

    integration.super.mtag(sent, NLP::Taggers::FWDBWD, K, 0.001);
    force_gold_super(sent);

    bool read_gold = read_features(FEATS, feats, integration.parser, gold_feats, gold_derivs);
    log << "read " << gold_feats.size() << " features" << endl;

    bool read_deps = inside_outside.read_dependencies(DEPS, deps);
    log << "read " << inside_outside.depscores.size() << " dependencies" << endl;

    for(RawWords::iterator i = sent.words.begin(); i != sent.words.end(); ++i)
      log << ' ' << *i;
    log << endl;

    if(!read_deps || !read_gold){
      log << "ignoring sentence without gold standard " << start + nsentences << endl;
      continue;
    }else if(sent.words.size() == 1){
      log << "ignoring unary sentence " << start + nsentences << endl;
      continue;
    }

    ulong trial = START;
    while(1){
      const double BETA = BETAS[trial];
      const ulong K = DICT_CUTOFFS[trial];

      if(integration.parser.parse(BETA, false)){
	ulong id = print_lengths() ? sent.words.size() : nsentences;
	if(integration.parser.print_forest(inside_outside, out, id, gold_feats, gold_derivs)){
	  ++nparsed;
	  log << start + nsentences << ' ' << CCG::SuperCat::nsupercats << ' '
	      << nparsed*100.0/nsentences << '%' << endl;

	  CCG::Statistics stats;
	  integration.parser.calc_stats(stats);
	  log << start + nsentences << " stats: logderivs nequiv " << stats.logderivs
	      << ' ' << stats.nequiv << ' ' << stats.ntotal << endl;
	  break;
	}else{
	  log << nsentences << " no spanning node at B=" << BETA << endl;
	  break;
	}
      }else{
	log << nsentences << " parse exploded at B=" << BETA << ", K=" << K << endl;
	++trial;
	if(trial >= BETAS.size()){
	  log << nsentences << " fail explode at B=" << BETA << ", K=" << K << endl;
	  break;
	}
      }
    }
  }

  log << "end of input" << endl;

  line.str("");
  line << "forests " << Cluster::rank << " of " << Cluster::size << " done on ";
  line << Cluster::processor << " pid " << getpid() << '\n';
  cerr << line.str() << flush;

  return 0;
}

#include "cluster_main.h"
