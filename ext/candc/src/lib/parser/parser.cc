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

using namespace std;

namespace NLP { namespace CCG {

using namespace NLP::Tree;

Parser::Config::Config(const OpPath *base, const std::string &name, const std::string &desc)
  : Directory(name, desc, base),
    cats(*this, "cats", "parser category directory", "//cats", &path),
    markedup(*this, "markedup", "parser markedup file", "//markedup", &cats),
    weights(*this, "weights", "parser model weights file", "//weights", &path),
    rules(*this, "rules", "parser rules file", "//rules", &path),
    maxwords(*this, SPACE, "maxwords", "maximum sentence length the parser will accept", 250),
    maxsupercats(*this, "maxsupercats", "maximum number of supercats before the parse explodes", 300000),
    alt_markedup(*this, SPACE, "alt_markedup", "use the alternative markedup categories (marked with !)", false),
    seen_rules(*this, "seen_rules", "only accept category combinations seen in the training data", true),
    extra_rules(*this, "extra_rules", "use additional punctuation and unary type-changing rules", true),
    question_rules(*this, "question_rules", "use the unary rules that only apply to questions", false),
    noisy_rules(*this, "noisy_rules", "use noisy non-CCG rules used in CCGbank", true),
    eisner_nf(*this, "eisner_nf", "only accept composition when the Eisner (1996) constraints are met", true),
    partial_gold(*this, SPACE, "partial_gold", "used for generating feature forests for training", false),
    beam(*this, "beam_ratio", "(not fully tested)", 0.0){}

void
Parser::_Impl::_load_rules(const std::string &filename){
  ulong nlines = 0;
  if(filename == "")
    return;

  ifstream in(filename.c_str());
  if(!in)
    throw NLP::IOException("could not open rules file", filename);

  comment += read_preface(filename, in, nlines);

  string cat1str, cat2str;
  while(in >> cat1str >> cat2str){
    try {
      const Cat *cat1 = cats.canonize(cat1str.c_str());
      const Cat *cat2 = cats.canonize(cat2str.c_str());
    
      rule_instances.insert(cat1, cat2);
    }catch(NLP::Exception e){
      throw NLP::IOException("error parsing category in rule instantiation", filename);
    }
  }
}

void
Parser::_Impl::_load_features(const std::string &filename){
  ulong nlines = 0;
  ifstream in(filename.c_str());
  if(!in)
    NLP::IOException("could not open features file", filename);

  comment += read_preface(filename, in, nlines);

  nfeatures = 1;
  string tmp;
  ulong freq;
  for(char c; in >> freq >> c; ++nfeatures){
    switch(c){
    case 'a': cat_feats.load(in, filename, nfeatures, LEX_WORD); break;
    case 'b': cat_feats.load(in, filename, nfeatures, LEX_POS); break;
    case 'c': cat_feats.load(in, filename, nfeatures, ROOT); break;
    case 'd': cat_feats.load(in, filename, nfeatures, ROOT_WORD); break;
    case 'e': cat_feats.load(in, filename, nfeatures, ROOT_POS); break;

    case 'f': dep_feats.load(in, filename, nfeatures, DEP_WORD); break;
    case 'g': dep_feats.load(in, filename, nfeatures, DEP_POS); break;
    case 'h': dep_feats.load(in, filename, nfeatures, DEP_WORD_POS); break;
    case 'i': dep_feats.load(in, filename, nfeatures, DEP_POS_WORD); break;

    case 'x': genrule_feats.load(in, filename, nfeatures, GEN_RULE); break;
    case 'm': rule_feats.load(in, filename, nfeatures, URULE); break;
    case 'n': rule_feats.load(in, filename, nfeatures, BRULE); break;

    case 'p': rule_head_feats.load(in, filename, nfeatures, URULE_HEAD); break;
    case 'q': rule_head_feats.load(in, filename, nfeatures, BRULE_HEAD); break;
    case 'r': rule_head_feats.load(in, filename, nfeatures, URULE_POS); break;
    case 's': rule_head_feats.load(in, filename, nfeatures, BRULE_POS); break;

    case 't': rule_dep_feats.load(in, filename, nfeatures, BRULE_HEAD_HEAD); break;
    case 'u': rule_dep_feats.load(in, filename, nfeatures, BRULE_POS_HEAD); break;
    case 'v': rule_dep_feats.load(in, filename, nfeatures, BRULE_HEAD_POS); break;
    case 'w': rule_dep_feats.load(in, filename, nfeatures, BRULE_POS_POS); break; 

    case 'F': rule_dep_dist_feats.load(in, filename, nfeatures, DIST_ADJ_HEAD); break;
    case 'G': rule_dep_dist_feats.load(in, filename, nfeatures, DIST_VERBS_HEAD); break;
    case 'H': rule_dep_dist_feats.load(in, filename, nfeatures, DIST_PUNCT_HEAD); break;
    case 'I': rule_dep_dist_feats.load(in, filename, nfeatures, DIST_ADJ_POS); break;
    case 'J': rule_dep_dist_feats.load(in, filename, nfeatures, DIST_VERBS_POS); break;
    case 'K': rule_dep_dist_feats.load(in, filename, nfeatures, DIST_PUNCT_POS); break;

    case 'L': dep_dist_feats.load(in, filename, nfeatures, DIST_ADJ_HEAD); break;
    case 'M': dep_dist_feats.load(in, filename, nfeatures, DIST_VERBS_HEAD); break;
    case 'N': dep_dist_feats.load(in, filename, nfeatures, DIST_PUNCT_HEAD); break;
    case 'P': dep_dist_feats.load(in, filename, nfeatures, DIST_ADJ_POS); break;
    case 'Q': dep_dist_feats.load(in, filename, nfeatures, DIST_VERBS_POS); break;
    case 'R': dep_dist_feats.load(in, filename, nfeatures, DIST_PUNCT_POS); break;

    default:
      throw NLP::IOException("unexpected feature type in load features", filename, nfeatures);
    }
  }
  --nfeatures;
}

ulong
Parser::_Impl::get_feature(const std::string &filename, const std::string &line, vector<long> &rules) const {
  istringstream in(line);

  char c;
  in >> c;
  std::string tmp;

  switch(c){
  case 'a': return cat_feats.get_id(in, filename, LEX_WORD, rules);
  case 'b': return cat_feats.get_id(in, filename, LEX_POS, rules);
  case 'c': return cat_feats.get_id(in, filename, ROOT, rules);
  case 'd': return cat_feats.get_id(in, filename, ROOT_WORD, rules);
  case 'e': return cat_feats.get_id(in, filename, ROOT_POS, rules);

  case 'f': return dep_feats.get_id(in, filename, DEP_WORD, rules);
  case 'g': return dep_feats.get_id(in, filename, DEP_POS, rules);
  case 'h': return dep_feats.get_id(in, filename, DEP_WORD_POS, rules);
  case 'i': return dep_feats.get_id(in, filename, DEP_POS_WORD, rules);

  case 'x': return genrule_feats.get_id(in, filename, GEN_RULE, rules);

  case 'm': return rule_feats.get_id(in, filename, URULE, rules);
  case 'n': return rule_feats.get_id(in, filename, BRULE, rules);
   
  case 'p': return rule_head_feats.get_id(in, filename, URULE_HEAD, rules);
  case 'q': return rule_head_feats.get_id(in, filename, BRULE_HEAD, rules);
  case 'r': return rule_head_feats.get_id(in, filename, URULE_POS, rules);
  case 's': return rule_head_feats.get_id(in, filename, BRULE_POS, rules);

  case 't': return rule_dep_feats.get_id(in, filename, BRULE_HEAD_HEAD, rules);
  case 'u': return rule_dep_feats.get_id(in, filename, BRULE_POS_HEAD, rules);
  case 'v': return rule_dep_feats.get_id(in, filename, BRULE_HEAD_POS, rules);
  case 'w': return rule_dep_feats.get_id(in, filename, BRULE_POS_POS, rules);

  case 'F': return rule_dep_dist_feats.get_id(in, filename, DIST_ADJ_HEAD, rules);
  case 'G': return rule_dep_dist_feats.get_id(in, filename, DIST_VERBS_HEAD, rules);
  case 'H': return rule_dep_dist_feats.get_id(in, filename, DIST_PUNCT_HEAD, rules);
  case 'I': return rule_dep_dist_feats.get_id(in, filename, DIST_ADJ_POS, rules);
  case 'J': return rule_dep_dist_feats.get_id(in, filename, DIST_VERBS_POS, rules);
  case 'K': return rule_dep_dist_feats.get_id(in, filename, DIST_PUNCT_POS, rules);

  case 'L': return dep_dist_feats.get_id(in, filename, DIST_ADJ_HEAD, rules);
  case 'M': return dep_dist_feats.get_id(in, filename, DIST_VERBS_HEAD, rules);
  case 'N': return dep_dist_feats.get_id(in, filename, DIST_PUNCT_HEAD, rules);
  case 'P': return dep_dist_feats.get_id(in, filename, DIST_ADJ_POS, rules);
  case 'Q': return dep_dist_feats.get_id(in, filename, DIST_VERBS_POS, rules);
  case 'R': return dep_dist_feats.get_id(in, filename, DIST_PUNCT_POS, rules);
  default:
    throw NLP::IOException("unexpected feature type in load feature", filename, nfeatures);
  } 
}


void
Parser::_Impl::_load_weights(const std::string &filename){
  ifstream in(filename.c_str());
  if(!in)
    NLP::IOException("could not open features file", filename);

  weights = new double[nfeatures];
  ulong nread = 0;
  double weight;

  while(in >> weight){
    if(nread == nfeatures)
      throw NLP::IOException("number of features is less than the number of weights", filename, nread + 1);
    weights[nread++] = weight;
  }

  if(!in.eof())
    throw NLP::IOException("error reading weight value", filename, nread + 1);

  if(nread != nfeatures)
    throw NLP::IOException("number of features is greater than the number of weights", filename, nread + 1);
}

Parser::_Impl::_Impl(const Config &cfg, Sentence &sent, Categories &cats, ulong load)
  : cfg(cfg), sent(sent),
    nsentences(0),
    cats(cats),
    lexicon("lexicon", cfg.lexicon()),
    cat_feats(cats, lexicon),
    rule_feats(cats),
    rule_head_feats(cats, lexicon),
    rule_dep_feats(cats, lexicon),
    rule_dep_dist_feats(cats, lexicon),
    dep_feats(lexicon),
    dep_dist_feats(lexicon),
    genrule_feats(cats),
    weights(0),
    chart(cats, cfg.extra_rules(), cfg.maxwords()),
    rules(chart.pool, cats.markedup, cfg.extra_rules(), cfg.noisy_rules()){

  if(load >= LOAD_FEATURES){
    _load_features(cfg.features());

    if(load >= LOAD_WEIGHTS){
      _load_weights(cfg.weights());

      cat_feats.set_weights(weights);
      rule_feats.set_weights(weights);
      rule_head_feats.set_weights(weights);
      rule_dep_feats.set_weights(weights);
      rule_dep_dist_feats.set_weights(weights);
      dep_feats.set_weights(weights);
      dep_dist_feats.set_weights(weights);
    }

    if(cfg.seen_rules())
      _load_rules(cfg.rules());
  }

  sent.words.reserve(cfg.maxwords());
  sent.pos.reserve(cfg.maxwords());
  sent.msuper.reserve(cfg.maxwords());
}

void
Parser::_Impl::combine(Cell &left, Cell &right, long pos, long span){
  ++stats.ncombines;
  
  if(left.empty() || right.empty()){
    ++stats.ncombines_zeros;
    return;
  }

  if(!left.updated() && !right.updated()){
    ++stats.ncombines_rejected;
    return;
  }

	if(left.exclude || right.exclude)
		return;

  results.resize(0);
  bool part_skip = false;
  for(Cell::iterator i = left.begin(); i != left.end(); ++i)
    for(Cell::iterator j = right.begin(); j != right.end(); ++j){
      if(i < left.old() && j < right.old()){
				part_skip = true;
				continue;
      }
      if(!cfg.seen_rules() || rule_instances((*i)->cat, (*j)->cat))
      	rules(*i, *j, cfg.eisner_nf(), cfg.seen_rules(), cfg.question_rules(), results);
    }

  chart.add(pos, span, results);
  stats.ncombines_reduced += part_skip;
}

bool
Parser::_Impl::parse(const double BETA, bool repair){
  try {
    nsentences++;
    if(!repair)
      SuperCat::nsupercats = 0;
  
    if(sent.words.size() > cfg.maxwords())
      throw NLP::ParseError("sentence length exceeds maximum number of words for parser", nsentences);

    chart.load(sent, BETA, repair, true, cfg.question_rules());

    Words words;
    raws2words(sent.words, words);
  
    Words tags;
    raws2words(sent.pos, tags);

    const long NWORDS = sent.words.size();

    // doesn't make sense to apply beam to leaf cells
    // since the supertagger already provides a beam at this level
    // but still need to calculate beam scores to be used at higher levels
    if(cfg.beam() > 0.0)
      for(long i = 0; i < NWORDS; ++i)
				calc_beam_scores(chart(i,1), words, tags);

    for(long j = 2; j <= NWORDS; ++j){
      for(long i = j - 2; i >= 0; --i){
	for(long k = i + 1; k < j; ++k){
	  try {
	    combine(chart(i, k - i), chart(k, j - k), i, j - i);
	  }catch(NLP::Exception e){
	    throw NLP::ParseError(e.what(), nsentences);
	  }

	  if(SuperCat::nsupercats > cfg.maxsupercats())
	    return false;
	}

	if(cfg.beam() > 0.0)
	  if(j - i < NWORDS){
	    calc_beam_scores(chart(i, j - i), words, tags);
	    apply_beam(chart(i, j - i), cfg.beam());
	  }

	if(j - i < NWORDS){
	  chart.lex(i, j - i, cfg.question_rules());
	  chart.tr(i, j - i);
	}

	if(SuperCat::nsupercats > cfg.maxsupercats())
	  return false;
      }
    }

    return true;
  }catch(NLP::Exception e){
    throw NLP::ParseError(e.msg, nsentences);
  }
}

void
Parser::_Impl::calc_root_canonical(SuperCat *sc, const Words &words, const Words &tags){
  for(SuperCat *equiv = sc; equiv; equiv = const_cast<SuperCat *>(equiv->next)){
    calc_score(equiv, words, tags);
    equiv->score += cat_feats.score(equiv, words, tags, ROOT);
  }

  sc->marker = 1;
}

void
Parser::_Impl::calc_score(SuperCat *sc, const Words &words, const Words &tags){
  if(sc->left){
    if(sc->right)
      calc_score_binary(sc, words, tags);
    else
      calc_score_unary(sc, words, tags);
  }else
    calc_score_leaf(sc, words, tags);
}

void
Parser::_Impl::calc_score_canonical(SuperCat *sc, const Words &words, const Words &tags){
  if(sc->marker)
    return;

  for(SuperCat *equiv = sc; equiv; equiv = const_cast<SuperCat *>(equiv->next))
    calc_score(equiv, words, tags);
}

double
Parser::_Impl::score_binary_feats(SuperCat *sc, const Words &words, const Words &tags){
  double score = 0.0;

  score += dep_feats.score(sc, words, tags, DEP_WORD_POS);
  score += dep_dist_feats.score(sc, words, tags, DEP_WORD_POS);
  score += rule_feats.score(sc, words, tags, BRULE);
  score += rule_head_feats.score(sc, words, tags, BRULE);
  score += genrule_feats.score(sc, words, tags, GEN_RULE);  //last 3 arguments don't get used
  score += rule_dep_feats.score(sc, words, tags, BRULE);
  score += rule_dep_dist_feats.score(sc, words, tags, BRULE);

  return score;
}

void
Parser::_Impl::calc_score_binary(SuperCat *sc, const Words &words, const Words &tags){
  sc->marker = 1;

  calc_score_canonical(const_cast<SuperCat *>(sc->left), words, tags);
  calc_score_canonical(const_cast<SuperCat *>(sc->right), words, tags);

  sc->score = score_binary_feats(sc, words, tags);
}

void
Parser::_Impl::calc_score_unary(SuperCat *sc, const Words &words, const Words &tags){
  sc->marker = 1;

  sc->score = rule_feats.score(sc, words, tags, URULE);
  sc->score += rule_head_feats.score(sc, words, tags, URULE);
  sc->score += genrule_feats.score(sc, words, tags, GEN_RULE); // sc: last 3 args not used

  calc_score_canonical(const_cast<SuperCat *>(sc->left), words, tags);
}

void
Parser::_Impl::calc_score_leaf(SuperCat *sc, const Words &words, const Words &tags){
  sc->marker = 1;
  sc->score = cat_feats.score(sc, words, tags, LEX);
}

bool
Parser::_Impl::calc_scores(void){
  Cell &root = chart.root();
  if(root.size() == 0)
    return false;

  Words words;
  raws2words(sent.words, words);

  Words tags;
  raws2words(sent.pos, tags);

  for(Cell::iterator i = root.begin(); i != root.end(); ++i)
    calc_root_canonical(*i, words, tags);

  return true;
}

ulong
Parser::_Impl::dependency_marker(const Filled *filled) const {
  Hash h(filled->head);
  h += filled->filler;
  h += filled->rel;
  h += filled->rule + filled->lrange;
  return h.value();
}

void
Parser::_Impl::raws2words(const vector<std::string> &raw, Words &words) const {
  words.resize(0);
  words.reserve(sent.words.size());
  for(vector<std::string>::const_iterator i = raw.begin(); i != raw.end(); ++i)
    words.push_back(lexicon[*i]);
}

const SuperCat *
Parser::_Impl::best(Decoder &decoder){
  return decoder.best(chart);
}

// counts the number of derivations each dependency occurs in --
// relies on having a uniform weights file
bool
Parser::_Impl::deps_count(ostream &out){
  calc_scores();
  inside_outside.calc(chart);

  // TODO allow an option to choose between these
  // top version outputs the words rather than numbers
  //  depscores.dump(out, cats.markedup, cats.relations, sentence);
  inside_outside.depscores.dump(out);
  out << '\n';

  return 1;
}

void
Parser::_Impl::calc_stats(Statistics &stats){
  stats = this->stats;
  stats.logderivs = inside_outside.calc_stats(chart, stats.nequiv, stats.ntotal);
}

void
Parser::_Impl::reset(void){
  stats.reset();
}

Parser::Parser(const Config &cfg, Sentence &sent, Categories &cats, ulong load){
  _impl = new Parser::_Impl(cfg, sent, cats, load);
}

Parser::~Parser(void){ delete _impl; }

bool Parser::parse(double BETA, bool repair){ return _impl->parse(BETA, repair); }

bool Parser::deps_count(ostream &out){ return _impl->deps_count(out); }

bool Parser::count_rules(void){
  return _impl->count_rules();
}

void Parser::print_rules(std::ostream &out) const {
  _impl->rule_attrs.print_entries(out);
}

bool Parser::print_forest(InsideOutside &inside_outside, ostream &out, ulong id,
			  const vector<ulong> &correct, const vector<long> &rules){
  return _impl->print_forest(inside_outside, out, id, correct, rules);
}

bool Parser::calc_scores(void){ return _impl->calc_scores(); }

const SuperCat *Parser::best(Decoder &decoder){ 
  return _impl->best(decoder);
}

ulong Parser::get_feature(const std::string &filename, const std::string &line,
			  std::vector<long> &rules) const {
  return _impl->get_feature(filename, line, rules);
}

bool Parser::is_partial_gold(void) const {
  return _impl->cfg.partial_gold();
}

void Parser::calc_stats(Statistics &stats){
  _impl->calc_stats(stats);
}

void Parser::reset(void){
  _impl->reset();
}

void
Parser::dump_chart(ostream &out) const {
  _impl->chart.dump(out);
}

Sentence &Parser::sentence(void){ return _impl->sent; }
Chart &Parser::chart(void){ return _impl->chart; }
Rules &Parser::rules(void){ return _impl->rules; }

} }
