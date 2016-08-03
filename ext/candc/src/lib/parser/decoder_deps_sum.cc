// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <limits>
#include <string>
#include <sstream>
#include <vector>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <algorithm>

#include <cassert>
#include <cmath>
#include <map>

#include <sys/time.h>
#include <sys/resource.h>

#include <cstdlib>

using namespace std;

#include "utils.h"
#include "pool.h"
#include "hash.h"
#include "exception.h"

#include "word.h"
#include "lexicon.h"

#include "input.h"

#include "parser/fixed.h"
#include "parser/atom.h"
#include "parser/feature.h"
#include "parser/varid.h"
#include "parser/category.h"
#include "parser/markedup.h"
#include "parser/variable.h"
#include "parser/relation.h"
#include "parser/dependency.h"
#include "parser/distance.h"
#include "parser/filled.h"
#include "parser/relations.h"
#include "parser/canonical.h"
#include "parser/categories.h"
#include "parser/supercat.h"
#include "parser/unify.h"
#include "parser/rule.h"
#include "parser/cell.h"
#include "parser/equiv.h"
#include "parser/treebank.h"
#include "parser/chart.h"
#include "tree/attributes.h"
#include "parser/depscore.h"
#include "parser/printer.h"
#include "parser/parser.h"

// this file is obsolete
// the sum decoder was the first decoder we tried when the
// C&C parser was first developed.  It finds the most probable
// dependency structure but is extremely inefficient.
// We don't know of an efficient algorithm for finding the most
// probable dependency structure.

// these lines were taken from parser.h

const SuperCat *best_equiv_sum(const SuperCat *sc);
const SuperCat *best_sum(void);
const SuperCat *calc_root_canonical_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags);
void calc_score_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags);
void calc_score_unary_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags);
void calc_score_binary_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags);
const SuperCat *calc_score_canonical_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags);


namespace NLP { namespace CCG {

typedef map<ulong,double> Sums;

using namespace NLP::Tree;

const SuperCat *
Parser::best_sum(void){
  Cell &root = chart.root();
  if(root.size() == 0)
    return 0;

  // calc_scores();

  Words words;
  raws2words(sent.sentence, words);
  Words tags;
  raws2words(sent.postags, tags);

  double max_score = -numeric_limits<double>::max();
  const SuperCat *max_sc = 0;
  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    const SuperCat *current = calc_root_canonical_sum(*i, words, tags, sent.sentence, sent.postags);
    if(current->score > max_score){
      max_score = current->score;
      max_sc = current;
    }
  }

  return max_sc;
}

const SuperCat *
Parser::best_equiv_sum(const SuperCat *sc){
  Sums sums;
  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    if(sums.count(equiv->marker) != 0)
      sums[equiv->marker] = add_logs(sums[equiv->marker], equiv->score);
    else
      sums[equiv->marker] = equiv->score;
  }

  double max_score = -numeric_limits<double>::max();
  ulong max_marker = 0;
  for(Sums::iterator i = sums.begin(); i != sums.end(); ++i)
    if(i->second > max_score){
      max_score = i->second;
      max_marker = i->first;
    }

  SuperCat *max_sc = 0;
  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next)
    if(equiv->marker == max_marker){
      max_sc = const_cast<SuperCat *>(equiv);
      max_sc->score = max_score;
      max_sc->max = max_sc;
      sc->max = max_sc;
      return max_sc;
    }

  throw NLP::Exception("could not find SuperCat marker in score_root");
  return 0;
}


const SuperCat *
Parser::calc_root_canonical_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags){
  for(SuperCat *equiv = sc; equiv; equiv = const_cast<SuperCat *>(equiv->next)){
    calc_score_sum(equiv, words, tags, str_words, str_tags);
    equiv->score += score_root_features(equiv, words, tags);
  }

  return best_equiv_sum(sc);
}


void
Parser::calc_score_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags){
  if(sc->left){
    if(sc->right)
      calc_score_binary_sum(sc, words, tags, str_words, str_tags);
    else
      calc_score_unary_sum(sc, words, tags, str_words, str_tags);
  }else
    calc_score_leaf(sc, words, tags);
}


void
Parser::calc_score_binary_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags){
  sc->left = calc_score_canonical_sum(const_cast<SuperCat *>(sc->left), words, tags, str_words, str_tags);
  sc->right = calc_score_canonical_sum(const_cast<SuperCat *>(sc->right), words, tags, str_words, str_tags);

  sc->marker = sc->left->marker * sc->right->marker;

  sc->score = sc->left->score + sc->right->score;
  sc->score += score_binary_feats(sc, words, tags, str_words, str_tags);

  for(const Filled *filled = sc->filled; filled; filled = filled->next)
    sc->marker *= dependency_marker(filled, sc->vars);
}


void
Parser::calc_score_unary_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags){
  sc->left = calc_score_canonical_sum(const_cast<SuperCat *>(sc->left), words, tags, str_words, str_tags);

  sc->marker = sc->left->marker;
  sc->score = sc->left->score;

  sc->score += score_rule(sc, words, tags, URULE);
  sc->score += score_genrule(sc);
}


const SuperCat *
Parser::calc_score_canonical_sum(SuperCat *sc, const Words &words, const Words &tags, const vector<std::string> &str_words, const vector<std::string> &str_tags){
  if(sc->marker != 0)
    return sc->max;

  for(SuperCat *equiv = sc; equiv; equiv = const_cast<SuperCat *>(equiv->next))
    calc_score_sum(equiv, words, tags, str_words, str_tags);

  return best_equiv_sum(sc);
}

} }
