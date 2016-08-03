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

bool
Parser::_Impl::print_forest(InsideOutside &inside_outside, ostream &out, ulong id,
			    const vector<ulong> &correct, const vector<long> &rules){
  ulong ndisj = 0;

  Cell &root = chart.root();
  if(root.size() == 0)
    return false;

  for(Cell::iterator i = root.begin(); i != root.end(); ++i)
    (*i)->mark_active_disj(ndisj);

  out << id << ' ' << inside_outside.depscores.size() << '\n';

  out << correct.size();
  for(vector<ulong>::const_iterator i = correct.begin(); i != correct.end(); ++i)
  out << ' ' << *i;
  out << '\n';
  
  //print out the correct rules
  out << rules.size();
  for(vector<long>::const_iterator i = rules.begin(); i != rules.end(); ++i)
    out << ' ' << *i;
  out << '\n';

  out << ndisj << '\n';

  Words words;
  raws2words(sent.words, words);
  Words tags;
  raws2words(sent.pos, tags);

  ulong node = 0;
  for(ulong pos = 0; pos < chart.nwords; ++pos){
    Cell &cell = chart(pos, 1);
    for(Cell::iterator i = cell.begin(); i != cell.end(); ++i){
      SuperCat *canonical = *i;
      if(!canonical->is_active())
	continue;

      canonical->marker = node++;

      ulong nequiv = canonical->nequiv();
      out << canonical->marker << '\n' << nequiv << '\n';
      for(const SuperCat *equiv = canonical; equiv; equiv = equiv->next){
	if(equiv->unary())
	  print_unary_features(inside_outside, out, equiv, words, tags);
	else
	  print_leaf_features(inside_outside, out, pos, equiv, words, tags);
      }
    }
  }

  const long NWORDS = chart.nwords;
  for(long j = 2; j <= NWORDS; ++j)
    for(long i = j - 2; i >= 0; --i){
      Cell &cell = chart(i, j - i);
      if(i == 0 && j == NWORDS)
	break;

      for(Cell::iterator k = cell.begin(); k != cell.end(); ++k){
	SuperCat *canonical = *k;
	if(!canonical->is_active())
	  continue;

	canonical->marker = node++;

	ulong nequiv = canonical->nequiv();
	out << canonical->marker << '\n' << nequiv << '\n';

	for(const SuperCat *equiv = canonical; equiv; equiv = equiv->next){
	  if(equiv->unary())
	    print_unary_features(inside_outside, out, equiv, words, tags);
	  else
	    print_binary_features(inside_outside, out, equiv, words, tags);
	}
      }
    }

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    SuperCat *canonical = *i;
    if(!canonical->is_active())
      continue;

    canonical->marker = node++;

    ulong nequiv = canonical->nequiv();
    out << canonical->marker << '\n' << nequiv << '\n';

    for(const SuperCat *equiv = canonical; equiv; equiv = equiv->next)
      print_root_features(inside_outside, out, equiv, words, tags);
  }

  return true;
}


void
Parser::_Impl::print_leaf_features(InsideOutside &, ostream &out, ulong,
				   const SuperCat *leaf, const Words &words, const Words &tags){
  out << "0 ";
  ids.resize(0);

  cat_feats.add(leaf, words, tags, LEX, ids);

  if(ids.size()){
    out << ids.size();
    for(vector<ulong>::iterator id = ids.begin(); id != ids.end(); ++id)
      out << ' ' << *id;
    out << '\n';
  }else
    out << "0\n";
}


void
Parser::_Impl::print_unary_features(InsideOutside &, ostream &out, const SuperCat *sc,
				    const Words &words, const Words &tags){
  out << "1 " << sc->left->marker << ' ';

  ids.resize(0);
  rule_feats.add(sc, words, tags, URULE, ids);
  rule_head_feats.add(sc, words, tags, URULE, ids);
  genrule_feats.add(sc, words, tags,GEN_RULE, ids); //sc: last 3 args unused

  if(ids.size()){
    out << ids.size();
    for(vector<ulong>::iterator id = ids.begin(); id != ids.end(); ++id)
      out << ' ' << *id;
    out << '\n';
  }else
    out << "0\n";
}

void
Parser::_Impl::print_binary_features(InsideOutside &inside_outside, ostream &out, const SuperCat *sc,
				     const Words &words, const Words &tags){
  out << "2 " << sc->left->marker << ' ' << sc->right->marker << ' ' << inside_outside.count_gold_deps(sc) << ' ';

  ids.resize(0);

  dep_feats.add(sc, words, tags, DEP_WORD_POS, ids);
  dep_dist_feats.add(sc, words, tags, DEP_WORD_POS, ids);
  rule_feats.add(sc, words, tags, BRULE, ids);
  rule_head_feats.add(sc, words, tags, BRULE, ids);
  rule_dep_feats.add(sc, words, tags, BRULE, ids);
  genrule_feats.add(sc, words, tags, GEN_RULE, ids); // sc: last 3 args not used
  rule_dep_dist_feats.add(sc, words, tags, BRULE, ids); 

  if(ids.size()){
    out << ids.size();
    for(vector<ulong>::iterator id = ids.begin(); id != ids.end(); ++id)
      out << ' ' << *id;
    out << '\n';
  }else
    out << "0\n";
}

void
Parser::_Impl::print_root_features(InsideOutside &inside_outside, ostream &out, const SuperCat *sc,
				   const Words &words, const Words &tags){
  out << "3 " << sc->left->marker << ' ' << sc->right->marker << ' ' << inside_outside.count_gold_deps(sc) << ' ';

  ids.resize(0);

  cat_feats.add(sc, words, tags, ROOT, ids);
  dep_feats.add(sc, words, tags, DEP_WORD_POS, ids);
  dep_dist_feats.add(sc, words, tags, DEP_WORD_POS, ids);
  rule_feats.add(sc, words, tags, BRULE, ids);
  rule_head_feats.add(sc, words, tags, BRULE, ids);
  rule_dep_feats.add(sc, words, tags, BRULE, ids);
  genrule_feats.add(sc, words, tags, GEN_RULE, ids); //sc: last 3 args not used
  rule_dep_dist_feats.add(sc, words, tags, BRULE, ids);

  if(ids.size()){
    out << ids.size();
    for(vector<ulong>::iterator id = ids.begin(); id != ids.end(); ++id)
      out << ' ' << *id;
    out << '\n';
  }else
    out << "0\n";
}

} }
