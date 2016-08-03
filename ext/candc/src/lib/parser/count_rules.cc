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
Parser::_Impl::count_rules(void){
  ulong ndisj = 0;

  const long NWORDS = sent.words.size();

  Cell &root = chart.root();
  if(root.size() == 0)
    return false;

  for(Cell::iterator i = root.begin(); i != root.end(); ++i)
    (*i)->mark_active_disj(ndisj);

  Words words;
  raws2words(sent.words, words);

  Words tags;
  raws2words(sent.pos, tags);

  for(long pos = 0; pos < NWORDS; ++pos){
    Cell &cell = chart(pos, 1);
    for(Cell::iterator i = cell.begin(); i != cell.end(); ++i){
      SuperCat *canonical = *i;
      if(!canonical->is_active())
	continue;

      for(const SuperCat *equiv = canonical; equiv; equiv = equiv->next){
	if(equiv->unary())
	  _count_rule(equiv, words, tags, URULE);
      }
    }
  }

  for(long j = 2; j <= NWORDS; ++j)
    for(long i = j - 2; i >= 0; --i){
      Cell &cell = chart(i, j - i);

      for(Cell::iterator k = cell.begin(); k != cell.end(); ++k){
	SuperCat *canonical = *k;
	if(!canonical->is_active())
	  continue;

	for(const SuperCat *equiv = canonical; equiv; equiv = equiv->next){
	  if(equiv->unary())
	    _count_rule(equiv, words, tags, URULE);
	  else
	    _count_rule(equiv, words, tags, BRULE);
	}
      }
    }

  return true;
}


void
Parser::_Impl::_count_rule_head(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var,
				const Words &words, const Words &tags, const Type type){

  const Position *const end = var->fillers + Variable::NFILLERS;
  for(const Position *p = var->fillers; p != end && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    Word head = words[*p - 1];
    Word tag = tags[*p - 1];

    // The lexicon is created from the features file, which is created 
    // from the gold-standard normal-form derivations. Some valid, infrequent
    // heads will not appear in the features file, so this check, which we
    // usually have, is useless:
    //    if(!head || !tag)
    //    throw NLP::IOException("head or tag not in lexicon\n");

    if(type == URULE){
     if(head)
       rule_attrs.add1(URULE_HEAD, cat1, cat2, cat3, head, Word());
     if(tag)
       rule_attrs.add1(URULE_POS, cat1, cat2, cat3, tag, Word());
    }
    else if(type == BRULE){
      if(head)
        rule_attrs.add1(BRULE_HEAD, cat1, cat2, cat3, head, Word());
      if(tag)
        rule_attrs.add1(BRULE_POS, cat1, cat2, cat3, tag, Word());
    }
    else
      throw NLP::IOException("type should be URULE or BRULE\n");
  }
}

void
Parser::_Impl::_count_rule_dep(const Cat *cat1, const Cat *cat2, const Cat *cat3, const Variable *var1, const Variable *var2,
                        const Words &words1, const Words &words2, const Type type){

  const Position *const end1 = var1->fillers + Variable::NFILLERS;
  const Position *const end2 = var2->fillers + Variable::NFILLERS;
  for(const Position *p = var1->fillers; p != end1 && *p != Variable::SENTINEL; ++p){
    if(!*p)
      continue;

    Word value1 = words1[*p - 1];
    if(!value1)
      continue;

    for(const Position *q = var2->fillers; q != end2 && *q != Variable::SENTINEL; ++q){
      if(!*q)
	continue;

      Word value2 = words2[*q - 1];
      if(!value2)
        continue;

      rule_attrs.add1(type, cat1, cat2, cat3, value1, value2);
    }
  }
}

void
Parser::_Impl::_count_rule(const SuperCat *sc, const Words &words, const Words &tags, const Type type){
  const Cat *cat1 = cats.canonize(sc->left->cat);
  const Cat *cat3 = cats.canonize(sc->cat);
  const Cat *cat2;
  if(type == URULE)
    cat2 = cat3;
  else if(type == BRULE)
    cat2 = cats.canonize(sc->right->cat);
  else
    throw NLP::IOException("should be URULE or BRULE type in _count_rule\n");

  rule_attrs.add1(type, cat1, cat2, cat3, Word(), Word());

  _count_rule_head(cat1, cat2, cat3, &sc->vars[sc->cat->var], words, tags, type);
}

} }
