/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) University of Sydney
// Copyright (c) James R. Curran and Tara Murphy
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"
#include "pool.h"
#include "hashtable/size.h"
#include "bootstrap/bootstrap.h"

using namespace std;

namespace NLP { namespace Bootstrap {

void
Bootstrap::load_terms_(void){
  ifstream lexicon_file(lexicon_filename.c_str());

  std::string term;
  ulong freq;
  while(lexicon_file >> term >> freq){
    Word word = lexicon.add(term);
    Term *term = new (pool, freq) Term(word);
    terms[word.index() - 2] = term;
  }
}

void
Bootstrap::load_ngrams_(void){
  ifstream ngrams_file(ngrams_filename.c_str());

  std::string tmp, w1, w2, w3, w4;
  ulong temp_nterms, freq;
  while(ngrams_file >> w1 >> w2 >> w3 >> w4 >> temp_nterms){
    Template *tmpl = new (pool, temp_nterms) Template(temp_nterms);
    tmpl->tmpl = w1 + ' ' + w2 + ' ' + w3 + ' ' + w4; 
    for(ulong i = 0; i < temp_nterms; ++i){
      ngrams_file >> tmp >> freq;
      Word word = lexicon[tmp];
      ulong index = word.index() - 2;
      ulong lfreq = static_cast<ulong>(log(static_cast<double>(freq))/LOG1p4 + 0.5);
      Term *term = terms[index];
      term->templates[term->nelems++] = tmpl;
      tmpl->ngrams[i] = NGram(index, lfreq);
    }
  }
}

Term *
Bootstrap::get_term(const Raw &raw) const {
  Word word = lexicon[raw];
  if(!word)
    return 0;

  return terms[word.index() - 2];
}

std::string
Bootstrap::get_word(const Term *term) const {
  return term->word.str();
}

void
Bootstrap::words2terms(const Raws &raws, Terms &terms) const {
  terms.clear();
  terms.reserve(raws.size());
  for(Raws::const_iterator i = raws.begin(); i != raws.end(); ++i){
    Term *term = get_term(*i);
    if(term)
      terms.push_back(term);
  }
}

void
Bootstrap::terms2words(const Terms &terms, Raws &raws) const {
  raws.clear();
  raws.reserve(terms.size());
  for(Terms::const_iterator i = terms.begin(); i != terms.end(); ++i)
    raws.push_back(get_word(*i));
}

void
Bootstrap::load_terms(const std::string &filename, Terms &terms, Cat cats) const {
  ifstream file(filename.c_str());
  std::string term_str;

  while(file >> term_str){
    Term *term = get_term(term_str);
    if(term){
      term->cats |= cats;
      term->added = 1;
      terms.push_back(term);
    }else
      cerr << filename << ' ' << term_str << " not in lexicon" << endl;
  }
}

Bootstrap::Bootstrap(const std::string &config)
  : pool(new Pool(HashTable::LARGE)), lexicon("lexicon"){

  ifstream cfg(config.c_str());
  cfg >> nngrams;
  cfg >> ntemplates;
  cfg >> nterms;
  cfg >> lexicon_filename;
  cfg >> ngrams_filename;

  cout << "lexicon = " << lexicon_filename << endl;
  cout << "ngrams = " << ngrams_filename << endl;

  cout << "nngrams = " << nngrams << endl;
  cout << "ntemplates = " << ntemplates << endl;
  cout << "nterms = " << nterms << endl;

  templates = new Template *[ntemplates];
  memset(templates, 0, sizeof(Template *)*ntemplates);

  terms = new Term *[nterms];
  memset(terms, 0, sizeof(Term *)*nterms);

  load_terms_();
  load_ngrams_();
}

Bootstrap::~Bootstrap(void){}

void
Category::get_templates(const ushort ITERATION){
  for(Terms::iterator i = previous.begin(); i != previous.end(); ++i){
    Term *term = *i;
    for(ulong j = 0; j < term->nelems; ++j){
      Template *tmpl = term->templates[j];
      if(tmpl->last)
	continue;

      if(!tmpl->cat){
	tmpls.push_back(tmpl);
	tmpl->cat = cat;
	tmpl->hits = 1;
	tmpl->errs = 0;
      }else if(tmpl->cat != cat)
	tmpl->errs++;
      else
	tmpl->hits++;
    }
  }

  sort(tmpls.begin(), tmpls.end(), HitCmp<Template>());
}

void
Category::get_terms(ushort ITERATION, const ulong NBEST, const ulong DEPTH){
  const ushort LAST = ITERATION + 2;

  ulong best = 0;
  for(ulong i = 0; i != tmpls.size() && i < DEPTH && best < NBEST; ++i){
    Template *tmpl = tmpls[i];

    cerr << ITERATION << ' ' << name << ':' << i << " => " << *tmpl;

    if(tmpl->errs){
      cerr << endl;
      continue;
    }

    ulong actual = 0;
    for(ulong j = 0; j < tmpl->nelems; ++j){
      Term *other = bootstrap.terms[tmpl->ngrams[j].id()];
      if(other->added)
	continue;

      if(!other->cats){
	current.push_back(other);
	actual++;
      }
      other->hits++;
      other->cats |= cat;
    }
    if(actual){
      cerr << " curr " << actual;
      ++best;
    }
    cerr << endl;

    tmpl->last = LAST;
  }

  sort(current.begin(), current.end(), HitCmp<Term>());
}

void
Category::add_terms(const ushort ITERATION, const ulong NBEST, const ulong DEPTH){
  const ushort LAST = ITERATION + 2;

  ulong best = 0;
  for(ulong i = 0; i != current.size() && i < DEPTH && best < NBEST; ++i){
    Term *term = current[i];
    if(term->cats == cat){
      previous.push_back(term);
      term->added = LAST;
      ++best;
    }
  }
}

void
Category::cleanup(const ushort ITERATION){
  const ushort LAST = ITERATION + 2;

  for(Templates::iterator i = tmpls.begin(); i != tmpls.end(); ++i){
    Template *tmpl = *i;

    if(tmpl->errs){
      tmpl->last = LAST;
      cerr << "ERRS " << name << " => " << *tmpl << endl;
      continue;
    }

    for(ulong j = 0; j < tmpl->nelems; ++j)
      if(bootstrap.terms[tmpl->ngrams[j].id()]->added == LAST){
	tmpl->last = 0;
	break;
      }

    if(tmpl->last)
      cerr << "USED " << name << " => " << *tmpl << endl;
    else
      tmpl->cat = 0;
  }
  tmpls.clear();

  for(Terms::iterator i = current.begin(); i != current.end(); ++i){
    (*i)->hits = 0;
    if(!(*i)->added)
      (*i)->cats = 0;
  }
  current.clear();
}

} }
