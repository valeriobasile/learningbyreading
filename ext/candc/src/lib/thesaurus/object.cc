// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <cmath>
#include <string>
#include <vector>
#include <algorithm>

#include <iostream>
#include <iomanip>
#include <stdexcept>

using namespace std;

#include "except.h"
#include "utils.h"
#include "utils/aux_heap.h"
#include "utils/io.h"

#include "hash.h"
#include "pool.h"
#include "hashtable/entry.h"

#include "thesaurus/options.h"
#include "thesaurus/type.h"
#include "thesaurus/types.h"
#include "thesaurus/attribute.h"
#include "thesaurus/attributes.h"
#include "thesaurus/weight.h"
#include "thesaurus/relation.h"
#include "thesaurus/object.h"

namespace NLP { namespace Thesaurus {

float Object::ftotal = 0;
ulong Object::ntotal = 0;
float Object::fcutoff = 0;
ulong Object::ncutoff = 0;

void
Object::compact(void){
  Relations tmp(relations);
  relations.swap(tmp);
}

void
Object::optimize(const Options &op){
  RelationLTZero filter(Relation::nzero, Relation::fzero);

  Relations::iterator rubbish = remove_if(relations.begin(), relations.end(), filter);
  if(rubbish != relations.end())
    relations.erase(rubbish, relations.end());
};

void
Object::heuristic(const Options &op){
  const ulong MAX_CANONICAL = op.heuristic_size;
  const float CUTOFF = op.heuristic_fmin;
  const ulong CEILING = op.heuristic_anmax;

  canonical.reserve(MAX_CANONICAL);
  Relations::iterator i = relations.begin();
  for( ; i != relations.end() && canonical.size() < MAX_CANONICAL; i++){
    if(i->freq() < CUTOFF) continue;
    if(!i->isverb() && op.heuristic_verbs) continue;
    if(i->an() >= CEILING) continue;

    canonical.push_back(&*i);
  }

  make_heap(canonical.begin(), canonical.end(), RelationGTComp());

  for( ; i != relations.end(); i++){
    if(i->freq() < CUTOFF) continue;
    if(!i->isverb() && op.heuristic_verbs) continue;
    if(i->an() >= CEILING) continue;

    if(canonical[0]->score() < i->score()){
      canonical[0] = &*i;
      decrease_key_heap(canonical.begin(), canonical.end(), canonical.begin(), RelationGTComp());
    }
  }
  ::sort(canonical.begin(), canonical.end(), RelationAlphaComp());
};

void
Object::global_dump_2(std::ostream &out){
  dump_ulong(out, ntotal);
  dump_float(out, ftotal);
  dump_ulong(out, ncutoff);
  dump_float(out, fcutoff);
}

void
Object::global_load_2(std::istream &in, const std::string &fname){
  if(!load_ulong(in, ntotal))
    throw NLP::IOException("could not load Object::ntotal", fname, in.tellg());
  if(!load_float(in, ftotal))
    throw NLP::IOException("could not load Object::ftotal", fname, in.tellg());
  if(!load_ulong(in, ncutoff))
    throw NLP::IOException("could not load Object::ncutoff", fname, in.tellg());
  if(!load_float(in, fcutoff))
    throw NLP::IOException("could not load Object::fcutoff", fname, in.tellg());
}

void
Object::dump_1(std::ostream &out) const {
  dump_string(out, str());
  dump_ulong(out, relations.size());
  for(Relations::const_iterator i = relations.begin(); i != relations.end(); i++)
    i->dump(out);
}

void
Object::load_1(Attributes attributes, std::istream &in, const std::string &fname){
  ulong nrelations;
  if(!load_ulong(in, nrelations))
    throw NLP::IOException("could not load object number of relations", fname, in.tellg());

  relations.reserve(nrelations);
  for(ulong i = 0; i < nrelations; i++)
    add(Relation::Load(attributes, in, fname));
}

void
Object::dump_2(std::ostream &out) const {
  dump_1(out);

  dump_ulong(out, _n);
  dump_float(out, _freq);
}

void
Object::load_2(Attributes attributes, std::istream &in, const std::string &fname){
  load_1(attributes, in, fname);

  if(!load_ulong(in, _n))
    throw NLP::IOException("could not load Object::n", fname, in.tellg());
  if(!load_float(in, _freq))
    throw NLP::IOException("could not load Object::freq", fname, in.tellg());
}

} }
