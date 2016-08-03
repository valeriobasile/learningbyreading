// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "tree/_baseimpl.h"

using namespace std;

namespace NLP { namespace Tree {

using namespace NLP::HashTable;
using namespace NLP::CCG;

struct Distance {
  const RelID rel;
  const CatID lrange;
  const RuleID rule;
  const Dir dir;
  const VerbDistance nverbs;
  const PunctDistance npunct;
  const ConjDistance nconj;

  Distance(RelID rel, RuleID rule, CatID lrange, Dir dir,
	   VerbDistance nverbs, PunctDistance npunct, ConjDistance nconj)
    : rel(rel), lrange(lrange), rule(rule), dir(dir),
      nverbs(nverbs), npunct(npunct), nconj(nconj){}

  Hash hash(void) const {
    Hash h((ulong)rel);
    h += rule;
    h += lrange*2 + dir;
    return h;
  }

  bool equal(const Distance &other){
    return other.rel == rel && other.dir == dir && other.rule == rule && 
      other.lrange == lrange && other.nverbs == nverbs && other.npunct == npunct &&
      other.nconj == nconj;
  }
};

class DistanceAttributes::Impl: public AttributesImpl<Distance, MEDIUM, LARGE>, public Shared {
public:
  Impl(void): AttributesImpl<Distance, MEDIUM, LARGE>("DistanceAttributes"){}
};

DistanceAttributes::DistanceAttributes(void): _impl(new Impl) {}
DistanceAttributes::DistanceAttributes(DistanceAttributes &other): _impl(share(other._impl)) {}
DistanceAttributes::~DistanceAttributes(void){ release(_impl); }

size_t DistanceAttributes::size(void) const { return _impl->size; }
void DistanceAttributes::set_weights(const double *weights){
  _impl->set_weights(weights);
}

ulong
DistanceAttributes::get_id(RelID rel, RuleID rule, CatID lrange, Dir dir,
			   VerbDistance nverbs, PunctDistance npunct,
			   ConjDistance nconj) const {
  Distance dist(rel, rule, lrange, dir, nverbs, npunct, nconj);
  return _impl->find_id(dist);
}

double
DistanceAttributes::weight(RelID rel, RuleID rule, CatID lrange, Dir dir,
			   VerbDistance nverbs, PunctDistance npunct,
			   ConjDistance nconj) const {
  Distance dist(rel, rule, lrange, dir, nverbs, npunct, nconj);
  return _impl->find_weight(dist);
}

void
DistanceAttributes::insert(ulong id, RelID rel, RuleID rule, CatID lrange, Dir dir,
			   VerbDistance nverbs, PunctDistance npunct, ConjDistance nconj) {
  Distance dist(rel, rule, lrange, dir, nverbs, npunct, nconj);
  _impl->insert(dist, id);
}

} }
