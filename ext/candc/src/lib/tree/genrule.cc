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

struct GenRule {
  const Cat *cat;
  ushort rule;

  GenRule(const Cat *cat, ushort rule)
    : cat(cat), rule(rule){}

  Hash hash(void) const {
    Hash h((ulong)rule);
    h += cat->rhash;
    return h;
  }

  bool equal(const GenRule &other) const {
    return other.rule == rule && eq(other.cat, cat);
  }
};

class GenRuleAttributes::Impl: public AttributesImpl<GenRule, MEDIUM, LARGE>, public Shared {
public:
  Impl(void): AttributesImpl<GenRule, MEDIUM, LARGE>("GenRuleAttributes"){}
};

GenRuleAttributes::GenRuleAttributes(void): _impl(new Impl) {}
GenRuleAttributes::GenRuleAttributes(GenRuleAttributes &other): _impl(share(other._impl)) {}
GenRuleAttributes::~GenRuleAttributes(void){ release(_impl); }

size_t GenRuleAttributes::size(void) const { return _impl->size; }
void
GenRuleAttributes::set_weights(const double *weights){
  _impl->set_weights(weights);
}

ulong
GenRuleAttributes::get(const Cat *cat, ushort rule) const {
  GenRule genrule(cat, rule);
  return _impl->find_id(genrule);
}

double
GenRuleAttributes::weight(const Cat *cat, ushort rule) const {
  GenRule genrule(cat, rule);
  return _impl->find_weight(genrule);
}

void
GenRuleAttributes::insert(ulong id, const Cat *cat, ushort rule) {
  GenRule genrule(cat, rule);
  return _impl->insert(genrule, id);
}

} }
