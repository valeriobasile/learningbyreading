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

struct Dependency {
  const Word head;
  const Word var;

  const uchar type;
  const RuleID rule;
  const RelID rel;
  const CatID lrange;
  const ushort dist;

  Dependency(Type type, Word head, RelID rel, Word var, RuleID rule,
	     CatID lrange, ushort dist)
    : head(head), var(var), type((uchar)type), rule(rule), rel(rel), lrange(lrange),
      dist(dist){}

  Hash hash(void) const {
    Hash h(static_cast<ulong>(head));
    h += static_cast<ulong>(var);
    h += rel;
    h += rule + lrange + type;
    h += dist;
    return h;
  }

  bool equal(const Dependency &other) const {
    return other.head == head && other.var == var && other.rel == rel &&
      other.rule == rule && other.lrange == lrange && other.dist == dist &&
      other.type == type;
  }
};

class DependencyAttributes::Impl: public AttributesImpl<Dependency, MEDIUM, LARGE>, public Shared {
public:
  Impl(void): AttributesImpl<Dependency, MEDIUM, LARGE>("DependencyAttributes"){}
};

DependencyAttributes::DependencyAttributes(void): _impl(new Impl) {}
DependencyAttributes::DependencyAttributes(DependencyAttributes &other): _impl(share(other._impl)) {}
DependencyAttributes::~DependencyAttributes(void){ release(_impl); }

size_t DependencyAttributes::size(void) const { return _impl->size; }
void
DependencyAttributes::set_weights(const double *weights){
  _impl->set_weights(weights);
}

ulong
DependencyAttributes::get_id(Type type, Word head, RelID rel, Word var, RuleID rule,
			     CatID lrange, ushort dist) const {
  Dependency dep(type, head, rel, var, rule, lrange, dist);
  return _impl->find_id(dep);
}

double
DependencyAttributes::weight(Type type, Word head, RelID rel, Word var, RuleID rule,
			     CatID lrange, ushort dist) const {
  Dependency dep(type, head, rel, var, rule, lrange, dist);
  return _impl->find_weight(dep);
}

void
DependencyAttributes::insert(ulong id, Type type, Word head, RelID rel, Word var, RuleID rule,
			     CatID lrange, ushort dist) {
  Dependency dep(type, head, rel, var, rule, lrange, dist);
  return _impl->insert(dep, id);
}

} }
