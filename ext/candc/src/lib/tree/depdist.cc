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

struct DepDist {
  const Type type;
  const Cat *cat1;
  const Cat *cat2;
  const Cat *cat3;
  const Word head;
  const ushort count;

  DepDist(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
	  Word head, ushort count)
    : type(type), cat1(cat1), cat2(cat2), cat3(cat3),
      head(head), count(count){}

  Hash hash(void) const {
    Hash h(static_cast<ulong>(head));
    h += (ulong)count;
    h += cat1->rhash;
    h += cat2->rhash;
    h += cat3->rhash;
    h += type;
    return h;
  }

  bool equal(const DepDist &other) const {
    return other.head == head && eq(other.cat1, cat1) && eq(other.cat2, cat2) &&
           eq(other.cat3, cat3) && other.count == count && other.type == type;
  }
};

class DepDistAttributes::Impl: public AttributesImpl<DepDist, MEDIUM, LARGE>, public Shared {
public:
  Impl(void): AttributesImpl<DepDist, MEDIUM, LARGE>("DepDistAttributes"){}
};

DepDistAttributes::DepDistAttributes(void): _impl(new Impl) {}
DepDistAttributes::DepDistAttributes(DepDistAttributes &other): _impl(share(other._impl)) {}
DepDistAttributes::~DepDistAttributes(void){ release(_impl); }

size_t DepDistAttributes::size(void) const { return _impl->size; }
void
DepDistAttributes::set_weights(const double *weights){
  _impl->set_weights(weights);
}

ulong
DepDistAttributes::get_id(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
			  Word head, ushort count) const {
  DepDist depdist(type, cat1, cat2, cat3, head, count);
  return _impl->find_id(depdist);
}

double
DepDistAttributes::weight(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
			  Word head, ushort count) const {
  DepDist depdist(type, cat1, cat2, cat3, head, count);
  return _impl->find_weight(depdist);
}

void
DepDistAttributes::insert(ulong id, Type type, const Cat *cat1, const Cat *cat2,
			  const Cat *cat3, Word head, ushort count) {
  DepDist depdist(type, cat1, cat2, cat3, head, count);
  _impl->insert(depdist, id);
}

} }
