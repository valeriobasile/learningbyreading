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

struct Rule {
  const Type type;
  const Cat *cat1;
  const Cat *cat2;
  const Cat *cat3;
  const Word value1;
  const Word value2;

  Rule(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
       Word value1, Word value2)
    : type(type), cat1(cat1), cat2(cat2), cat3(cat3),
      value1(value1), value2(value2){}

  Hash hash(void) const {
    Hash h(static_cast<ulong>(value1));
    h += static_cast<ulong>(value2);
    h += cat1->rhash;
    h += cat2->rhash;
    h += cat3->rhash;
    h += type;
    return h;
  }

  bool equal(const Rule &other){
    return other.value1 == value1 && other.value2 == value2 &&
      eq(other.cat1, cat1) && eq(other.cat2, cat2) && eq(other.cat3, cat3) &&
      other.type == type;
  }
};

static void
print_rule_entries(const AttribEntry<Rule> *entry, ostream &out){
  for(const AttribEntry<Rule> *l = entry; l; l = l->next){
    out << l->id << ' ';
    int flag = 0;
    switch(l->value.type){
    case URULE: out << "m "; break;
    case BRULE: out << "n "; break;
    case URULE_HEAD: out << "p "; flag = 1; break;	
    case URULE_POS: out << "r "; flag = 1; break;
    case BRULE_HEAD: out << "q "; flag = 1; break;
    case BRULE_POS: out << "s "; flag = 1; break;
    case BRULE_HEAD_HEAD: out << "t "; flag = 2; break;
    case BRULE_POS_HEAD: out << "u "; flag = 2; break;
    case BRULE_HEAD_POS: out << "v "; flag = 2; break;
    case BRULE_POS_POS: out << "w "; flag = 2; break;
    default:
      throw NLP::IOException("unexpected feature type\n");
    }
    const bool outerbrack = true;
    l->value.cat1->out_novar(out, outerbrack);
    out << ' ';
    l->value.cat2->out_novar(out, outerbrack);
    out << ' ';
    l->value.cat3->out_novar(out, outerbrack);

    if(flag != 0)
      out << ' ' << l->value.value1.str();
    if(flag == 2)
      out << ' ' << l->value.value2.str();
    out << '\n';
  }
}

class RuleAttributes::Impl: public AttributesImpl<Rule, MEDIUM, LARGE>, public Shared {
public:
  Impl(void): AttributesImpl<Rule, MEDIUM, LARGE>("RuleAttributes"){}

  void print_entries(ostream &out){
    for(ulong i = 0; i < NBUCKETS_; i++){
      if(buckets_[i])
	print_rule_entries(buckets_[i], out);
    }
  }
};

RuleAttributes::RuleAttributes(void): _impl(new Impl) {}
RuleAttributes::RuleAttributes(RuleAttributes &other): _impl(share(other._impl)) {}
RuleAttributes::~RuleAttributes(void){ release(_impl); }

size_t RuleAttributes::size(void) const { return _impl->size; }
void
RuleAttributes::set_weights(const double *weights){
  _impl->set_weights(weights);
}

ulong
RuleAttributes::get_id(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		       Word value1, Word value2) const {
  Rule rule(type, cat1, cat2, cat3, value1, value2);
  return _impl->find_id(rule);
}

double
RuleAttributes::weight(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		       Word value1, Word value2) const {
  Rule rule(type, cat1, cat2, cat3, value1, value2);
  return _impl->find_weight(rule);
}

void
RuleAttributes::insert(ulong id, Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		       Word value1, Word value2) {
  Rule rule(type, cat1, cat2, cat3, value1, value2);
  return _impl->insert(rule, id);
}

void
RuleAttributes::add1(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		     Word value1, Word value2) {
  Rule rule(type, cat1, cat2, cat3, value1, value2);
  _impl->add(rule)->id++;
}

void
RuleAttributes::print_entries(ostream &out){
  _impl->print_entries(out);
}


} }
