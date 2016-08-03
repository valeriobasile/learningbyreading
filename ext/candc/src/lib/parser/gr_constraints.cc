// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Lexicon
// stores canonical strings and their frequencies
// used in many places in the system for lexicons and
// counting other kinds of strings such as classes,
// postags and tag-dict pairs in the Extraction classes

// canonical strings (i.e. one instance of each string
// seen in the training data) are an important efficiency
// strategy because they reduce the number of feature
// lookups on features involving words (because we know
// that if we haven't seen the word, then we haven't seen
// any features involving the word).  It also makes string
// comparisons in these feature lookups fast because we
// can compare string pointers directly.

// canonical strings are wrapped in Word objects to distinguish
// them from regular const char * values in the code

#include "base.h"

#include "parser/gr_constraints.h"

#include "hashtable/base.h"
#include "hashtable/ordered.h"

#include "share.h"

using namespace std;
using namespace NLP::HashTable;

namespace NLP { namespace CCG {

typedef Ordered<Entry<ulong>, const string &, MEDIUM, LARGE> ImplBase;

class GRConstraints::Impl_: public ImplBase, public Shared {
public:
  Impl_(const string &name)
    : ImplBase(name){}
  virtual ~Impl_(void){}
};

GRConstraints::GRConstraints(const string &name)
  : impl_(new Impl_(name)){}

GRConstraints::GRConstraints(const GRConstraints &other)
  : impl_(share(other.impl_)){}

GRConstraints &
GRConstraints::operator=(const GRConstraints &other){
  if(impl_ != other.impl_){
    release(impl_);
    impl_ = share(other.impl_);
  }

  return *this;
}

GRConstraints::~GRConstraints(void){
  release(impl_);
}

const string &
GRConstraints::name(void) const {
  return impl_->name;
}

size_t
GRConstraints::size(void) const {
  return impl_->size;
}

void
GRConstraints::add(const string &label, const string &word){
  impl_->add(label + ' ' + word);
}

bool
GRConstraints::get(const string &label, const string &word) const {
  return impl_->find(label + ' ' + word);
}

} }

