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

#include "hashtable/base.h"
#include "hashtable/ordered.h"

#include "huge.h"

#include "share.h"

using namespace std;

namespace NLP {

using namespace HashTable;

typedef Ordered<Entry<ulong>, const string &, LARGE, MASSIVE> ImplBase;

class Huge::Impl_: public ImplBase, public Shared {
public:
  string PREFACE;

  Impl_(const string &name)
    : ImplBase(name){}

  Impl_(const string &name, const string &filename)
    : ImplBase(name){
    load(filename, false);
  }
  virtual ~Impl_(void) {}

  void insert(const string &word, ulong freq){
    ImplBase::insert(word)->value = freq;
  }

  using ImplBase::add;
  void add(const string &word, ulong freq){
    ImplBase::add(word)->value += freq;
  }

  void load(const string &filename, const bool ADD){
    ulong nlines = 0;
    ifstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open lexicon file", filename);

    PREFACE += read_preface(filename, stream, nlines);

    string word;
    ulong freq;
    while(stream >> word >> freq){
      ++nlines;
      if(stream.get() != '\n')
	throw IOException("expected newline after frequency in lexicon file", filename, nlines);
      if(ADD)
	add(word, freq);
      else
	insert(word, freq);
    }

    if(!stream.eof())
      throw IOException("could not parse word or frequency information", filename, nlines);
  }

  // remove entries less than freq and then call HashTable::Count::compress
  // to eliminate the gaps created in the entry list by removing entries
  // doesn't remove entries from the chains, only the entry list
  // so this is mainly used just before dumping out to disk in Extract classes
  void
  apply_cutoff(ulong freq){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->value < freq)
        *i = 0;
    compress();
  }

  // reset the lexicon to an initial empty state
  // clears up the buckets, entries list and memory pools
  void clear(void){
    size = 0;
    memset(buckets_, 0, sizeof(buckets_));
    pool_->clear();
    entries.resize(0);
  }
};

Huge::Huge(const string &name)
  : impl_(new Impl_(name)), PREFACE(impl_->PREFACE){}

Huge::Huge(const string &name, const string &filename)
  : impl_(new Impl_(name, filename)), PREFACE(impl_->PREFACE){}

Huge::Huge(const Huge &other)
  : impl_(share(other.impl_)), PREFACE(impl_->PREFACE){}

Huge &
Huge::operator=(const Huge &other){
  if(impl_ != other.impl_){
    release(impl_);
    impl_ = share(other.impl_);
  }

  return *this;
}

Huge::~Huge(void){
  release(impl_);
}

const string &
Huge::name(void) const {
  return impl_->name;
}

size_t
Huge::size(void) const {
  return impl_->size;
}

void
Huge::clear(void){
  impl_->clear();
}

void
Huge::load(const string &filename){
  impl_->load(filename, true);
}

void
Huge::save(const string &filename, const string &preface) const {
  ofstream stream(filename.c_str());
  if(!stream)
    throw IOException("could not open " + impl_->name + " file for writing", filename);

  save(stream, preface);
}

void
Huge::save(ostream &stream, const string &preface) const {
  stream << preface << endl;

  impl_->save(stream);
}

Word
Huge::add(const string &str){
  return Word(reinterpret_cast<ulong>(impl_->add(str)));
}

void
Huge::add(const string &str, ulong freq){
  impl_->add(str, freq);
}

void
Huge::insert(const string &str, ulong freq){
  impl_->insert(str, freq);
}

Word
Huge::can(const string &str) const {
  return Word(reinterpret_cast<ulong>(impl_->find(str)));
}

void
Huge::can(const RawWords &raws, Words &words) const {
  words.resize(0);
  words.reserve(raws.size());
  for(RawWords::const_iterator i = raws.begin(); i != raws.end(); ++i)
    words.push_back(Word(reinterpret_cast<ulong>(impl_->find(*i))));
}

void
Huge::can(const RawWords &raws, OffsetWords &words) const {
  words.resize_buffer(0);
  words.reserve(raws.size());
  words.pad_front(SENTINEL);
  for(RawWords::const_iterator i = raws.begin(); i != raws.end(); ++i)
    words.push_back(Word(reinterpret_cast<ulong>(impl_->find(*i))));
  words.pad_back(SENTINEL);
}

Word
Huge::check(const string &str) const {
  Entry<ulong> *entry = impl_->find(str);
  if(!entry)
    throw Exception("the string '" + str + "' is not a member of " + impl_->name);

  return Word(reinterpret_cast<ulong>(entry));
}

ulong
Huge::freq(const string &str) const {
  Entry<ulong> *entry = impl_->find(str);
  if(entry)
    return entry->value;

  return 0;
}

void
Huge::sort_by_alpha(void){
  impl_->sort_by_alpha();
}

void
Huge::sort_by_freq(void){
  impl_->sort_by_value();
}

void
Huge::sort_by_rev_freq(void){
  impl_->sort_by_rev_value();
}

void
Huge::apply_cutoff(ulong freq){
  impl_->apply_cutoff(freq);
}

Word
Huge::attribute_check(const std::string &str) const {
  if(str[0] == '_' && str[1] == '_'){
    if(str == None::str)
      return NONE;
    else if(str == Sentinel::str)
      return SENTINEL;
  }
  return check(str);
}

Word
Huge::attribute_can(const std::string &str) const {
  if(str[0] == '_' && str[1] == '_'){
    if(str == None::str)
      return NONE;
    else if(str == Sentinel::str)
      return SENTINEL;
  }
  return can(str);
}

Word
Huge::attribute_add(const std::string &str){
  if(str[0] == '_' && str[1] == '_'){
    if(str == None::str)
      return NONE;
    else if(str == Sentinel::str)
      return SENTINEL;
  }
  return add(str);
}

void
Huge::print_stats(ostream &out) const {
  out << "statistics for the lexicon hash table" << endl;
  out << "-------------------------------------" << endl;
  impl_->printstats(out);
}

}
