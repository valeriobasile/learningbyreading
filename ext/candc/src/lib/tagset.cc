// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::TagSet
// translate between the names of tags and classes (strings)
// and their internal representation, which is an enumeration
// wrapped in Tag objects

#include "base.h"

#include "hashtable/base.h"
#include "hashtable/ordered.h"

#include "share.h"

using namespace std;

namespace NLP {

using namespace HashTable;

typedef Ordered<Entry<ulong>, const string &, TINY, TINY> ImplBase;

class TagSet::Impl_: public ImplBase, public Shared {
public:
  string PREFACE;

  Impl_(const string &name)
    : ImplBase(name){
    insert(None::str, 0);
    insert(Sentinel::str, 0);
  }
  Impl_(const string &name, const string &filename)
    : ImplBase(name){
    insert(None::str, 0);
    insert(Sentinel::str, 0);
    load(filename, false);
  }
  virtual ~Impl_(void){}

  using ImplBase::insert;
  void insert(const string &word, ulong freq){
    ImplBase::insert(word)->value = freq;
  }

  Tag add(const string &word, ulong freq = 1){
    Entry *entry = ImplBase::add(word);
    entry->value += freq;
    return static_cast<ushort>(entry->index);
  }

  void load(const string &filename, const bool ADD){
    ulong nlines = 0;
    ifstream stream(filename.c_str());
    if(!stream)
      throw NLP::IOException("could not open classes file", filename);

    PREFACE += read_preface(filename, stream, nlines);

    string tag;
    ulong freq;
    while(stream >> tag >> freq){
      ++nlines;
      if(stream.get() != '\n')
	throw IOException("expected newline after frequency in classes file", filename, nlines);

      if(ADD)
	add(tag, freq);
      else
	insert(tag, freq);
    }

    if(!stream.eof())
      throw IOException("could not parse class", filename, nlines);
  }
};

TagSet::TagSet(const string &name)
  : impl_(new Impl_(name)), PREFACE(impl_->PREFACE){}

TagSet::TagSet(const string &name, const string &filename)
  : impl_(new Impl_(name, filename)), PREFACE(impl_->PREFACE){}

TagSet::TagSet(const TagSet &other)
  : impl_(share(other.impl_)), PREFACE(impl_->PREFACE){}

TagSet &
TagSet::operator=(TagSet &other){
  if(impl_ != other.impl_){
    release(impl_);
    impl_ = share(other.impl_);
  }

  return *this;
}

TagSet::~TagSet(void){
  release(impl_);
}

const string &
TagSet::name(void) const {
  return impl_->name;
}

size_t
TagSet::size(void) const {
  return impl_->entries.size();
}

const char *
TagSet::can(const Tag tag) const {
  return impl_->entries[tag.value()]->str;
}

string
TagSet::str(const Tag tag) const {
  return impl_->entries[tag.value()]->str;
}

string
TagSet::str_checked(const Tag tag) const {
  if(tag.value() >= size())
    throw Exception("tag value is out of range of " + impl_->name);

  return impl_->entries[tag.value()]->str;
}

Tag
TagSet::tag(const string &str) const {
  Entry<ulong> *entry = impl_->find(str);
  if(!entry)
    return NONE;

  return static_cast<ushort>(entry->index);
}

Tag
TagSet::check(const string &str) const {
  Entry<ulong> *entry = impl_->find(str);
  if(!entry)
    throw Exception("tag '" + str + "' is not a member of " + impl_->name);

  return static_cast<ushort>(entry->index);
}

string
TagSet::load_tags(const string &filename, Tags &tags) const {
  ifstream stream(filename.c_str());
  if(!stream)
    throw IOException("could not open tags file", filename);

  ulong nlines = 0;
  string preface = read_preface(filename, stream, nlines);

  try {
    string str;
    while(stream >> str)
      tags.push_back(check(str));
  }catch(Exception e){
    throw IOException(e.msg, filename);
  }

  return preface;
}

void
TagSet::tag(const RawTags &raws, Tags &tags) const {
  tags.resize(0);
  tags.reserve(raws.size());
  for(RawTags::const_iterator i = raws.begin(); i != raws.end(); ++i)
    tags.push_back(tag(*i));
}

void
TagSet::tag(const RawTags &raws, OffsetTags &tags) const {
  tags.resize_buffer(0);
  tags.reserve(raws.size());
  tags.pad_front(SENTINEL);
  for(RawTags::const_iterator i = raws.begin(); i != raws.end(); ++i)
    tags.push_back(tag(*i));
  tags.pad_back(SENTINEL);
}

void
TagSet::str(const Tags &tags, RawTags &raws) const {
  raws.resize(0);
  raws.reserve(tags.size());
  for(Tags::const_iterator i = tags.begin(); i != tags.end(); ++i)
    raws.push_back(str(*i));
}

void
TagSet::str(const MultiTags &tags, MultiRaws &raws) const {
  raws.resize(tags.size());
  for(ulong i = 0; i != tags.size(); ++i){
    const MultiTag &mtag = tags[i];
    MultiRaw &mraw = raws[i];

    mraw.resize(0);
    mraw.reserve(mtag.size());
    for(ulong j = 0; j != mtag.size(); ++j)
      mraw.push_back(ScoredRaw(str(mtag[i].tag), mtag[i].score));
  }
}

void
TagSet::add(const string &str){
  impl_->add(str);
}

void
TagSet::insert(const string &str, ulong freq){
  impl_->insert(str, freq);
}

void
TagSet::load(const string &filename){
  impl_->load(filename, true);
}

}
