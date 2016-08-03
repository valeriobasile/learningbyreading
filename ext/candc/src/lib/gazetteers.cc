// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Gazettteers
// allows fast lookup of up to 32 gazetteers simultaneously
// this number could be larger if the internal representation is changed
// each word is represented using a bit vector indicating which gazetteers
// it is a member of

#include "base.h"

#include "hashtable/base.h"

#include "gazetteers.h"
#include "share.h"

using namespace std;

namespace NLP {

using namespace HashTable;

typedef Base<Entry<GazFlags>, const string &, LARGE, LARGE> ImplBase;

class Gazetteers::Impl_: public ImplBase, public Shared {
public:
  const string config;

  Impl_(const string &name)
    : ImplBase(name){}
  Impl_(const string &name, const string &dir, const string &config)
    : ImplBase(name), config(config){
    load(dir, config);
  }
  virtual ~Impl_(void){ /* do nothing */ }

  // bitwise or the new flags with the existing values if the word already exists
  // otherwise, create a new entry with the specified flags
  void add(const string &str, GazFlags flags){
    ImplBase::add(str)->value |= flags;
  }

  // return the existing flags or zero
  GazFlags find(const string &str) const {
    Entry *e = ImplBase::find(str);
    if(e)
      return e->value;
    else
      return 0;
  }

  // load gazetteers from the base directory dir and the configuration
  // file config (which stores the type, flags and filename for gazetteer
  // resources)
  void load(const string &dir, const string &config){
    ifstream stream(config.c_str());
    if(!stream)
      throw IOException("could not open gazetteer configuration file", config);

    string type, filename;
    GazFlags index;
    while(stream >> type >> index >> filename){
      if(filename[0] != '/')
        filename = dir + '/' + filename;
      load(type, filename, 1 << index);
    }
  };

  // load a single gazetteer file with a given set of flags
  // a gazetteer file consists of a list of words one per line
  void load(const string, const string &filename, GazFlags flags){
    ifstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open gazetteers file ", filename);

    string word;
    while(stream >> word)
      add(word, flags);

    if(!stream.eof())
      throw IOException("could not parse class", filename, size);
  }
};

Gazetteers::Gazetteers(const string &name)
  : impl_(new Impl_(name)){}
Gazetteers::Gazetteers(const string &name, const string &dir, const string &config)
  : impl_(new Impl_(name, dir, config)){}
Gazetteers::Gazetteers(const Gazetteers &other)
  : impl_(share(other.impl_)){}

Gazetteers &
Gazetteers::operator=(const Gazetteers &other){
  if(impl_ != other.impl_){
    release(impl_);
    impl_ = share(other.impl_);
  }

  return *this;
}

Gazetteers::~Gazetteers(void){
  release(impl_);
}

const string
Gazetteers::name(void) const {
  return impl_->name;
}

const string
Gazetteers::config(void) const {
  return impl_->config;
}

size_t
Gazetteers::size(void) const {
  return impl_->size;
}

void
Gazetteers::load(const string &type, const string &filename, GazFlags flags){
  impl_->load(type, filename, flags);
}

void
Gazetteers::add(const string &str, GazFlags flags){
  impl_->add(str, flags);
}

GazFlags
Gazetteers::exists(const string &str) const {
  return impl_->find(str);
}

GazFlags
Gazetteers::lower(const string &str) const {
  string buffer;
  buffer.reserve(str.size() + 1);
  for(string::const_iterator i = str.begin(); i != str.end(); ++i)
    buffer += tolower(*i);

  return exists(buffer);
}

}
