// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Extract::Context and NLP::Extract::Attributes
// storage for contexts, and features and attributes in
// the model extraction process

#include "base.h"

#include "config/config.h"

#include "hashtable/base.h"
#include "hashtable/ordered.h"

#include "model/types.h"

#include "model/registry.h"

#include "extract/feature.h"
#include "extract/attributes.h"

#include "cluster.h"

#include "share.h"

using namespace std;

namespace NLP { namespace Extract {

using namespace HashTable;
using namespace Model;

// custom hash function and hash table entry for storing attributes
// this stores the tuple (type, value, freq), the attribute index
// which becomes the attribute id.  It also stores a vector of the
// features which have this attribute value.
class _AttributeEntry {
private:
  _AttributeEntry(const char *type, _AttributeEntry *next)
    : next(next), index(0), value(0), type(type) {}
  ~_AttributeEntry(void){}

  void *operator new(size_t size, Pool *pool, size_t len){
    return (void *)pool->alloc(size + aligned_size(len, sizeof(_AttributeEntry)));
  }

  void operator delete(void *, Pool *, size_t) { /* do nothing */ }
public:
  static Hash hash(const char *type, const string &str){
    Hash hash(type);
    hash += ' ';
    hash += str;
    return hash;
  }

  static _AttributeEntry *create(Pool *, const string &,
				 ulong, NLP::Hash, _AttributeEntry *){ return 0; }
  static _AttributeEntry *create(Pool *pool, const char *type,
				 const string &str, _AttributeEntry *next){
    _AttributeEntry *entry = new (pool, get_remainder(str.size())) _AttributeEntry(type, next);
    strcpy(entry->str, str.c_str());
    return entry;
  }

  _AttributeEntry *next;
  ulong index;
  ulong value;
  Features features;
  const char *type;
  char str[MIN_STR_BUFFER];

  void update_counts(void){
    value = 0;
    for(Features::iterator i = features.begin(); i != features.end(); ++i)
      value += i->freq;
  }

  void update_features(const Feature *feats, ulong nfeats){
    features.assign(feats, feats + nfeats);
    update_counts();
  }

  void insert(Tag tag){
    features.push_back(Feature(tag, 1));
    ++value;
  }

  void inc(Tag tag){
    for(Features::iterator i = features.begin(); i != features.end(); ++i)
      if(i->tag == tag){
        ++value;
        ++i->freq;
        return;
      }

    insert(tag);
  }

  bool equal(const char *type, const string &str) const {
    return type == this->type && str == this->str;
  }

  _AttributeEntry *find(const char *type, const string &str){
    for(_AttributeEntry *l = this; l; l = l->next)
      if(l->equal(type, str))
        return l;

    return 0;
  }

  bool find(const char *type, const string &str, ulong &id) const {
    for(const _AttributeEntry *l = this; l; l = l->next)
      if(l->equal(type, str) && l->value){
        id = l->index - 1;
        return l->index != 0;
      }

    return false;
  }

  // apply a minimum frequency cutoff to the features in this Attribute
  // if the feature frequency < freq, the frequency is set to zero
  // and so it is ignored in later processing
  bool cutoff(ulong freq){
    for(Features::iterator i = features.begin(); i != features.end(); ++i)
      if(i->freq < freq){
        value -= i->freq;
        i->freq = 0;
      }

    return value == 0;
  }

  // dump the attribute information
  void save_attributes(ostream &stream) const {
    assert(index != 0);
    stream << type << ' ' << str << ' ' << value << '\n';
  }

  // sort the features by class and then dump
  void save_features(ostream &stream){
    assert(index != 0);
    sort(features.begin(), features.end(), FeatureCmp());
    for(Features::const_iterator i = features.begin(); i != features.end(); ++i)
      // skip features with frequency zero (i.e. those below the cutoff)
      if(i->freq)
        stream << i->tag.value() << ' ' << (index - 1) << ' ' << i->freq << '\n';
  }

  // calculate the total number of features by type
  ulong nfeatures(void) const {
    assert(index != 0);
    ulong total = 0;
    for(Features::const_iterator i = features.begin(); i != features.end(); ++i)
      if(i->freq)
        total++;
    return total;
  }

  // count the hash table chain length
  ulong nchained(void){
    return next ? next->nchained() + 1 : 1;
  }
  
  void map_load(Cluster::KeyValue *kv) const {
    for(Features::const_iterator i = features.begin(); i != features.end(); ++i){
      if(i->freq == 0)
        continue;
      ostringstream oss;
      oss << i->tag.value() << ' ' << type << ' ' << str;
      kv->add(oss.str(), i->freq);
    }
  }
};

template <class E>
class RevAttribCmp {
public:
  bool operator ()(const E *const e1, const E *const e2){
    if(e1->value != e2->value)  
      return e1->value > e2->value;
    if(e1->type != e2->type)
      return strcmp(e1->type, e2->type) == -1;
    return strcmp(e1->str, e2->str) == -1;
  }
};

// use the hash table that supports countable entries
typedef Ordered<_AttributeEntry, const string &, MEDIUM, LARGE> _ImplBase;

// private implementation, which is a shared hash table
class Attributes::_Impl: public _ImplBase, public Shared {
public:
  Registry registry;
  TagSet klasses;

  _Impl(const string &name, const Registry &registry, const TagSet &klasses)
    : _ImplBase(name), registry(registry), klasses(klasses){}
  virtual ~_Impl(void) { /* do nothing */ };

  void add(const char *type, const string &value, Tag tag){
    ulong bucket = Entry::hash(type, value) % NBUCKETS_;
    Entry *entry = buckets_[bucket]->find(type, value);
    if(entry)
      return entry->inc(tag);

    entry = Entry::create(pool_, type, value, buckets_[bucket]);
    buckets_[bucket] = entry;
    entries.push_back(entry);
    ++size;

    entry->insert(tag);
  }

  void update_features(const char *type, const std::string value, 
              const Feature *feats, ulong nfeats){
    ulong bucket = Entry::hash(type, value) % NBUCKETS_;
    Entry *entry = buckets_[bucket]->find(type, value);
    if(!entry){
      entry = Entry::create(pool_, type, value, buckets_[bucket]);
      buckets_[bucket] = entry;
      entries.push_back(entry);
      ++size;
    }
    entry->update_features(feats, nfeats);
  }

  void update_index(const char *type, const std::string value, ulong index){
    ulong bucket = Entry::hash(type, value) % NBUCKETS_;
    Entry *entry = buckets_[bucket]->find(type, value);
    if(!entry)
      return;
    entry->index = index;
  }

  void filter_indices(void){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->index == 0)
        (*i) = 0;
  }

  bool find(const char *type, const string &value, ulong &id){
    Hash hash = Entry::hash(type, value);
    return buckets_[hash % NBUCKETS_]->find(type, value, id);
  }

  // eliminate any features with frequency less than freq
  void apply_cutoff(const ulong freq){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->cutoff(freq))
        (*i) = 0;
  }

  // eliminate any features of a given type with a frequency less than freq
  void apply_cutoff(const char *type, const ulong freq){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->type == type && (*i)->cutoff(freq))
        (*i) = 0;
  }

  // eliminate any features of a given type with frequency less than freq
  // and any other features with a frequency less than a default value def
  void apply_cutoff(const char *type, const ulong freq, const ulong def){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->type == type){
        if((*i)->cutoff(freq))
          (*i) = 0;
      }else if((*i)->cutoff(def))
        (*i) = 0;
  }

  void apply_attrib_cutoff(const ulong freq){
    for(Entries::iterator i = entries.begin(); i != entries.end(); ++i)
      if((*i)->value < freq)
        (*i) = 0;
  }

  // dump out the current list of attributes to a given filename
  void save_attributes(const string &filename, const string &PREFACE) const {
    ofstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open attributes file for writing", filename);
    stream << PREFACE << '\n';
    for(Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
      (*i)->save_attributes(stream);
  }

  // dump out the current list of features to a given filename
  void save_features(const string &filename, const string &PREFACE){
    ofstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open features file for writing", filename);

    stream << PREFACE << '\n';
    for(Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
      (*i)->save_features(stream);
  }

  ulong nfeatures(void) const {
    ulong total = 0;
    for(Entries::const_iterator i = entries.begin(); i != entries.end(); ++i)
      total += (*i)->nfeatures();
    return total;
  }

  void sort_by_rev_attrib_freq(void){ sort(RevAttribCmp<Entry>()); renumber(); };
  
  static void map_load(int, Cluster::KeyValue *kv, void *ptr){
    Attributes::_Impl *impl = reinterpret_cast<Attributes::_Impl *>(ptr);
    string s;

    for(ulong i = 0; i != impl->entries.size(); ++i){
      Entry *e = impl->entries[i];
      if(!e)
	      continue;
	    e->map_load(kv);
    }
  }

  static void reduce_sum(char *key, int keybytes, char *multivalue,
			 int nvalues, int *, Cluster::KeyValue *kv, void *){
    const ulong *counts = reinterpret_cast<ulong *>(multivalue);
    ulong total = 0;
    for(int i = 0; i != nvalues; ++i)
      total += counts[i];
    kv->add(key, keybytes, total);
  }

  static void reduce_mv2v(char *key, int keybytes, char *multivalue,
               int nvalues, int *, Cluster::KeyValue *kv, void *){
    kv->add(key, keybytes, multivalue, nvalues*sizeof(Feature));
  }

  static void map_update(int, char *key, int, char *value,
			 int valuebytes, Cluster::KeyValue *, void *ptr){
    Attributes::_Impl *impl = reinterpret_cast<Attributes::_Impl *>(ptr);
    const Feature *feats = reinterpret_cast<Feature *>(value);
    ulong nfeats = valuebytes/sizeof(Feature);
    
    char *attrib = strchr(key, ' ');
    *attrib++ = '\0';
    const char *type = impl->registry.canonize(key);
    impl->update_features(type, attrib, feats, nfeats);
  }

  static void map_klass_k2v(int, char *key, int keybytes, char *value,
			    int, Cluster::KeyValue *kv, void *){
    char *newkey = 0;
    Tag tag(strtoul(key, &newkey, 10));
    assert(*newkey++ == ' ');
    Feature feat(tag, *reinterpret_cast<ulong *>(value));
    kv->add(newkey, keybytes - (newkey - key), feat);
  }

  void merge(void);

  void pack_index(Cluster::Counts &counts){
    for(ulong i = 0; i != entries.size(); ++i){
      Entry *e = entries[i];
      if(!e)
	      continue;
      std::string key = e->type;
      key += ' ';
      key += e->str;
      counts.push_back(make_pair(key, e->index));
    }
  }

  void unpack_index(Cluster::Counts &counts){
    for(Cluster::Counts::iterator i = counts.begin(); i != counts.end(); ++i){
      string::size_type loc = i->first.find(' ');
      std::string key = i->first.substr(0, loc);
      std::string attrib = i->first.substr(loc + 1);
      const char *type = registry.canonize(key.c_str());
      update_index(type, attrib, i->second);
    }
    filter_indices();
    compact();
  }
 
  void bcast_indices(void){
    if(!Cluster::USE_MPI)
      return;
    Cluster::Counts counts;
    if(Cluster::rank == 0)
      pack_index(counts);
    Cluster::bcast(counts);
    if(Cluster::rank != 0)
      unpack_index(counts);
  }

  void renumber(void){
    for(ulong i = 0; i != entries.size(); ++i)
      entries[i]->index = i + 1;
  }
};

// public wrappers for the private implementation

void
Attributes::_Impl::merge(void){
  if(Cluster::size == 1)
    return;

  Cluster::MapReduce mr;

  mr.map(Cluster::size, map_load, this);

  mr.collate();
  mr.reduce(reduce_sum, 0);
  mr.map(mr.kv(), map_klass_k2v, 0);
  mr.collate();
  mr.reduce(reduce_mv2v, 0);
  mr.gather(1);
  mr.map(mr.kv(), map_update, this);
}

Attributes::Attributes(const string &name, const Model::Registry &registry,
                       const TagSet &klasses)
  : _impl(new _Impl(name, registry, klasses)) {}
Attributes::Attributes(const Attributes &other): _impl(share(other._impl)){}

Attributes &
Attributes::operator=(const Attributes &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Attributes::~Attributes(void){
  release(_impl);
}

const string
Attributes::name(void) const {
  return _impl->name;
}

size_t
Attributes::size(void) const {
  return _impl->entries.size();
}

void
Attributes::operator()(Context &context, const Type &type, const string &value) const {
  ulong id;
  if(_impl->find(type.id, value, id))
    context.push_back(id);
}

void
Attributes::operator()(Context &context, const Type &type, const string &v1,
		       const string &v2) const {
  ulong id;
  if(_impl->find(type.id, v1 + ' ' + v2, id))
    context.push_back(id);
}

void
Attributes::operator()(Context &context, const Type &type, const string &v1,
		       const string &v2, const string &v3) const {
  ulong id;
  if(_impl->find(type.id, v1 + ' ' + v2 + ' '+ v3, id))
    context.push_back(id);
}

void
Attributes::operator()(Tag klass, const Type &type, const string &value){
  _impl->add(type.id, value, klass);
}

void
Attributes::operator()(Tag klass, const Type &type, const string &v1, const string &v2){
  _impl->add(type.id, v1 + ' ' + v2, klass);
}

void
Attributes::operator()(Tag klass, const Type &type, const string &v1, const string &v2, const string &v3){
  _impl->add(type.id, v1 + ' ' + v2 + ' ' + v3, klass);
}

ulong
Attributes::nfeatures(void) const {
  return _impl->nfeatures();
}

void
Attributes::apply_cutoff(ulong freq){
  _impl->apply_cutoff(freq);
}

void
Attributes::apply_cutoff(const Type &type, ulong freq){
  _impl->apply_cutoff(type.id, freq);
}

void
Attributes::apply_cutoff(const Type &type, ulong freq, ulong def){
  _impl->apply_cutoff(type.id, freq, def);
}

void
Attributes::apply_attrib_cutoff(ulong freq){
  _impl->apply_attrib_cutoff(freq);
}

void
Attributes::merge(void){
  _impl->merge();
}

void
Attributes::bcast_indices(void){
  _impl->bcast_indices();
}

void
Attributes::save(const string &attributes, const string &features, const string &PREFACE){
  // eliminate zero entries
  _impl->compress();
  // put the most frequent attributes first
  // and setup the attribute index values
  _impl->sort_by_rev_attrib_freq();
  // dump out the attributes
  _impl->save_attributes(attributes, PREFACE);
  // dump out the features
  _impl->save_features(features, PREFACE);
}

} }
