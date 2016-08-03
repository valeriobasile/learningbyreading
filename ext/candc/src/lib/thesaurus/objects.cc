// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <cmath>
#include <string>
#include <vector>
#include <bitset>
#include <map>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <numeric>
#include <limits>
#include <algorithm>
#include <stdexcept>

using namespace std;

#include "except.h"
#include "utils.h"
#include "utils/io.h"

#include "hash.h"
#include "pool.h"

#include "hashtable/size.h"
#include "hashtable/entry.h"
#include "hashtable/count.h"

#include "thesaurus/options.h"
#include "thesaurus/type.h"
#include "thesaurus/types.h"
#include "thesaurus/attribute.h"
#include "thesaurus/attributes.h"
#include "thesaurus/weight.h"
#include "thesaurus/relation.h"
#include "thesaurus/object.h"
#include "thesaurus/objects.h"

#include "share.h"

namespace NLP { namespace Thesaurus {

using namespace HashTable;

typedef HashTable::Count<Object,LARGE,LARGE,LARGE> _ImplBase;

class Objects::_Impl: public _ImplBase, public Shared {
protected:
  Attributes _attributes;

  // serialization interface
  void _load_1(std::istream &in, const std::string &fname){
    char buffer[1024];
    if(!load_string(in, buffer, sizeof(buffer)))
      throw NLP::IOException("could not load object string", fname, in.tellg());

    Object *o = add(buffer);
    o->load_1(_attributes, in, fname);
  }

  void _load_2(std::istream &in, const std::string &fname){
    char buffer[1024];
    if(!load_string(in, buffer, sizeof(buffer)))
      throw NLP::IOException("could not load object string", fname, in.tellg());

    Object *o = add(buffer);
    o->load_2(_attributes, in, fname);
  }
public:
  _Impl(Attributes attributes, Pool *pool = 0, Pool *lpool = 0):
      _ImplBase("objects", pool, lpool), _attributes(attributes) {};
  ~_Impl(void) { /* do nothing */ };

  // serializing interface
  void load_1(std::istream &in, const std::string &fname){
    ulong nobjects;
    if(!load_ulong(in, nobjects))
      throw NLP::IOException("missing number of objects", fname, in.tellg());

    for(ulong i = 0; i < nobjects; i++)
      _load_1(in, fname);
  }

  void dump_1(std::ostream &out) const {
    dump_ulong(out, size);

    for(ulong i = 0; i < size; i++)
      entries[i]->dump_1(out);
  }

  void load_2(std::istream &in, const std::string &fname){
    ulong nobjects;
    if(!load_ulong(in, nobjects))
      throw NLP::IOException("missing number of objects", fname, in.tellg());

    for(ulong i = 0; i < nobjects; i++)
      _load_2(in, fname);
  }

  void dump_2(std::ostream &out) const {
    dump_ulong(out, size);

    for(ulong i = 0; i < size; i++)
      entries[i]->dump_2(out);
  }

  using _ImplBase::add;

  void score(Weight &weight){
    for(ulong i = 0; i < size; i++)
      if(entries[i])
        entries[i]->score(weight);
  }

  void cutoff(const Options &op){
    if(op.cutoff_minimum == 0)
      return;

    const ulong CUTOFF = op.cutoff_minimum;
    for(ulong i = 0; i < size; i++)
      if(entries[i]->freq() < CUTOFF){
        Object::ncutoff++;
        Object::fcutoff += entries[i]->freq();
        Relation::ncutoff += entries[i]->unique();
        Relation::fcutoff += entries[i]->freq();
        entries[i] = 0;
      }
  }

  void optimize(const Options &op){
    for(ulong i = 0; i < size; i++){
      if(entries[i] == 0)
        continue;
      entries[i]->optimize(op);
      if(entries[i]->score() == 0.0)
        entries[i] = 0;
    }
    compress();
  }

  void heuristic(const Options &op){
    for(ulong i = 0; i < size; i++){
      if(entries[i] == 0)
        continue;
      entries[i]->heuristic(op);
    }
  }

  Object *pseudo(std::istream &in, const std::string &fname, const std::string &name){
    Object *o = add(name.c_str());

    std::string attrstr;
    while(in >> attrstr){
      ulong freq;
      if(!(in >> freq))
        throw NLP::IOException("could not convert pseudo object relation", fname, in.tellg());

      Attribute *attribute = _attributes.find(attrstr.c_str());
      if(!attribute)
        throw NLP::IOException("attribute " + attrstr + " does not exist", fname, in.tellg());

      o->add(attribute, freq);
    }

    return o;
  }

  void split(Object *old, float c, const std::string &newstr){
    Object *newobj = add(newstr.c_str());

    const Relations &rels = old->relations;
    for(Relations::const_iterator i = rels.begin(); i != rels.end(); ++i)
      newobj->add(Relation(*i, c));
  }

  Object *diff(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr){
    const Relations &r1 = obj1->relations;
    const Relations &r2 = obj2->relations;

    Object *newobj = add(newstr.c_str());

    Relations::const_iterator i = r1.begin();
    Relations::const_iterator j = r2.begin();

    while(i != r1.end() && j != r2.end()){
      if(i->equal(*j)){
        float freq = i->freq()*c1 - j->freq()*c2;
        if(freq > 0)
          newobj->add(Relation(i->attrib(), freq));
        i++;
        j++;
      }else if(i->compare(*j) < 0){
        newobj->add(Relation(*i, c1));
        i++;
      }else
        j++;
    }
    for( ; i != r1.end(); ++i)
      newobj->add(Relation(*i, c1));

    return newobj;
  }

  Object *sum(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr){
    const Relations &r1 = obj1->relations;
    const Relations &r2 = obj2->relations;

    Object *newobj = add(newstr.c_str());

    Relations::const_iterator i = r1.begin();
    Relations::const_iterator j = r2.begin();

    while(i != r1.end() && j != r2.end()){
      if(i->equal(*j)){
        float freq = i->freq()*c1 + j->freq()*c2;
        newobj->add(Relation(i->attrib(), freq));
        i++;
        j++;
      }else if(i->compare(*j) < 0){
        newobj->add(Relation(*i, c1));
        i++;
      }else{
        newobj->add(Relation(*j, c2));
        j++;
      }
    }
    for( ; i != r1.end(); ++i)
      newobj->add(Relation(*i, c1));
    for( ; j != r2.end(); ++j)
      newobj->add(Relation(*j, c2));

    return newobj;
  }

  Object *intersect(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr){
    const Relations &r1 = obj1->relations;
    const Relations &r2 = obj2->relations;

    Object *newobj = add(newstr.c_str());

    Relations::const_iterator i = r1.begin();
    Relations::const_iterator j = r2.begin();

    while(i != r1.end() && j != r2.end()){
      if(i->equal(*j)){
        float freq = i->freq()*c1 + j->freq()*c2;
        newobj->add(Relation(i->attrib(), freq));
        i++;
        j++;
      }else if(i->compare(*j) < 0)
        i++;
      else
        j++;
    }

    return newobj;
  }

  void add(Object *target, const Object *src, float c){
    Relations &r1 = target->relations;
    const Relations &r2 = src->relations;

    Relations::const_iterator j = r2.begin();

    size_t i = 0;
    size_t size = r1.size();
    while(i != size && j != r2.end()){
      if(r1[i].equal(*j)){
        r1[i].inc(c*j->freq());
        i++;
        j++;
      }else if(r1[i].compare(*j) < 0)
        i++;
      else{
        target->add(Relation(*j, c));
        j++;
      }
    }
    for( ; j != r2.end(); ++j)
      target->add(Relation(*j, c));

    std::sort(r1.begin(), r1.end(), RelationAlphaComp());
  }

  void sub(Object *target, const Object *src, float c){

  }

  void intersect(Object *target, const Object *src, float c){

  }

  void printvectorstats(std::ostream &out){
    ulong vused = 0;
    ulong vtotal = 0;
    for(ulong i = 0; i < size; i++){
      if(!entries[i])
        continue;
      vused += entries[i]->relations.size();
      vtotal += entries[i]->relations.capacity();
    }
    out << vused << " (" << (vused*sizeof(Relation)) << " bytes) used relation vector elems\n";
    out << vtotal << " (" << (vtotal*sizeof(Relation)) << " bytes) total relation vector elems\n";
    out << setprecision(2) << (100.0*vused/(float)vtotal) << " vector utilisation\n";
  }
};

Objects::Objects(Attributes attributes, Pool *pool, Pool *lpool):
    _impl(new _Impl(attributes, pool, lpool)) {}
Objects::Objects(const Objects &other): _impl(share(other._impl)){}
Objects::~Objects(void){ release(_impl); }

Objects &
Objects::operator=(const Objects &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

ulong Objects::size(void) const { return _impl->size; }

Object *Objects::add(const char *s){ return _impl->add(s); }
Object *Objects::copy(const Object *obj){ return _impl->copy(obj); }
Object *Objects::find(const char *s) const { return _impl->find(s); }

Object *Objects::get(ulong id) const { return _impl->entries[id]; }

void
Objects::load_1(std::istream &in, const std::string &fname){
  _impl->load_1(in, fname);
}

void
Objects::dump_1(std::ostream &out) const{
  _impl->dump_1(out);
}

void
Objects::load_2(std::istream &in, const std::string &fname){
  _impl->load_2(in, fname);
}

void
Objects::dump_2(std::ostream &out) const{
  _impl->dump_2(out);
}

void Objects::score(Weight &weight){ _impl->score(weight); }
void Objects::cutoff(const Options &op){ _impl->cutoff(op); }
void Objects::optimize(const Options &op){ _impl->optimize(op); }
void Objects::heuristic(const Options &op){ _impl->heuristic(op); }

Object *
Objects::pseudo(std::istream &in, const std::string &filename, const std::string &name){
  return _impl->pseudo(in, filename, name);
}

void
Objects::split(Object *obj, float c, const std::string &newstr){
  _impl->split(obj, c, newstr);
}

Object *
Objects::diff(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr){
  return _impl->diff(obj1, c1, obj2, c2, newstr);
}

Object *
Objects::sum(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr){
  return _impl->sum(obj1, c1, obj2, c2, newstr);
}

Object *
Objects::intersect(const Object *obj1, float c1, const Object *obj2, float c2, const std::string &newstr){
  return _impl->intersect(obj1, c1, obj2, c2, newstr);
}


void 
Objects::add(Object *target, const Object *src, float c){
  _impl->add(target, src, c);
}

void 
Objects::sub(Object *target, const Object *src, float c){
  _impl->sub(target, src, c);
}

void 
Objects::intersect(Object *target, const Object *src, float c){
  _impl->intersect(target, src, c);
}

void Objects::printstats(std::ostream &os) const { _impl->printstats(os); }

void
Objects::printvectorstats(std::ostream &out){
  _impl->printvectorstats(out);
}

} }
