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
#include <map>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <numeric>
#include <limits>
#include <algorithm>
#include <stdexcept>

#include <csignal>

using namespace std;

#include "except.h"
#include "utils.h"
#include "utils/io.h"

#include "hash.h"
#include "pool.h"
#include "timer.h"

#include "hashtable/entry.h"

#include "thesaurus/options.h"
#include "thesaurus/type.h"
#include "thesaurus/types.h"
#include "thesaurus/attribute.h"
#include "thesaurus/attributes.h"
#include "thesaurus/weight.h"
#include "thesaurus/relation.h"
#include "thesaurus/object.h"
#include "thesaurus/objects.h"

#include "thesaurus/match.h"
#include "thesaurus/measure.h"
#include "thesaurus/measures.h"

#include "thesaurus/weights.h"

#include "thesaurus/explain.h"
#include "thesaurus/extractor.h"

#include "share.h"
#include "fields.h"

namespace NLP { namespace Thesaurus {

class Extractor::_Impl: public Shared {
protected:
  void _load_binary_1(std::istream &in, const std::string &filename);
  void _load_binary_2(std::istream &in, const std::string &filename);
  void _dump_binary_1(std::ostream &out);
  void _dump_binary_2(std::ostream &out);

  Object *_get_object(const std::string &str) const {
    Object *obj = objects.find(str);
    if(obj == 0)
      throw NLP::Exception("the object '" + str + "' does not exist");
    return obj;
  };
public:
  _Impl(const Options &op);
  ~_Impl(void) {}

  MeasureRegistry measures;
  WeightRegistry weights;

  Options op;
  Types types;
  Attributes attributes;
  Objects objects;

  Measure *measure;

  void load(const std::string &filename);
  void load_attributes(const std::string &filename);
  void load_globals(const std::string &filename);
  void load_common(const std::string &filename);

  void load(std::istream &in, const std::string &fname);
  void load_ascii(std::istream &in, const std::string &filename);
  void load_binary(std::istream &in, const std::string &filename);
  void load_attributes(std::istream &in, const std::string &filename);
  void load_globals(std::istream &in, const std::string &filename);
  void load_common(std::istream &in, const std::string &filename);

  void dump_binary(const std::string &filename, int version);
  void dump_binary(std::ostream &out, const std::string &filename, int version);

  void pseudo(const std::string &obj, const std::string &filename);
  void split(const std::string &objstr, float c, const std::string &newstr);
  void diff(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr);
  void sum(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr);
  void intersect(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr);

  void scale(const std::string &targetstr, float c);
  void add(const std::string &targetstr, const std::string &srcstr, float c);
  void sub(const std::string &targetstr, const std::string &srcstr, float c);
  void intersect(const std::string &targetstr, const std::string &srcstr, float c);

  void globals(std::ostream &out) const;

  void thesaurus(const std::string &outfile);
  void thesaurus(const std::string &infile, ulong nresults);

  void thesaurus(const std::string &obj, ulong nresults, Results &results, std::ostream &out);
  ulong rank(const std::string &obj1, const std::string &obj2, std::ostream &out);
  Result best(const std::string &obj, std::ostream &out);

  void explain(const std::string &obj1, const std::string &obj2, std::ostream &out);

  void object(const std::string &obj, std::ostream &out) const;
  void relations(const std::string &obj, ulong nresults, std::ostream &out) const;
  void canonical(const std::string &obj, std::ostream &out) const;
  void type(const std::string &obj, std::ostream &out) const;
  void attribute(const std::string &attr, std::ostream &out) const;

  float freq(const std::string &obj) const;
  float nrelations(const std::string &obj) const;
  float maxrelation(const std::string &obj) const;

  void clear(void) { set_weight("Zero"); };
  void set_weight(const std::string &s);
  void set_measure(const std::string &s);
};

Extractor::_Impl::_Impl(const Options &op): op(op), attributes(types), objects(attributes) {
  set_measure(op.measure_name);
  measure->verbose = op.verbose;
  measure->use_heuristic = op.heuristic;

  load(op.relations_file);
  if(op.common_file.length() != 0)
    load_common(op.common_file);
  if(op.split){
    load_attributes(op.attributes_file);
    load_globals(op.globals_file);
  }

  if(op.verbose){
    cerr << op.relations_file << ": " << Object::ntotal << " unique objects\n";
    cerr << op.relations_file << ": " << Attribute::ntotal << " unique attributes\n";
    cerr << op.relations_file << ": " << Relation::ntotal << " unique relations\n";
    cerr << op.relations_file << ": " << Relation::ftotal << " total instances" << endl;

    cerr << stats(attributes) << endl;
    cerr << stats(objects) << endl;
    objects.printvectorstats(cerr);
  }

  objects.cutoff(op);
  if(op.verbose){
    cerr << op.relations_file << ": " << Object::ncutoff << " unique cutoff objects\n";
    cerr << op.relations_file << ": " << Relation::ncutoff << " unique cutoff relations\n";
    cerr << op.relations_file << ": " << Relation::fcutoff << " total cutoff relations" << endl;
  }

  if(op.optimize){
    set_weight(op.weightfn_name);
    if(op.meta)
      set_weight(op.meta_name);

    objects.optimize(op);

    if(op.verbose){
      cerr << op.relations_file << ": " << Relation::nzero << " unique zeroed relations\n";
      cerr << op.relations_file << ": " << Relation::fzero << " total zeroed relations" << endl;
      objects.printvectorstats(cerr);
    }
  }

  if(op.heuristic){
    set_weight(op.heuristic_name);
    objects.heuristic(op);
  }

  set_weight(op.weightfn_name);
  if(op.meta)
    set_weight(op.meta_name);
}

void
Extractor::_Impl::thesaurus(const std::string &filename){
  std::ofstream output(filename.c_str());
  if(!output)
    NLP::Exception("full thesaurus output file '" + filename + "' could not be opened");
  measure->thesaurus(objects, output);
}

void
Extractor::_Impl::thesaurus(const std::string &filename, ulong nresults){
  measure->thesaurus(objects, filename, nresults);
}

void
Extractor::_Impl::thesaurus(const std::string &objstr, ulong nresults, Results &results, std::ostream &out){
  results.resize(0);
  results.reserve(nresults);

  Object *obj = _get_object(objstr);
  measure->all(objects, out, obj);

  Matches &matches = measure->matches;
  if(matches.size() > 0){
    if(matches.size() < nresults)
      nresults = matches.size();

    for(uint i = 0; i < nresults; i++)
      results.push_back(Result(matches[i].object->str(), matches[i].score));
  }
}

ulong
Extractor::_Impl::rank(const std::string &obj1str, const std::string &obj2str, std::ostream &out){
  Object *obj1 = _get_object(obj1str);
  Object *obj2 = _get_object(obj2str);

  measure->all(objects, out, obj1);
  Matches &matches = measure->matches;

  ulong rank = 0;
  float score = matches[0].score;

  for(uint i = 0; i < matches.size(); i++){
    if(matches[i].score != score){
      score = matches[i].score;
      rank = i;
    }
    if(matches[i].object == obj2)
      return rank + 1;
  }

  return 0;
}

void
Extractor::_Impl::pseudo(const std::string &objstr, const std::string &filename){
  std::ifstream file(filename.c_str());
  if(file)
    throw NLP::IOException("could not open relations file for reading", filename);

  objects.pseudo(file, filename, objstr);
}

void 
Extractor::_Impl::split(const std::string &objstr, float c, const std::string &newstr){
  Object *obj = _get_object(objstr);
  objects.split(obj, c, newstr);
}

void 
Extractor::_Impl::diff(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr){
  Object *obj1 = _get_object(obj1str);
  Object *obj2 = _get_object(obj2str);

  objects.diff(obj1, c1, obj2, c2, newstr);
}

void 
Extractor::_Impl::sum(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr){
  Object *obj1 = _get_object(obj1str);
  Object *obj2 = _get_object(obj2str);
  objects.sum(obj1, c1, obj2, c2, newstr);
}

void 
Extractor::_Impl::intersect(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr){
  Object *obj1 = _get_object(obj1str);
  Object *obj2 = _get_object(obj2str);
  objects.intersect(obj1, c1, obj2, c2, newstr);
}

void
Extractor::_Impl::scale(const std::string &targetstr, float c){
  Object *target = _get_object(targetstr);
  target->scale(c);
}

void
Extractor::_Impl::add(const std::string &targetstr, const std::string &srcstr, float c){
  Object *target = _get_object(targetstr);
  Object *src = _get_object(srcstr);
  objects.add(target, src, c);
}

void 
Extractor::_Impl::sub(const std::string &targetstr, const std::string &srcstr, float c){
  Object *target = _get_object(targetstr);
  Object *src = _get_object(srcstr);
  objects.sub(target, src, c);
}

void 
Extractor::_Impl::intersect(const std::string &targetstr, const std::string &srcstr, float c){
  Object *target = _get_object(targetstr);
  Object *src = _get_object(srcstr);
  objects.intersect(target, src, c);
}


Result
Extractor::_Impl::best(const std::string &objstr, std::ostream &out){
  Object *obj = _get_object(objstr);

  Match match;
  measure->best(objects, out, obj, match);

  if(!match.object)
    throw NLP::Exception("something strange happened in Extractor::best");
  return Result(match.object->str(), match.score);
}

void
Extractor::_Impl::explain(const std::string &obj1str, const std::string &obj2str, std::ostream &out){
  Object *obj1 = _get_object(obj1str);
  Object *obj2 = _get_object(obj2str);
  measure->explain(out, obj1, obj2);
}

void
Extractor::_Impl::object(const std::string &objstr, std::ostream &out) const {
  Object *obj = _get_object(objstr);
  out << stats(*obj);
}

float
Extractor::_Impl::freq(const std::string &objstr) const {
  Object *obj = objects.find(objstr);
  if(obj == 0)
    return 0;
  else
    return obj->freq();
}

float
Extractor::_Impl::nrelations(const std::string &objstr) const {
  Object *obj = objects.find(objstr);
  if(obj == 0)
    return 0;
  else
    return obj->relations.size();
}

float
Extractor::_Impl::maxrelation(const std::string &objstr) const {
  Object *obj = objects.find(objstr);
  if(obj == 0)
    return 0;
  else {
    const Relations &rels = obj->relations;
    float max_freq = 0;
    for(Relations::const_iterator i = rels.begin(); i != rels.end(); ++i)
      if(i->freq() > max_freq)
        max_freq = i->freq();
    return max_freq;
  }
}

void
Extractor::_Impl::relations(const std::string &objstr, ulong nresults, std::ostream &out) const {
  Object *obj = _get_object(objstr);

  vector<const Relation *> relations;
  for(Relations::iterator r = obj->relations.begin(); r != obj->relations.end(); r++)
    relations.push_back(&*r);

  sort(relations.begin(), relations.end(), RelationGTComp());

  out << "object: " << obj->str() << ' ' << relations.size() << ' ' << obj->freq() << endl;
  uint n = 0;
  for(uint i = 0; i < relations.size() && n < nresults; i++)
    if(relations[i]->freq() > 2){
      out << "  " << stats(*relations[i]) << endl;
      n++;
    }
}

void
Extractor::_Impl::canonical(const std::string &objstr, std::ostream &out) const {
  Object *obj = _get_object(objstr);
  obj->printcanonical(out);
  out << endl;
}

void
Extractor::_Impl::type(const std::string &typestr, std::ostream &out) const {
  Type *type = types.find(typestr);
  if(type == 0)
    throw NLP::Exception("the type '" + typestr + "' does not exist");
  out << stats(*type) << endl;
}

void
Extractor::_Impl::attribute(const std::string &attrstr, std::ostream &out) const {
  Attribute *attribute = attributes.find(attrstr);
  if(attribute == 0)
    throw NLP::Exception("the attribute '" + attrstr + "' does not exist");
  out << stats(*attribute) << endl;
}

void
Extractor::_Impl::set_weight(const std::string &s){
  objects.score(weights.get(s));
  if(measure)
    measure->previous = 0;
}

void
Extractor::_Impl::set_measure(const std::string &s){
  measure = measures.get(s);
  measure->verbose = op.verbose;
  measure->use_heuristic = op.heuristic;
  measure->previous = 0;
}

const char *const BINARY_FILE_VERSION_1 = "0.1";
const char *const BINARY_FILE_VERSION_2 = "0.2";
const ulong BINARY_FILE_MAGIC = 0x12345678;

bool
_isbinheader(char header[5]){
  return header[0] == 'T' && isdigit(header[1]) && header[2] == '.' &&
         isdigit(header[3]) && header[4] == '\0';
}

void
_load_magic(std::istream &in, const std::string &fname){
  ulong magic;
  if(!load_ulong(in, magic))
    throw NLP::IOException("unexpected EOF, missing magic number", fname, in.tellg());
  if(magic != BINARY_FILE_MAGIC)
    throw NLP::IOException("incorrect magic number", fname, in.tellg());
}

void
_dump_magic(std::ostream &out){
  dump_ulong(out, BINARY_FILE_MAGIC);
}

void
Extractor::_Impl::load(const std::string &fname){
  ifstream in(fname.c_str());
  if(!in)
    throw NLP::IOException("could not open file for reading", fname);
  load(in, fname);
}

void
Extractor::_Impl::load_attributes(const std::string &fname){
  ifstream in(fname.c_str());
  if(!in)
    throw NLP::IOException("could not open file for reading", fname);
  load_attributes(in, fname);
}

void
Extractor::_Impl::load_globals(const std::string &fname){
  ifstream in(fname.c_str());
  if(!in)
    throw NLP::IOException("could not open file for reading", fname);
  load_globals(in, fname);
}

void
Extractor::_Impl::load_common(const std::string &fname){
  ifstream in(fname.c_str());
  if(!in)
    throw NLP::IOException("could not open file for reading", fname);
  load_common(in, fname);
}

void
Extractor::_Impl::load(std::istream &in, const std::string &fname){
  // identify file format (binary or not)
  char header[5];

  if(!in.read(header, sizeof(char)*4))
    throw NLP::IOException("unexpected end of file", fname);

  header[4] = '\0';

  for(int i = 3; i >= 0; i--)
    in.putback(header[i]);

  if(_isbinheader(header))
    load_binary(in, fname);
  else
    load_ascii(in, fname);
};

void
Extractor::_Impl::load_attributes(std::istream &in, const std::string &fname){
  ulong nlines = 1;
  char buffer[1024];
  char *attrstr;
  ulong freq = 0;
  ulong nrelations = 0;
  char *s = 0;

  while(in.getline(buffer, sizeof(buffer))){
    for(s = buffer; *s; s++)
      if(*s == ' '){
        *s++ = '\0';
        if(!str2ulong(buffer, freq)){
          string msg = "invalid frequency '";
          msg += buffer;
          msg += '\'';
          throw NLP::IOException(msg, fname, nlines);
        }
        break;
      }
    if(!*s){
      string msg = "missing relation frequency and attribute '";
      msg += buffer;
      msg += '\'';
      throw NLP::IOException(msg, fname, nlines);
    }

    char *tmp = s;
    for(; *s; s++)
      if(*s == ' '){
        *s++ = '\0';
        if(!str2ulong(tmp, nrelations)){
          string msg = "invalid nrelations '";
          msg += buffer;
          msg += '\'';
          throw NLP::IOException(msg, fname, nlines);
        }
        break;
      }
    if(!*s){
      string msg = "missing attribute '";
      msg += buffer;
      msg += '\'';
      throw NLP::IOException(msg, fname, nlines);
    }

    attrstr = s;
    Attribute *attribute = attributes.add(attrstr);
    attribute->set(freq, nrelations);
    nlines++;
  }

  attributes.sort_by_alpha();
}

void
Extractor::_Impl::globals(std::ostream &out) const {
  out.precision(15);
  out << "Type::ftotal " << Type::ftotal << endl;
  out << "Type::ntotal " << Type::ntotal << endl;
  out << "Attribute::ftotal " << Attribute::ftotal << endl;
  out << "Attribute::ntotal " << Attribute::ntotal << endl;
  out << "Relation::ntotal " << Relation::ntotal << endl;
  out << "Relation::ftotal " << Relation::ftotal << endl;
  out << "Object::ntotal " << Object::ntotal << endl;
  out << "Object::ftotal " << Object::ftotal << endl;
}

void
Extractor::_Impl::load_globals(std::istream &in, const std::string &fname){
  load_field(fname, in, "Type::ftotal", Type::ftotal);
  load_field(fname, in, "Type::ntotal", Type::ntotal);
  load_field(fname, in, "Attribute::ftotal", Attribute::ftotal);
  load_field(fname, in, "Attribute::ntotal", Attribute::ntotal);
  load_field(fname, in, "Relation::ntotal", Relation::ntotal);
  load_field(fname, in, "Relation::ftotal", Relation::ftotal);
  load_field(fname, in, "Object::ntotal", Object::ntotal);
  load_field(fname, in, "Object::ftotal", Object::ftotal);
}

void
Extractor::_Impl::load_ascii(std::istream &in, const std::string &fname){
  ulong nlines = 1;
  char buffer[1024];
  char *objstr;
  char *attrstr;
  ulong count = 0;
  char *s = 0;

  Object *object = 0;
  while(in.getline(buffer, sizeof(buffer))){
    for(s = buffer; *s; s++)
      if(*s == ' '){
        *s++ = '\0';
        if(!str2ulong(buffer, count)){
          string msg = "invalid frequency '";
          msg += buffer;
          msg += '\'';
          throw NLP::IOException(msg, fname, nlines);
        }
        break;
      }
    if(!*s){
      string msg = "missing object and attribute '";
      msg += buffer;
      msg += '\'';
      throw NLP::IOException(msg, fname, nlines);
    }

    for(objstr = s; *s; s++)
      if(*s == ' '){
        *s++ = '\0';
        break;
      }
    if(!*s){
      string msg = "missing attribute '";
      msg += buffer;
      msg += '\'';
      throw NLP::IOException(msg, fname, nlines);
    }

    attrstr = s;
    Attribute *attribute = attributes.add(attrstr);
    attribute->add(count);

    if(!object)
      object = objects.add(objstr);
    else if(strcmp(objstr, object->str())){
      object->compact();
      object = objects.add(objstr);
    }
    object->add(attribute, count);

    nlines++;
  }

  attributes.sort_by_alpha();
}

void
Extractor::_Impl::load_common(std::istream &in, const std::string &fname){
  ulong nlines = 1;
  char buffer[1024];
  char *objstr;
  char *attrstr;
  ulong count = 0;
  char *s = 0;

  Object *object = 0;
  while(in.getline(buffer, sizeof(buffer))){
    for(s = buffer; *s; s++)
      if(*s == ' '){
        *s++ = '\0';
        if(!str2ulong(buffer, count)){
          string msg = "invalid frequency '";
          msg += buffer;
          msg += '\'';
          throw NLP::IOException(msg, fname, nlines);
        }
        break;
      }
    if(!*s){
      string msg = "missing object and attribute '";
      msg += buffer;
      msg += '\'';
      throw NLP::IOException(msg, fname, nlines);
    }

    for(objstr = s; *s; s++)
      if(*s == ' '){
        *s++ = '\0';
        break;
      }
    if(!*s){
      string msg = "missing attribute '";
      msg += buffer;
      msg += '\'';
      throw NLP::IOException(msg, fname, nlines);
    }

    attrstr = s;
    Attribute *attribute = attributes.add(attrstr);
    attribute->add(count);

    if(!object){
      object = objects.add(objstr);
      if(object->relations.size() != 0)
        object->reset();
      else
        object->testset();
    }else if(strcmp(objstr, object->str())){
      object->compact();
      object = objects.add(objstr);
      if(object->relations.size() != 0)
        object->reset();
      else
        object->testset();
    }
    object->add(attribute, count);

    nlines++;
  }
}

void
Extractor::_Impl::_dump_binary_1(std::ostream &out){
  _dump_magic(out);

  attributes.dump_1(out);
  _dump_magic(out);

  objects.dump_1(out);
  _dump_magic(out);
}

void
Extractor::_Impl::_load_binary_1(std::istream &in, const std::string &fname){
  _load_magic(in, fname);

  attributes.load_1(in, fname);
  _load_magic(in, fname);

  objects.load_1(in, fname);
  _load_magic(in, fname);
}

void
Extractor::_Impl::_dump_binary_2(std::ostream &out){
  _dump_magic(out);

  attributes.dump_2(out);
  Attribute::global_dump_2(out);
  _dump_magic(out);

  objects.dump_2(out);
  Object::global_dump_2(out);
  _dump_magic(out);

  Relation::global_dump_2(out);
  _dump_magic(out);
}

void
Extractor::_Impl::_load_binary_2(std::istream &in, const std::string &fname){
  _load_magic(in, fname);

  attributes.load_2(in, fname);
  Attribute::global_load_2(in, fname);
  _load_magic(in, fname);

  objects.load_2(in, fname);
  Object::global_load_2(in, fname);
  _load_magic(in, fname);

  Relation::global_load_2(in, fname);
  _load_magic(in, fname);
}

void
Extractor::_Impl::dump_binary(const std::string &fname, int version){
  ofstream out(fname.c_str());
  if(!out)
    throw NLP::IOException("could not open file for writing", fname);
  dump_binary(out, fname, version);
}

void
Extractor::_Impl::dump_binary(std::ostream &out, const std::string &fname, int version){
  out << 'T';
  switch(version){
    case 1:
      out << BINARY_FILE_VERSION_1;
      _dump_binary_1(out);
      break;
    case 2:
      out << BINARY_FILE_VERSION_2;
      _dump_binary_2(out);
      break;
    default:
      assert(!"unknown binary format");
  }
}

void
Extractor::_Impl::load_binary(std::istream &in, const std::string &fname){
  char header[5];

  if(!in.read(header, sizeof(char)*4))
    throw NLP::IOException("unexpected end of file", fname, 0);
  header[4] = '\0';

  if(!_isbinheader(header)){
    string msg = "header does not have correct format '";
    msg += header;
    msg += '\'';
    throw NLP::IOException(msg, fname, 0);
  }

  if(strcmp(&header[1], BINARY_FILE_VERSION_1) == 0)
    _load_binary_1(in, fname);
  else if(strcmp(&header[1], BINARY_FILE_VERSION_2) == 0)
    _load_binary_2(in, fname);
  else{
    string msg = "unknown version number ";
    msg += header;
    msg += '\'';
    throw NLP::IOException(msg, fname, in.tellg());
  }
}

Extractor::Extractor(const Options &op): _impl(new _Impl(op)) {}
Extractor::Extractor(const Extractor &other): _impl(share(other._impl)){}

Extractor &
Extractor::operator=(const Extractor &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

Extractor::~Extractor(void){
  release(_impl);
}

void Extractor::load(const std::string &filename){ _impl->load(filename); };
void Extractor::load_attributes(const std::string &filename){ _impl->load_attributes(filename); };
void Extractor::load_globals(const std::string &filename){ _impl->load_globals(filename); };
void Extractor::load_common(const std::string &filename){ _impl->load_common(filename); };

void Extractor::load(std::istream &in, const std::string &fname){ _impl->load(in, fname); };

void
Extractor::load_ascii(std::istream &in, const std::string &filename){
  _impl->load_ascii(in, filename);
};

void
Extractor::load_attributes(std::istream &in, const std::string &filename){
  _impl->load_attributes(in, filename);
}

void
Extractor::load_globals(std::istream &in, const std::string &filename){
  _impl->load_globals(in, filename);
}

void
Extractor::load_common(std::istream &in, const std::string &filename){
  _impl->load_common(in, filename);
}

void
Extractor::load_binary(std::istream &in, const std::string &filename){
  _impl->load_binary(in, filename);
};

void
Extractor::dump_binary(const std::string &filename, int version){
  _impl->dump_binary(filename, version);
};

void
Extractor::dump_binary(std::ostream &out, const std::string &filename, int version){
  _impl->dump_binary(out, filename, version);
};

void
Extractor::pseudo(const std::string &obj, const std::string &filename){
  _impl->pseudo(obj, filename);
};

void 
Extractor::split(const std::string &oldstr, float c, const std::string &newstr){
  _impl->split(oldstr, c, newstr);
}

void 
Extractor::diff(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr){
  _impl->diff(obj1str, c1, obj2str, c2, newstr);
}

void
Extractor::sum(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr){
  _impl->sum(obj1str, c1, obj2str, c2, newstr);
}

void
Extractor::intersect(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr){
  _impl->intersect(obj1str, c1, obj2str, c2, newstr);
}

void 
Extractor::scale(const std::string &targetstr, float c){
  _impl->scale(targetstr, c);
}

void 
Extractor::add(const std::string &targetstr, const std::string &srcstr, float c){
  _impl->add(targetstr, srcstr, c);
}

void 
Extractor::sub(const std::string &targetstr, const std::string &srcstr, float c){
  _impl->sub(targetstr, srcstr, c);
}

void 
Extractor::intersect(const std::string &targetstr, const std::string &srcstr, float c){
  _impl->intersect(targetstr, srcstr, c);
}

void
Extractor::thesaurus(const std::string &outfile){
  _impl->thesaurus(outfile);
};

void
Extractor::thesaurus(const std::string &infile, ulong nresults){
  _impl->thesaurus(infile, nresults);
};

void
Extractor::thesaurus(const std::string &obj, ulong nresults, Results &results, std::ostream &out){
  _impl->thesaurus(obj, nresults, results, out);
};

ulong
Extractor::rank(const std::string &obj1, const std::string &obj2, std::ostream &out){
  return _impl->rank(obj1, obj2, out);
};

Result
Extractor::best(const std::string &obj, std::ostream &out){
  return _impl->best(obj, out);
};

void
Extractor::explain(const std::string &obj1, const std::string &obj2, std::ostream &out){
  _impl->explain(obj1, obj2, out);
};

void
Extractor::object(const std::string &obj, std::ostream &out) const {
  _impl->object(obj, out);
};

void
Extractor::relations(const std::string &obj, ulong nresults, std::ostream &out) const {
  _impl->relations(obj, nresults, out);
};

void
Extractor::canonical(const std::string &obj, std::ostream &out) const {
  _impl->canonical(obj, out);
};

void
Extractor::type(const std::string &obj, std::ostream &out) const {
  _impl->type(obj, out);
};

void
Extractor::attribute(const std::string &attr, std::ostream &out) const {
  _impl->attribute(attr, out);
};

void
Extractor::globals(std::ostream &out) const {
  _impl->globals(out);
};

float Extractor::freq(const std::string &obj) const { return _impl->freq(obj); }
float Extractor::nrelations(const std::string &obj) const { return _impl->nrelations(obj); }
float Extractor::maxrelation(const std::string &obj) const { return _impl->maxrelation(obj); }

void Extractor::clear(void){ _impl->clear(); };
void Extractor::set_weight(const std::string &s){ _impl->set_weight(s); };
void Extractor::set_measure(const std::string &s){ _impl->set_measure(s); };
void Extractor::print_weights(std::ostream &out){ out << _impl->weights; };
void Extractor::print_measures(std::ostream &out){ out << _impl->measures; };

}  }
