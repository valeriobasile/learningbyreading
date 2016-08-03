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

#include "thesaurus/weights.h"

#include "thesaurus/match.h"
#include "thesaurus/measure.h"
#include "thesaurus/measures.h"

#include "thesaurus/extractor.h"
#include "thesaurus/answer.h"

namespace NLP { namespace Thesaurus {

bool
answer_extractor(Extractor extractor, std::istream &in, std::ostream &out){
  string objstr;
  ulong nresults = 0;
  Results results;

  if(!(in >> objstr)){
    out << "extractor: incorrect format used\n";
    out << "usage: t <object> [nresults]" << endl;
    return false;
  }

  if(in >> nresults)
    out << "extractor: returning " << nresults << " results" << endl;
  else{
    nresults = 10;
    out << "extractor: returning " << nresults << " results (default)" << endl;
  }

  extractor.thesaurus(objstr, nresults, results, out);
  if(results.size() != 0){
    if(results.size() != nresults)
      out << "only " << results.size() << " matches found" << endl;

    for(uint i = 0; i < results.size(); i++)
      out << '[' << i << "] " << results[i] << endl;
  }else
    out << "no matches found" << endl;

  return true;
}

bool
answer_rank(Extractor extractor, std::istream &in, std::ostream &out){
  string obj1str;
  string obj2str;

  if(!(in >> obj1str >> obj2str)){
    out << "extractor: incorrect format used\n";
    out << "usage: r <object1> <object2>" << endl;
    return false;
  }

  ulong rank = extractor.rank(obj1str, obj2str, out);
  if(rank)
    out << obj1str << ' ' << obj2str << ' ' << rank << endl;
  else
    out << obj1str << ' ' << obj2str << " *" << endl;

  return true;
}

bool
answer_pseudo(Extractor extractor, std::istream &in, std::ostream &out){
  string name;
  string filename;

  if(!(in >> name >> filename)){
    out << "extractor: incorrect format used\n";
    out << "usage: p <name> <filename>" << endl;
    return false;
  }

  extractor.pseudo(name, filename);
  return true;
}

bool
answer_best(Extractor extractor, std::istream &in, std::ostream &out){
  string objstr;
  if(!(in >> objstr)){
    out << "extractor: a string is required\n";
    out << "usage: b <object>" << endl;
    return false;
  }

  out << objstr << " best match is " << extractor.best(objstr, out) << endl;
  return true;
}

bool
answer_explain(Extractor extractor, std::istream &in, std::ostream &out){
  string obj1str;
  string obj2str;

  if(!(in >> obj1str >> obj2str)){
    out << "extractor: two strings are required\n";
    out << "usage: e <object1> <object2>" << endl;
    return false;
  }

  extractor.explain(obj1str, obj2str, out);
  return true;
}

bool
answer_object(Extractor extractor, std::istream &in, std::ostream &out){
  string objstr;
  if(!(in >> objstr)){
    out << "extractor: a string is required\n";
    out << "usage: o <object>" << endl;
    return false;
  }

  extractor.object(objstr, out);
  return true;
}

bool
answer_relations(Extractor extractor, std::istream &in, std::ostream &out){
  string objstr;
  ulong nresults;

  if(!(in >> objstr >> nresults)){
    out << "extractor: a string is required\n";
    out << "usage: n <object> <nresults>" << endl;
    return false;
  }

  extractor.relations(objstr, nresults, out);
  return true;
}

bool
answer_attribute(Extractor extractor, std::istream &in, std::ostream &out){
  string attrstr;
  if(!(in >> attrstr)){
    out << "extractor: a string is required\n";
    out << "usage: a <attribute>" << endl;
    return false;
  }

  extractor.attribute(attrstr, out);
  return true;
}

bool
answer_canonical(Extractor extractor, std::istream &in, std::ostream &out){
  string objstr;
  if(!(in >> objstr)){
    out << "extractor: a string is required\n";
    out << "usage: k <object>" << endl;
    return false;
  }

  extractor.canonical(objstr, out);
  return true;
}

bool
answer_types(Extractor extractor, std::istream &in, std::ostream &out){
  string typestr;
  if(!(in >> typestr)){
    out << "extractor: a string is required\n";
    out << "usage: x <type>" << endl;
    return false;
  }

  extractor.type(typestr, out);
  return true;
}


bool
answer_clear(Extractor extractor, std::istream &in, std::ostream &out){
  extractor.clear();
  return true;
}

bool
answer_weight(Extractor extractor, std::istream &in, std::ostream &out){
  string weightstr;
  if(!(in >> weightstr)){
    out << "extractor: a string is required\n";
    out << "usage: w <method>" << endl;
    out << "usage: where <method> is on of:\n";
    extractor.print_weights(out);
    return false;
  }

  extractor.set_weight(weightstr);
  return true;
}

bool
answer_measure(Extractor extractor, std::istream &in, std::ostream &out){
  string measurestr;
  if(!(in >> measurestr)){
    out << "extractor: a string is required\n";
    out << "usage: m <measure>" << endl;
    out << "usage: where <measure> is one of:\n";
    extractor.print_measures(out);
    return false;
  }

  return true;
}

bool
answer_dump(Extractor extractor, std::istream &in, std::ostream &out){
  string filename;
  int version;

  if(!(in >> version >> filename)){
    out << "usage: d <version> <filename>" << endl;
    return false;
  }

  extractor.dump_binary(filename, version);
  return true;
}

bool
answer(Extractor extractor, std::istream &in, std::ostream &out){
  char command;
  if(!(in >> command))
    return true;

  try {
    switch(command){
      case 'a':
        return answer_attribute(extractor, in, out);
      case 'b':
        return answer_best(extractor, in, out);
      case 'c':
        return answer_clear(extractor, in, out);
      case 'd':
        return answer_dump(extractor, in, out);
      case 'e':
        return answer_explain(extractor, in, out);
      case 'h':
        out << "%s commands" << endl;
        out << "  a <attribute> - return information about the attribute" << endl;
        out << "  b <object> - return the best match for the object" << endl;
        out << "  c - clear the current weighting" << endl;
        out << "  d - dump state to binary out" << endl;
        out << "  e <object1> <object2> - explain the score for objects 1 and 2" << endl;
        out << "  h - return this help" << endl;
        out << "  t <object> - return a extractor listing for the object" << endl;
        out << "  w <method> - apply weighting to the relations" << endl;
        out << "  z - toggle printing of query evaluation time" << endl;
        out << "  q - quit" << endl;
        return true;
      case 'k':
        return answer_canonical(extractor, in, out);
      case 'm':
        return answer_measure(extractor, in, out);
      case 'n':
        return answer_relations(extractor, in, out);
      case 'o':
        return answer_object(extractor, in, out);
      case 'p':
        return answer_pseudo(extractor, in, out);
      case 'q':
        exit(0);
      case 'r':
        return answer_rank(extractor, in, out);
      case 't':
        return answer_extractor(extractor, in, out);
      case 'w':
        return answer_weight(extractor, in, out);
      case 'x':
        return answer_types(extractor, in, out);
      default:
        out << "extractor: unrecognised option '" << command << "'\n";
        return false;
    }
  }catch(NLP::Exception e){
    out << "extractor: " << e.what() << endl;
    return false;
  }catch(NLP::IOException e){
    out << "extractor: " << e.what() << endl;
    return false;
  }
  return false;
}

} }
