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
#include <algorithm>

#include <iostream>
#include <iomanip>
#include <fstream>
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

#include "thesaurus/match.h"

#include "thesaurus/measure.h"

namespace NLP { namespace Thesaurus {

void
Measure::best(Objects objects, std::ostream &out, Object *obj1, Match &match){
  if(previous == obj1){
    match = matches[0];
    return;
  }

  match.score = 0.0;
  match.object = 0;

  for(ulong i = 0; i < objects.size(); i++){
    Object *obj2 = (Object *)objects[i];
    if(obj2 == 0 || obj1 == obj2)
      continue;
    float score = measure(obj1, obj2);
    if(score > match.score){
      match.score = score;
      match.object = obj2;
    }
  }
}

void
Measure::all(Objects objects, std::ostream &out, Object *obj){
  if(previous == obj)
    return;

  ulong hskipped = 0;
  matches.clear();

  if(verbose){
    out << obj->str() << ' ' << " has " << obj->relations.size() << " attributes, ";
    out << obj->canonical.size() << " canonical" << endl;
  }

  for(ulong i = 0; i < objects.size(); i++){
    Object *other = (Object *)objects[i];
    if(other == 0 || obj == other || other->is_testset())
      continue;
    if(use_heuristic && !heuristic(obj, other)){
      hskipped++;
      continue;
    }
    float weight = measure(obj, other);
    if(weight)
      matches.push_back(Match(other, weight));
  }

  if(verbose){
    out << "found "  << matches.size() << " matches for " << obj->str();
    out << " using " << NAME << endl;
    out << "heuristic ignored " << hskipped << " comparisons (";
    out << setprecision(2) << (hskipped/100.0*objects.size()) << "%)" << endl;
  }

  std::sort(matches.begin(), matches.end(), MatchGTComp());
  previous = obj;
}

void
Measure::thesaurus(Objects objects, std::ostream &out){
  for(ulong i = 0; i < objects.size(); i++){
    Object *object = objects[i];
    if(object == 0)
      continue;

    matches.clear();
    for(ulong j = 0; j < objects.size(); j++){
      Object *other = objects[j];
      if(other == 0 || object == other || other->is_testset())
        continue;

      float weight = measure(object, other);
      if(weight)
        matches.push_back(Match(other, weight));
    }
    std::sort(matches.begin(), matches.end(), MatchGTComp());

    out << object->str() << ' ' << object->freq();
 
    ulong nresults = 50 < matches.size() ? 50 : matches.size();
    for(ulong j = 0; j < nresults; j++)
      out << ' ' << matches[j].object->str() << setprecision(4) <<  matches[j].score;
    out << endl;
  }
}

void
Measure::thesaurus(Objects objects, const std::string &input, ulong print_limit){
  std::ifstream in(input.c_str());
  char buffer[1024];

  while(in.getline(buffer, sizeof(buffer))){
    string objstr(buffer);
    Object *object = objects.find(buffer);
    if(object == 0){
      cerr << "object '" << objstr << "' does not exist" << endl;
      continue;
    }

    if(verbose){
      object->printcanonical(cerr);
      cerr << endl;
    }

    matches.clear();
    ulong hskipped = 0;
    for(ulong j = 0; j < objects.size(); j++){
      Object *other = objects[j];
      if(other == 0 || object == other || other->is_testset())
        continue;

      if(use_heuristic && !heuristic(object, other)){
        hskipped++;
        continue;
      }

      float weight = measure(object, other);
      if(weight)
        matches.push_back(Match(other, weight));
    }

    if(verbose){
      cerr << "found " << matches.size() << " matches for ";
      cerr << buffer << " using " << NAME << endl;
    }
    if(use_heuristic){
      ulong npotentials = object->npotentials();
      if(verbose){
        cerr << npotentials << " potential matches by heuristics (";
        cerr << setprecision(2) << (npotentials/100.0*objects.size()) << "%)\n";
        cerr << "heuristics ignored " << hskipped << " comparisons (";
        cerr << setprecision(2) << (hskipped/100.0*objects.size()) << "%)\n";
        cerr << "matched " << setprecision(2) << (matches.size()/100.0*npotentials);
        cerr << " of potential matches" << endl;
      }
    }
    if(verbose){
      cerr << "matched " << (matches.size()/100.0*objects.size());
      cerr << "% of maximum matches\n";
    }

    std::sort(matches.begin(), matches.end(), MatchGTComp());

    cout << object->str() << ' ' << object->freq();
    ulong nresults = print_limit < matches.size() ? print_limit : matches.size();
    for(ulong j = 0; j < nresults; j++)
      cout << ' ' << matches[j].object->str() << ' ' << setprecision(5) << matches[j].score;
    cout << endl;
  }
}

void
Measure::pairs(Objects objects, std::ostream &out){
  for(ulong i = 0; i < objects.size(); i++){
    Object *object = objects[i];
    if(object == 0)
      continue;

    Match best;
    matches.clear();

    for(ulong j = 0; j < objects.size(); j++){
      Object *other = objects[j];
      if(other == 0 || object == other || other->is_testset())
        continue;
      float score = measure(object, other);
      if(score > best.score){
        best.score = score;
        best.object = other;
      }
    }
    out << object->str() << ' ' << best.object->str() << ' ';
    out << setprecision(4) << best.score << endl;
  }
}

} }
