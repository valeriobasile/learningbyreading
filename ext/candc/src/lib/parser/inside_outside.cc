// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_parser.h"

#include <map>

using namespace std;

namespace NLP { namespace CCG {

typedef map<ulong,double> Sums;

using namespace NLP::Tree;

Filled *
InsideOutside::load_dependency(const std::string &filename, const std::string &line) const {
  istringstream in(line);

  ulong head;
  RelID rel;
  ulong filler;
  CatID lrange;
  ulong rule;

  if(in >> head >> rel >> filler >> lrange >> rule)
    return new (pool) Filled(head, rel, filler, lrange, rule);
  else
    throw NLP::IOException("could not interpret dependency", filename);
}

bool
InsideOutside::read_dependencies(const std::string filename, std::istream &in){
  depscores.clear();
  pool->clear();

  ulong ndeps = 0;
  std::string line;
  while(getline(in, line)){
    if(line.length() == 0)
      break;
    Filled *filled = load_dependency(filename, line);
    depscores.add(filled, 1.0);
    ++ndeps;
  }

  return ndeps != 0;
}

int
InsideOutside::count_gold_deps(const SuperCat *sc) const {
  int ngold = 0;

  for(const Filled *filled = sc->filled; filled; filled = filled->next){
    if(depscores[filled] > 0.0)
      ngold++;
    else if(!PARTIAL_GOLD)  // partial_gold allows incorrect deps in a "correct" derivation
      return -1;
  }
  return ngold;
}


double
InsideOutside::conj_calc_inside(SuperCat *conj){
  double inside = conj->score;

  SuperCat *left = const_cast<SuperCat *>(conj->left);
  SuperCat *right = const_cast<SuperCat *>(conj->right);
  
  if(left){
    if(right)
      inside += left->d_inside + right->d_inside;
    else
      inside += left->d_inside;
  }

  return conj->inside = inside;
}

void
InsideOutside::conj_calc_outside(SuperCat *conj, double outside, double invZ){
  double sum = conj->score + outside;
  SuperCat *left = const_cast<SuperCat *>(conj->left);
  SuperCat *right = const_cast<SuperCat *>(conj->right);

  if(left){
    if(right){
      if(left->is_visited())
	left->outside = add_logs(left->outside, right->d_inside + sum);
      else{
	left->outside = right->d_inside + sum;
	left->mark_visited();
      }

      if(right->is_visited())
	right->outside = add_logs(right->outside, left->d_inside + sum);
      else{
	right->outside = left->d_inside + sum;
	right->mark_visited();
      }
    }else{
      if(left->is_visited())
	left->outside = add_logs(left->outside, sum);
      else{
	left->outside = sum;
	left->mark_visited();
      }
    }
  }

  double p = exp(outside + conj->inside + invZ);

  for(const Filled *filled = conj->filled; filled; filled = filled->next)
    depscores.add(filled, p);
}

void
InsideOutside::disj_calc_inside(SuperCat *disj){
  double d_inside = conj_calc_inside(disj);
  for(SuperCat *conj = disj->next; conj; conj = conj->next)
    d_inside = add_logs(d_inside, conj_calc_inside(conj));

  disj->d_inside = d_inside;
}
  
void
InsideOutside::disj_calc_outside(SuperCat *disj, double invZ){
  for(SuperCat *conj = disj; conj; conj = conj->next)
    conj_calc_outside(conj, disj->outside, invZ);
}

double
InsideOutside::calc_inside(Chart &chart){
  for(ulong i = 0; i != chart.ncells; ++i){
    Cell &cell = chart.cells[i];
    for(Cell::iterator j = cell.begin(); j != cell.end(); ++j)
      if((*j)->marker)
        disj_calc_inside(*j);
  }

  Cell &root = chart.root();
  if(root.empty())
    return 0.0;

  Cell::iterator i = root.begin();
  double Z = (*i)->d_inside;
  for(++i; i != root.end(); ++i)
    Z = add_logs(Z, (*i)->d_inside);

  return Z;
}
  
void
InsideOutside::calc_outside(Chart &chart, double invZ){
  depscores.clear();

  // iterating backwards over the disj nodes should ensure all parents
  // are dealt with before any children

  for(long i = chart.ncells - 1; i >= 0; --i){
    Cell &cell = chart.cells[i];

    //TODO use a reverse iterator here

    //need to go backwards over cell too 
    //to ensure unary-rule parents are dealt with before child
    for(Cell::iterator j = cell.end() - 1; j != cell.begin() - 1; --j)
      if((*j)->is_active())
	disj_calc_outside(*j, invZ);
  }
}


double
InsideOutside::calc_stats(Chart &chart, ulong &nequiv, ulong &ntotal){
  nequiv = 0;
  ntotal = 0;

  for(ulong i = 0; i != chart.ncells; ++i){
    Cell &cell = chart.cells[i];
    for(Cell::iterator j = cell.begin(); j != cell.end(); ++j){
      nequiv++;
      for(SuperCat *next = *j; next; next = next->next){
        next->score = 0.0;
	ntotal++;
      }
    }
  }

  return calc_inside(chart);
}

} }
