// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "config/config.h"

#include "utils.h"

#include "pool.h"

#include "parser/parser.h"

using namespace std;

namespace NLP { namespace CCG {

Categories::Categories(const std::string &dir, const std::string &markedup,
		       const bool ALT_MARKEDUP)
  : pool(1 << 17),
    markedup("markedup"), relations("relations"), seen("seen"){
  _read_markedup(markedup, ALT_MARKEDUP);

  relations.init_conj(*this);
  _read_typeraise(dir + "/trNP", trNP);
  _read_typeraise(dir + "/trPP", trPP);
  _read_typeraise(dir + "/trAP", trAP);
  _read_typeraise(dir + "/trVP_to", trVP_to);
}

Categories::~Categories(void){}

void
Categories::_read_markedup(const std::string &filename, const bool ALT_MARKEDUP){
  markedup.add("+", "+", 0);

  ulong nlines = 0;
  ifstream in(filename.c_str());
  if(!in)
    throw NLP::IOException("could not open markedup file for reading", filename);

  PREFACE = read_preface(filename, in, nlines);

  try {
    string buffer;
    string plain_str, markedup_str;
    VarID seen[Vars::NVARS];
    VarID order;

    enum STATES { CAT, MARKEDUP, GRS } state;
    state = CAT;

    ulong maxslots = 0;
    ulong nslots = 0;
    while(getline(in, buffer)){
      ++nlines;

      if(buffer.length() == 0){
	if(state != GRS)
	  throw NLP::Exception("missing markedup or GRS");

	state = CAT;
	continue;
      }

      if(buffer[0] == '#')
	continue;

      if(buffer[0] == '='){
	istringstream line(buffer);
	string group, word;
	line >> group;
	while(line >> word)
	  gr_constraints.add(group, word);
	continue;
      }

      istringstream line(buffer);
      ulong slot = 0;
      std::string gr;
      Cat *cat = 0;
      switch(state){
      case CAT:
	line >> plain_str;
	state = MARKEDUP;
	break;
      case MARKEDUP:
	line >> maxslots >> markedup_str;
	cat = _parse(markedup_str.c_str(), markedup_str.c_str());
	memset(seen, 0, sizeof(seen));
	order = 0;
	cat->reorder(seen, order);
	markedup.add(plain_str, markedup_str, cat);
	state = GRS;
	nslots = 0;
	break;
      case GRS:
	char tmp;
	++nslots;
	line >> tmp;
	if(tmp == '!'){
	  if(!ALT_MARKEDUP)
	    continue;

	  line >> markedup_str;
	  Cat *cat = _parse(markedup_str.c_str(), markedup_str.c_str());
	  memset(seen, 0, sizeof(seen));
	  order = 0;
	  cat->reorder(seen, order);
	  markedup.add(plain_str, markedup_str, cat);
	  continue;
	}
	slot = tmp - '0';
	if(line.get() != ' ')
	  throw NLP::IOException("there should be a space after the RelID", filename, nlines);
	getline(line, gr);
	relations.add_gr(*this, markedup_str, slot, gr);
	break;
      }
    }
  }catch(NLP::Exception e){
    throw NLP::IOException(e.msg, filename, nlines);
  }

  if(!in.eof())
    throw NLP::IOException("could not read markedup entry", filename);

  relations.set_constraints(*this);
  relations.set_cats(*this);
}

void
Categories::_read_typeraise(const std::string &filename, TRCats &tr){
  ifstream in(filename.c_str());
  if(!in)
    throw NLP::IOException("could not open type raise file for reading", filename);

  string markedup_str, dep_str;
  VarID seen[Vars::NVARS];
  VarID order;
  while(in >> markedup_str >> dep_str){
    if(markedup_str[0] == '#')
      continue;

    Cat *cat = _parse(markedup_str.c_str(), markedup_str.c_str());
    VarID dep(dep_str);
    memset(seen, 0, sizeof(seen));
    order = 0;
    cat->reorder(seen, order);
    tr.push_back(TRCat(cat, seen[Vars::X], seen[dep]));
  }

  if(!in.eof())
    throw NLP::IOException("could not read plain entry", filename);
}

Cat *
Categories::_parse(const char *current, const char *orig){
  ulong njulia_slots = 0;
  Cat *cat = _consume_category(current, orig, true, njulia_slots);

  if(*current)
    throw NLP::Exception("could not parse category string");

  return cat;
}

Cat *
Categories::_consume_category(const char *&current, const char *orig,
			      bool in_result, ulong &njulia_slots){
  switch(*current){
    case '(':
      return _consume_complex(++current, orig, in_result, njulia_slots);
    case ')':
    case '\\':
    case '/':
      throw NLP::Exception("could not parse category string");
    default:
      return _consume_basic(current, orig, njulia_slots);
  }
}

Cat *
Categories::_consume_basic(const char *&current, const char *orig,
			   ulong njulia_slots){
  if(!isalpha(*current) &&
      *current != ',' && *current != '.' &&
      *current != ';' && *current != ':')
    throw Exception("unexpected token parsing category in consume_basic");

  std::string basic_str;
  while(isalpha(*current) ||
        *current == ',' || *current == '.' ||
        *current == ';' || *current == ':')
    basic_str += *current++;

  Atom atom(basic_str);

  Feature feature = Features::NONE;
  if(*current == '[')
    feature = _consume_feature(++current, orig);

  VarID var = Vars::NONE;
  CatID lrange = 0;
  if(*current == '{')
    var = _consume_var(++current, orig, lrange);

  ulong slot = 0;
  if(*current == '<')
    slot = _consume_slot(++current, orig, njulia_slots);

  if(*current == '<' || *current == '(' || *current == '!')
    throw Exception("unexpected nesting or dependency parsing basic category");

  return Cat::Basic(&pool, atom, feature, var, slot, lrange);
}

Cat *
Categories::_consume_complex(const char *&current, const char *orig,
			     bool in_result, ulong &njulia_slots){
  Cat *left = _consume_category(current, orig, in_result, njulia_slots);

  Slash slash = 0;
  if(*current == '/')
    slash = FWD;
  else if(*current == '\\')
    slash = BWD;
  else
    throw NLP::Exception("expected a forward or backward slash" + string(orig));

  ++current;

  if(in_result)
    ++njulia_slots;

  Cat *right = _consume_category(current, orig, false, njulia_slots);

  if(*current != ')')
    throw NLP::Exception("unbalanced parentheses in category string");
  ++current;

  VarID var = Vars::NONE;
  CatID lrange = 0;
  if(*current == '{')
    var = _consume_var(++current, orig, lrange);

  ulong slot = 0;
  if(*current == '<')
    slot = _consume_slot(++current, orig, njulia_slots);

  if(*current == '<' || *current == '(' || *current == '!')
    throw Exception("unexpected nesting or dependency parsing basic category");

  return Cat::Complex(&pool, left, slash, right, var, slot, lrange);
}

Feature
Categories::_consume_feature(const char *&current, const char *){
  std::string feature_str;
  while(isalpha(*current))
    feature_str += *current++;

  if(*current != ']')
    throw Exception("unexpected character parsing feature");
  ++current;

  return Feature(feature_str.c_str());
}

VarID
Categories::_consume_var(const char *&current, const char *, CatID &lrange){
  std::string head_str;
  while(isalpha(*current) || *current == '_')
    head_str += *current++;

  if(*current == '*'){
    lrange = markedup.size();
    ++current;
  }

  if(*current != '}')
    throw Exception("unexpected character parsing variable");
  ++current;

  return VarID(head_str);
}

RelID
Categories::_consume_slot(const char *&current, const char *orig, ulong jslot){
  std::string slot_str;
  while(isdigit(*current))
    slot_str += *current++;

  if(*current != '>')
    throw Exception("unexpected character parsing dependency");
  ++current;

  ulong slot;
  if(istringstream(slot_str) >> slot)
    return relations(orig, slot, jslot);
  else
    throw Exception("could not convert to slot number");

  return 0;
}

//used to create new canonical categories from a cat pointer
const Cat *
Categories::canonize(const Cat *cat){
  const Cat *concat;
  if((concat = canonical[cat]))
    return concat;

  concat = Cat::Clone(&pool, cat);
  canonical.add(concat);
  return concat;
}

const Constraint *
Categories::constraint(const std::string &s){
	istringstream in(s);
	int pos, span;
	string type, catstr;
	const Cat *cat = 0;

	cerr << "constraint: " << s << endl;
	if(!(in >> type >> pos >> span))
		throw Exception("failed to parse constraint " + s);

	if(type == "match"){
		if(!(in >> catstr))
			throw Exception("failed to read category in match constraint " + s);
		cat = markedup[catstr];
		if(!cat)
			throw NLP::ParseError("attempted to use category without markedup in constraint " + s);
		cerr << "loaded category " << *cat << " from constraint " << s << endl;
	}else if(type != "require" && type != "exclude")
		throw Exception("unrecognised type of constraint " + s);

	return new Constraint(pos - 1, span, type == "require", type == "exclude", cat);
}

} }
