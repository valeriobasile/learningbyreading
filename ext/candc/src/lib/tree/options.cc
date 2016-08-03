// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "std.h"

using namespace std;

#include "utils.h"
#include "exception.h"

// TODO put fields.h temporarily in here to compile

// read a field name and throw if the read name does not match the expected value
inline void
expect_field(const std::string &filename, std::istream &s, std::string field){
  std::string name;

  if(!(s >> name))
    throw NLP::IOException("unexpected EOF reading attribute '" + field + "'", filename);
  if(name != field)
    throw NLP::IOException("attribute '" + name + "' does not match expected '" + field + "'", filename);
}

// read in a value and throw if it is the wrong type
template <class T>
void
read_field(const std::string &filename, std::istream &s, std::string field, T &value){
  if(!(s >> value))
    throw NLP::IOException("unexpected EOF reading attribute value for '" + field + "'", filename);
}

// template specialisation for booleans, handles true/1 and false/0
template <>
inline void
read_field(const std::string &filename, std::istream &s, std::string field, bool &value){
  std::string tmp;
  if(!(s >> tmp))
    throw NLP::IOException("unexpected EOF reading attribute value for '" + field + "'", filename);

  if(tmp == "true" || tmp == "1")
    value = true;
  else if(tmp == "false" || tmp == "0")
    value = false;
  else
    throw NLP::IOException("unexpected non-boolean attribute value for '" + field + "'", filename);
}

// wrap up expecting a field and then reading in the value
template <class T>
void
load_field(const std::string &filename, std::istream &s, std::string field, T &value){
  expect_field(filename, s, field);
  read_field(filename, s, field, value);
}

#include "tree/options.h"

namespace NLP { namespace Tree {

inline char
to_lower(const char c){ return std::tolower(c); }

unsigned long int
Options::Smoothing::from_string(std::string str){
  transform(str.begin(), str.end(), str.begin(), to_lower);
  if(str == "none")
    return NONE;
  else if(str == "gaussian")
    return GAUSSIAN;
  else if(str == "gamma"){
    assert(!"gamma smoothing not implemented yet");
    return GAMMA;
  }else{
    assert(!"unrecognised smoothing option");
    return NONE;
  }
}

std::string
Options::Smoothing::to_string(ulong value){
  switch(value){
    case NONE: return "none";
    case GAUSSIAN: return "gaussian";
    case GAMMA: return "gamma";
    default: assert(!"unexpected value for smoothing");
  }
  return "none";
}

Options::Options(string filename): control(filename){
  ifstream stream(control.c_str());
  assert(stream || !"could not open control file");

  expect_field(control, stream, "base");
  read_field(control, stream, "base", base);
  expect_field(control, stream, "model");
  read_field(control, stream, "model", model);
  expect_field(control, stream, "iterations");
  read_field(control, stream, "iterations", niterations);

  string s;
  expect_field(control, stream, "smoothing");
  read_field(control, stream, "smoothing", s);
  smoothing = Smoothing::from_string(s);

  alpha = 0.0;
  if(smoothing == Smoothing::GAUSSIAN){
    expect_field(control, stream, "alpha");
    read_field(control, stream, "alpha", alpha);
  }
  //sc: added this
  expect_field(control, stream, "norm_form");
  read_field(control, stream, "norm_form", norm_form);

  stream.close();
  stream.clear();

  info = model + "/info";

  stream.open(info.c_str());
  assert(stream || !"could not open info file");

  expect_field(info, stream, "nsentences");
  read_field(info, stream, "nsentences", nsentences);
  expect_field(info, stream, "nfeatures");
  read_field(info, stream, "nfeatures", nfeatures);
};

} }
