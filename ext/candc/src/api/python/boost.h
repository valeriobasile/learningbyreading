/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

namespace py = boost::python;

using namespace py;

typedef return_value_policy<copy_const_reference> ret_ccr;
typedef return_value_policy<copy_non_const_reference> ret_cncr;
typedef return_internal_reference<1> ret_ir1;
typedef with_custodian_and_ward<1,2> wcw12;
typedef with_custodian_and_ward<1,3> wcw13;
typedef with_custodian_and_ward<1,4> wcw14;
typedef with_custodian_and_ward<1,5> wcw15;
typedef with_custodian_and_ward<1,2, wcw13> wcw12_13;
typedef with_custodian_and_ward<1,2, wcw14> wcw12_14;
typedef with_custodian_and_ward<1, 2, with_custodian_and_ward<1, 3, wcw14> > wcw12_13_14;
typedef with_custodian_and_ward<1, 2, with_custodian_and_ward<1, 3, with_custodian_and_ward<1, 4, wcw15> > > wcw12_13_14_15;

namespace pyNLP {

  extern PyObject *error;
  extern PyObject *ioerror;
  extern PyObject *configerror;

  class TypeError: std::exception {
  public:
    std::string msg;

    TypeError(const std::string &msg): msg(msg){}
    virtual ~TypeError(void) throw(){ /* do nothing */ }
  };

  extern void register_exception_translators(void);

}
