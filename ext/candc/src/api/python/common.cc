// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <boost/python.hpp>

#include "base.h"

using namespace std;

#include "boost.h"

namespace pyNLP {

  namespace py = boost::python;

  PyObject *ioerror = 0;
  PyObject *configerror = 0;
  PyObject *error = 0;

  void trans_error(const NLP::Exception &e){
    PyErr_SetString(error, e.msg.c_str());
    py::throw_error_already_set();
  }

  void trans_ioerror(const NLP::IOException &e){
    py::tuple msg;
    if(e.line > 0)
      msg = py::make_tuple(e.msg, e.uri, e.line);
    else if(e.uri != "")
      msg = py::make_tuple(e.msg, e.uri);
    else
      msg = py::make_tuple(e.msg);

    PyErr_SetObject(ioerror, msg.ptr());
    py::throw_error_already_set();
  }

  void trans_configerror(const NLP::ConfigError &e){
    py::tuple msg;
    if(e.line > 0)
      msg = py::make_tuple(e.option, e.msg, e.uri, e.line);
    else if(e.uri != "")
      msg = py::make_tuple(e.option, e.msg, e.uri);
    else
      msg = py::make_tuple(e.option, e.msg);

    PyErr_SetObject(ioerror, msg.ptr());
    py::throw_error_already_set();
  }

  void trans_typeerror(const TypeError &e){
    PyErr_SetString(PyExc_TypeError, e.msg.c_str());
    py::throw_error_already_set();
  }

  void
  register_exception_translators(void){
    py::register_exception_translator<NLP::Exception>(&pyNLP::trans_error);
    py::register_exception_translator<NLP::IOException>(&pyNLP::trans_ioerror);
    py::register_exception_translator<NLP::ConfigError>(&pyNLP::trans_configerror);
    py::register_exception_translator<pyNLP::TypeError>(&pyNLP::trans_typeerror);
  }

}
