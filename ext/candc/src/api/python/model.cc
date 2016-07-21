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
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

#include "base.h"

#include "config/config.h"

#include "model/model.h"

#include "model/types.h"

using namespace std;

BOOST_PYTHON_MODULE_INIT(model){
  using namespace NLP;
  namespace M = NLP::Model;
  using namespace boost::python;

  class_<M::Model, bases<Config::Cfg> >("Model", no_init)
    .def_readonly("comment", &M::Model::comment)
    .def_readonly("data", &M::Model::data)
    .def_readonly("update", &M::Model::update)
    .def_readonly("sigma", &M::Model::sigma)
    .def_readonly("niterations", &M::Model::niterations)
    .def_readonly("weights", &M::Model::weights);

  class_<M::Config, bases<Config::Directory> >("ModelConfig", no_init)
    .def_readonly("model", &M::Config::model);

  class_<M::Types, bases<Config::Cfg> >("ModelTypes", no_init)
    .def_readonly("use_words", &M::Types::use_words)
    .def_readonly("use_history", &M::Types::use_history)
    .def_readonly("use_pos", &M::Types::use_pos)
    .def_readonly("use_chunks", &M::Types::use_chunks)
    .def_readonly("use_prefix", &M::Types::use_prefix)
    .def_readonly("use_suffix", &M::Types::use_suffix)
    .def_readonly("use_has_uppercase", &M::Types::use_has_uppercase)
    .def_readonly("use_has_hyphen", &M::Types::use_has_hyphen)
    .def_readonly("use_has_digit", &M::Types::use_has_digit)
    .def_readonly("use_has_punct", &M::Types::use_has_punct)
    .def_readonly("use_has_period", &M::Types::use_has_period)
    .def_readonly("use_one_digit", &M::Types::use_one_digit)
    .def_readonly("use_two_digits", &M::Types::use_two_digits)
    .def_readonly("use_three_digits", &M::Types::use_three_digits)
    .def_readonly("use_four_digits", &M::Types::use_four_digits)
    .def_readonly("use_digits", &M::Types::use_digits)
    .def_readonly("use_number", &M::Types::use_number)
    .def_readonly("use_case", &M::Types::use_case)
    .def_readonly("use_alphanum", &M::Types::use_alphanum)
    .def_readonly("use_length", &M::Types::use_length)
    .def_readonly("use_roman", &M::Types::use_roman)
    .def_readonly("use_initial", &M::Types::use_initial)
    .def_readonly("use_acronym", &M::Types::use_acronym)
    .def_readonly("use_last", &M::Types::use_last)
    .def_readonly("use_gazetteers", &M::Types::use_gazetteers)
    .def_readonly("use_gaz_common", &M::Types::use_gaz_common)
    .def_readonly("use_gaz_first", &M::Types::use_gaz_first)
    .def_readonly("use_gaz_last", &M::Types::use_gaz_last)
    .def_readonly("use_gaz_loc", &M::Types::use_gaz_loc)
    .def_readonly("use_prev_gazetteers", &M::Types::use_prev_gazetteers)
    .def_readonly("use_next_gazetteers", &M::Types::use_next_gazetteers)
    .def_readonly("use_nu", &M::Types::use_nu)
    .def_readonly("use_nnu", &M::Types::use_nnu)
    .def_readonly("use_wordtypes", &M::Types::use_wordtypes)
    .def_readonly("use_bitypes", &M::Types::use_bitypes)
    .def_readonly("use_tritypes", &M::Types::use_tritypes)
    .def_readonly("use_composites", &M::Types::use_composites);
}
