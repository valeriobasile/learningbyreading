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

#include "config/config.h"
#include "io/format.h"
#include "config/format.h"

#include "io/reader.h"
#include "io/reader_factory.h"
#include "io/writer.h"
#include "io/writer_factory.h"

#include "model/model.h"
#include "model/types.h"

#include "tagger/tagdict.h"
#include "tagger/tagsetdict.h"
#include "tagger/tagger.h"

#include "tagger/pos.h"
#include "tagger/chunk.h"
#include "tagger/ner.h"
#include "tagger/super.h"

using namespace std;

#include "boost.h"

namespace pyTagger {
  using namespace NLP::Taggers;

  void tag_sentence(const Tagger &tagger, NLP::Sentence &sent, Algorithm alg, ulong DICT_CUTOFF){
    tagger.tag(sent, alg, DICT_CUTOFF);
  }

  void mtag_sentence(const Tagger &tagger, NLP::Sentence &sent, Algorithm alg, ulong DICT_CUTOFF, double BETA){
    tagger.mtag(sent, alg, DICT_CUTOFF, BETA);
  }

  const NLP::Tags &tags_word(const TagDict &tagdict, NLP::Word word){
    return tagdict[word];
  }

  const NLP::Tags &tags_string(const TagDict &tagdict, const std::string &s){
    return tagdict[s];
  }
}

BOOST_PYTHON_MODULE_INIT(tagger){
  namespace T = NLP::Taggers;
  namespace C = NLP::Config;
  namespace M = NLP::Model;

  pyNLP::register_exception_translators();

  scope().attr("VITERBI") = T::VITERBI;
  scope().attr("NOSEQ") = T::NOSEQ;
  scope().attr("GREEDY") = T::GREEDY;
  scope().attr("FWDBWD") = T::FWDBWD;

  class_<T::Tagger::Config, bases<M::Config> >("Config", no_init)
    .def_readonly("model", &T::Tagger::Config::model)
    .def_readonly("tagdict", &T::Tagger::Config::tagdict)
    .def_readonly("unknowns", &T::Tagger::Config::unknowns)
    .def_readonly("cutoff_default", &T::Tagger::Config::cutoff_default)
    .def_readonly("cutoff_words", &T::Tagger::Config::cutoff_words)
    .def_readonly("rare_cutoff", &T::Tagger::Config::rare_cutoff)
    .def_readonly("beam_width", &T::Tagger::Config::beam_width)
    .def_readonly("beam_ratio", &T::Tagger::Config::beam_ratio)
    .def_readonly("forward_beam_ratio", &T::Tagger::Config::forward_beam_ratio)
    .def_readonly("tagdict_min", &T::Tagger::Config::tagdict_min)
    .def_readonly("tagdict_ratio", &T::Tagger::Config::tagdict_ratio);

  class_<T::POS::Config, bases<T::Tagger::Config> >("POSConfig", init<>())
    .def(init<const C::OpPath *>());

  class_<T::Chunk::Config, bases<T::Tagger::Config> >("ChunkConfig", init<>())
    .def(init<const C::OpPath *>())
    .def_readonly("postags", &T::Chunk::Config::postags);

  class_<T::NER::Config, bases<T::Tagger::Config> >("NERConfig", init<>())
    .def(init<const C::OpPath *>())
    .def_readonly("postags", &T::NER::Config::postags)
    .def_readonly("chunktags", &T::NER::Config::chunktags)
    .def_readonly("gazetteers", &T::NER::Config::gazetteers)
    .def_readonly("types", &T::NER::Config::types);

  class_<T::Super::Config, bases<T::Tagger::Config> >("SuperConfig", init<>())
    .def(init<const C::OpPath *>())
    .def_readonly("category_cutoff", &T::Super::Config::category_cutoff)
    .def_readonly("postags", &T::Super::Config::postags)
    .def_readonly("posdict", &T::Super::Config::posdict);

  class_<T::TagDict>("TagDict", init<const std::string &, const std::string &,
		     const NLP::TagSet &, const NLP::Lexicon &>())
    .def(init<const std::string &, const std::string &, double, ulong,
	 const NLP::TagSet &, const NLP::Lexicon &>())
    .def(init<const T::TagDict &>())
    .add_property("name", make_function(&T::TagDict::name, ret_ccr()))
    .add_property("size", &T::TagDict::size)
    .def("tags", pyTagger::tags_word, ret_ccr())
    .def("tags", pyTagger::tags_string, ret_ccr())
    .def("__getitem__", pyTagger::tags_word, ret_ccr())
    .def("__getitem__", pyTagger::tags_string, ret_ccr())
    .def("__len__", &T::TagDict::size);

  class_<T::Tagger, boost::noncopyable>("Tagger", no_init)
    .add_property("tagset", &T::Tagger::tagset)
    .add_property("lexicon", &T::Tagger::lexicon)
    .add_property("tagdict", &T::Tagger::tagdict)
    .def("tag", &pyTagger::tag_sentence)
    .def("mtag", &pyTagger::mtag_sentence);

  class_<T::POS, bases<T::Tagger>, boost::noncopyable>("POS", init<T::POS::Config &>());

  class_<T::Chunk, bases<T::Tagger>, boost::noncopyable>("Chunk", init<T::Chunk::Config &>());

  class_<T::NER, bases<T::Tagger>, boost::noncopyable>("NER", init<T::NER::Config &>());

  class_<T::Super, bases<T::Tagger>, boost::noncopyable>("Super", init<T::Super::Config &>());
}
