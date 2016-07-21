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

  std::string word_repr(const NLP::Word &word){
    if(word == NLP::NONE)
      return "<Word __NONE__>";
    else if(word == NLP::SENTINEL)
      return "<Word __SENTINEL__>";
    else{
      ostringstream out;
      out << "<Word '" << word.str() << "'>";
      return out.str();
    }
  }

  std::string tag_repr(const NLP::Tag &tag){
    if(tag == NLP::NONE)
      return "<Word __NONE__>";
    else if(tag == NLP::SENTINEL)
      return "<Word __SENTINEL__>";
    else{
      ostringstream out;
      out << "<Tag " << tag.value() << '>';
      return out.str();
    }
  }

  void list2raws(NLP::Raws &raws, const py::list &pyraws){
    int nwords = py::len(pyraws);

    raws.clear();
    raws.reserve(nwords);

    for(int i = 0; i < nwords; ++i)
      raws.push_back(py::extract<std::string>(pyraws[i]));
  }

  void scoredraw_assign(NLP::ScoredRaw &scoredraw, const NLP::Raw &raw, float score){
    scoredraw.raw = raw;
    scoredraw.score = score;
  }

  std::string scoredraw_repr(const NLP::ScoredRaw &sr){
    ostringstream out;
    out << "<ScoredRaw " << sr.raw << ' ' << sr.score << '>';
    return out.str();
  }

  void obj2scoredraw(NLP::ScoredRaw &scoredraw, const py::object &pyobj){
    py::extract<std::string> as_string(pyobj);
    if(as_string.check()){
      scoredraw.score = 1.0;
      scoredraw.raw = as_string;
      return;
    }else if(py::len(pyobj) == 2){
      extract<std::string> raw(pyobj[0]);
      extract<float> score(pyobj[1]);
      if(raw.check() && score.check()){
        scoredraw.raw = raw;
        scoredraw.score = score;
        return;
      }
      throw TypeError("a ScoredRaw must be assigned a string/float pair");
    }

    throw TypeError("a ScoredRaw must be assigned either a string or a sequence of length 2"); 
  }

  void obj2multiraw(NLP::MultiRaw &mraw, const py::object &pyobj){
    extract<std::string> as_string(pyobj);
    if(as_string.check()){
      mraw.clear();
      mraw.push_back(NLP::ScoredRaw(as_string(), 1.0));
    }else{
      int ntags = len(pyobj);
      mraw.clear();
      mraw.reserve(ntags);
      for(int i = 0; i < ntags; ++i){
        NLP::ScoredRaw tmp;
        obj2scoredraw(tmp, pyobj[i]);
        mraw.push_back(tmp);
      }
    }
  }

  void list2multiraws(NLP::MultiRaws &mraws, const py::object &pyobj){
    int nwords = len(pyobj);
    mraws.clear();
    mraws.reserve(nwords);
    for(int i = 0; i < nwords; ++i){
      NLP::MultiRaw mraw;
      obj2multiraw(mraw, pyobj[i]);
      mraws.push_back(mraw);
    }
  }

  struct Sentence {
    static NLP::Raws &get_words(NLP::Sentence &sent){ return sent.words; }
    static void set_words(NLP::Sentence &sent, const py::list &l){
      list2raws(sent.words, l);
    }

    static NLP::Raws &get_lemmas(NLP::Sentence &sent){ return sent.lemmas; }
    static void set_lemmas(NLP::Sentence &sent, const py::list &l){
      list2raws(sent.lemmas, l);
    }

    static NLP::Raws &get_pos(NLP::Sentence &sent){ return sent.pos; }
    static void set_pos(NLP::Sentence &sent, const py::list &l){
      list2raws(sent.pos, l);
    }

    static NLP::Raws &get_chunks(NLP::Sentence &sent){ return sent.chunks; }
    static void set_chunks(NLP::Sentence &sent, const py::list &l){
      list2raws(sent.chunks, l);
    }

    static NLP::Raws &get_entities(NLP::Sentence &sent){ return sent.entities; }
    static void set_entities(NLP::Sentence &sent, const py::list &l){
      list2raws(sent.entities, l);
    }

    static NLP::Raws &get_super(NLP::Sentence &sent){ return sent.super; }
    static void set_super(NLP::Sentence &sent, const py::list &l){
      list2raws(sent.super, l);
    }

    static NLP::MultiRaws &get_mpos(NLP::Sentence &sent){ return sent.mpos; }
    static void set_mpos(NLP::Sentence &sent, const py::list &l){
      list2multiraws(sent.mpos, l);
    }

    static NLP::MultiRaws &get_mchunks(NLP::Sentence &sent){ return sent.mchunks; }
    static void set_mchunks(NLP::Sentence &sent, const py::list &l){
      list2multiraws(sent.mchunks, l);
    }

    static NLP::MultiRaws &get_mentities(NLP::Sentence &sent){ return sent.mentities; }
    static void set_mentities(NLP::Sentence &sent, const py::list &l){
      list2multiraws(sent.mentities, l);
    }

    static NLP::MultiRaws &get_msuper(NLP::Sentence &sent){ return sent.msuper; }
    static void set_msuper(NLP::Sentence &sent, const py::list &l){
      list2multiraws(sent.msuper, l);
    }
  };

}

static char candc_error_name[] = "candc.Error";
static char candc_ioerror_name[] = "candc.IOError";
static char candc_configerror_name[] = "candc.ConfigError";

BOOST_PYTHON_MODULE_INIT(base){
  using namespace NLP;
  using namespace py;

  // exception handling
  pyNLP::error = PyErr_NewException(candc_error_name, 0, 0);
  scope().attr("Error") = object(detail::new_reference(pyNLP::error));

  pyNLP::ioerror = PyErr_NewException(candc_ioerror_name, 0, 0);
  scope().attr("IOError") = object(detail::new_reference(pyNLP::ioerror));

  pyNLP::configerror = PyErr_NewException(candc_configerror_name, 0, 0);
  scope().attr("ConfigError") = object(detail::new_reference(pyNLP::configerror));

  pyNLP::register_exception_translators();

  class_<None>("None", init<>());
  class_<Sentinel>("Sentinel", init<>());

  scope().attr("NONE") = NONE;
  scope().attr("SENTINEL") = SENTINEL;

  scope().attr("__version__") = VERSION;

  class_<Word>("Word", init<>())
    .def(init<None>())
    .def(init<Sentinel>())
    .def(init<const Word &>())
    .def("freq", &Word::freq)
    .def("__hash__", &Word::hash)
    .def("__str__", &Word::str)
    .def("__repr__", &pyNLP::word_repr)
    .def(self == self)
    .def(self != self)
    .def(self == other<None>())
    .def(self == other<Sentinel>())
    .def(self != other<None>())
    .def(self != other<Sentinel>());

  class_<Tag>("Tag", init<ulong>())
    .def(init<None>())
    .def(init<Sentinel>())
    .def(init<const Word &>())
    .def("__int__", &Tag::value)
    .def("__repr__", &pyNLP::tag_repr)
    .def(self == self)
    .def(self != self)
    .def(self < self)
    .def(self == other<None>())
    .def(self == other<Sentinel>())
    .def(self != other<None>())
    .def(self != other<Sentinel>());

  class_<TagSet>("TagSet", init<const std::string &, const std::string &>())
    .def(init<const std::string &>())
    .def(init<const TagSet &>())
    .add_property("name", make_function(&TagSet::name, ret_ccr()))
    .add_property("size", &TagSet::size)
    .def("str", &TagSet::str_checked)
    .def("can", (Tag (TagSet::*)(const std::string &) const)&TagSet::tag)
    .def("check", &TagSet::check)
    .def("load", &TagSet::load)
    .def("__getitem__", &TagSet::str_checked)
    .def("__getitem__", (Tag (TagSet::*)(const std::string &) const)&TagSet::tag)
    .def("__len__", &TagSet::size);

  class_<Lexicon>("Lexicon", init<const std::string &, const std::string &>())
    .def(init<const std::string &>())
    .def(init<const Lexicon &>())
    .add_property("name", make_function(&Lexicon::name, ret_ccr()))
    .add_property("size", &Lexicon::size)
    .def("can", (Word (Lexicon::*)(const std::string &) const)&Lexicon::can)
    .def("freq", (ulong (Lexicon::*)(const std::string &) const)&Lexicon::freq)
    .def("clear", &Lexicon::clear)
    .def("load", &Lexicon::load)
    .def("save", (void (Lexicon::*)(const std::string &, const std::string &) const)&Lexicon::save)
    .def("sort_by_alpha", &Lexicon::sort_by_alpha)
    .def("sort_by_freq", &Lexicon::sort_by_freq)
    .def("sort_by_rev_freq", &Lexicon::sort_by_rev_freq)
    .def("__getitem__", (Word (Lexicon::*)(const std::string &) const)&Lexicon::can)
    .def("__len__", &Lexicon::size);

  class_<ScoredTag>("ScoredTag")
    .def_readwrite("score", &ScoredTag::score)
    .def_readwrite("tag", &ScoredTag::tag);

  class_<ScoredRaw>("ScoredRaw", init<>())
    .def(init<const std::string &, float>())
    .def_readwrite("score", &ScoredRaw::score)
    .def_readwrite("raw", &ScoredRaw::raw)
    .def("assign", &pyNLP::obj2scoredraw)
    .def("assign", &pyNLP::scoredraw_assign)
    .def("__repr__", &pyNLP::scoredraw_repr);

  class_<Raws>("Raws")
    .def(vector_indexing_suite<Raws, true>());

  class_<MultiRaw>("MultiRaw")
    .def(vector_indexing_suite<MultiRaw>())
    .def("assign", &pyNLP::obj2multiraw)
    .def(self == self);

  class_<MultiRaws>("MultiRaws")
    .def(vector_indexing_suite<MultiRaws>());

  class_<Tags>("Tags")
    .def(vector_indexing_suite<Tags>());

  class_<MultiTag>("MultiTag")
    .def(vector_indexing_suite<MultiTag>());

  class_<MultiTags>("MultiTags")
    .def(vector_indexing_suite<MultiTags>());

  class_<Sentence>("Sentence")
    .def("clear", &Sentence::reset)
    .def("reset", &Sentence::reset)
    .def("copy_multi", &Sentence::copy_multi)
    .add_property("words", make_function(&pyNLP::Sentence::get_words, ret_ir1()),
                  &pyNLP::Sentence::set_words)
    .add_property("lemmas", make_function(&pyNLP::Sentence::get_lemmas, ret_ir1()),
                  &pyNLP::Sentence::set_lemmas)
    .add_property("pos", make_function(&pyNLP::Sentence::get_pos, ret_ir1()),
                  &pyNLP::Sentence::set_pos)
    .add_property("chunks", make_function(&pyNLP::Sentence::get_chunks, ret_ir1()),
                  &pyNLP::Sentence::set_chunks)
    .add_property("entities", make_function(&pyNLP::Sentence::get_entities, ret_ir1()),
                  &pyNLP::Sentence::set_entities)
    .add_property("super", make_function(&pyNLP::Sentence::get_super, ret_ir1()),
                  &pyNLP::Sentence::set_super)
    .add_property("mpos", make_function(&pyNLP::Sentence::get_mpos, ret_ir1()),
                  &pyNLP::Sentence::set_mpos)
    .add_property("mchunks", make_function(&pyNLP::Sentence::get_mchunks, ret_ir1()),
                  &pyNLP::Sentence::set_mchunks)
    .add_property("mentities", make_function(&pyNLP::Sentence::get_mentities, ret_ir1()),
                  &pyNLP::Sentence::set_mentities)
    .add_property("msuper", make_function(&pyNLP::Sentence::get_msuper, ret_ir1()),
                  &pyNLP::Sentence::set_msuper);
}
