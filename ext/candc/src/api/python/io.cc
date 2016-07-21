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

#include "io/format.h"

#include "io/reader.h"
#include "io/reader_factory.h"
#include "io/reader_horiz.h"
#include "io/reader_multi_horiz.h"

#include "io/writer.h"
#include "io/writer_factory.h"
#include "io/writer_stream.h"
#include "io/writer_format.h"
#include "io/writer_horiz.h"
#include "io/writer_multi_horiz.h"
#include "io/writer_vert.h"
#include "io/writer_multi_vert.h"

using namespace std;

#include "boost.h"

namespace pyIO {
  using namespace NLP::IO;

  struct ReaderWrap: Reader, wrapper<Reader> {
    ReaderWrap(const std::string &uri, const std::string &name): Reader(uri, name){}
    virtual ~ReaderWrap(void){ /* do nothing */ }

    virtual void reset(void){
      get_override("reset")();
    }

    virtual bool next(NLP::Sentence &sent, bool add = false, bool expect = false){
      return get_override("next")(sent, add, expect);
    }
  };

}

BOOST_PYTHON_MODULE_INIT(io){
  using namespace NLP::IO;

  pyNLP::register_exception_translators();

  class_<Format>("Format", init<const std::string &>());

  class_<pyIO::ReaderWrap, boost::noncopyable>("Reader", init<const std::string &, const std::string &>())
    .def("reset", pure_virtual(&Reader::reset))
    .def("next", pure_virtual(&Reader::next));

  class_<ReaderFactory, bases<Reader>, boost::noncopyable>("ReaderFactory", init<const std::string &, const Format &>()[wcw13()]);

  scope().attr("STDIN") = STDIN;
  scope().attr("STDOUT") = STDOUT;
  scope().attr("STDERR") = STDERR;

  class_<Input, boost::noncopyable>("Input", init<const std::string &>())
    ;
  class_<Output, boost::noncopyable>("Output", init<const std::string &>())
    ;
  class_<Log, boost::noncopyable>("Log", init<const std::string &>())
    ;
}
