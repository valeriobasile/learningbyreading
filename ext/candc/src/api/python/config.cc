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

using namespace std;

#include "boost.h"

namespace pyNLP {

  using namespace NLP::Config;
  
  struct NodeWrap: Node, wrapper<Node> {

    NodeWrap(const std::string &name, const std::string &desc, Flags flags)
      : Node(name, desc, flags){}

    void reg(Node &child, Flags flags){
      get_override("reg")(child, flags);
    }

    bool has(const std::string &name) const {
      return get_override("has")(name);
    }

    Node &get(const std::string &){
      return *(Node *)0;
      // FIXME: not sure why this is commented out atm
      //      return extract<Node &>(get_override("get")(name));
    }

    void set(const std::string &val){
      get_override("set")(val);
    }

    void check(void){
      get_override("check")();
    }

    bool has_children(void) const {
      return get_override("has_children")();
    }

    bool needs_arg(void) const{
      return get_override("needs_arg")();
    }

    void write_help(std::ostream &, std::string, bool) const {}
    void write_config(std::ostream &, std::string, bool) const {}
    void write_preface(std::ostream &, std::string, bool) const {}
  };

  struct OptionWrap: Option, wrapper<Option> {
    OptionWrap(const std::string &name, const std::string &desc, Flags flags)
      : Option(name, desc, flags){}
    OptionWrap(Cfg &cfg, const std::string &name, const std::string &desc, Flags flags)
      : Option(cfg, name, desc, flags){}

    bool is_valid(void) const {
      return get_override("is_valid")();
    }

    void set(const std::string &val){
      get_override("set")(val);
    }

    bool needs_arg(void) const{
      return get_override("needs_arg")();
    }
  };

  std::string str_node(const Node &node){
    std::ostringstream out;
    node.write_config(out, "", false);
    return out.str();
  }

  template <typename T>
  struct OpT: class_<NLP::Config::Op<T>, bases<NLP::Config::Option> > {
    typedef class_<NLP::Config::Op<T>, bases<NLP::Config::Option> > OpBase;

    OpT(const char *name)
      : OpBase(name, init<const std::string &, const std::string &>()){
      this->def(init<const std::string &, const std::string &, T>())
	.def(init<Cfg &, const std::string &, const std::string &>()[ret_ir1()])
	.def(init<Cfg &, Flags, const std::string &, const std::string &>()[ret_ir1()])
	.def(init<Cfg &, const std::string &, const std::string &, T>()[ret_ir1()])
	.def(init<Cfg &, Flags, const std::string &, const std::string &, T>()[ret_ir1()])
	.add_property("value", &Op<T>::get_value, &Op<T>::set_value)
	.add_property("default", &Op<T>::get_default, &Op<T>::set_default);
    }
  };

  template <typename T>
  void list2vec(std::vector<T> &vec, const py::object &pyseq){
    int nelems = len(pyseq);

    vec.clear();
    vec.reserve(nelems);

    for(int i = 0; i < nelems; ++i)
      vec.push_back(extract<T>(pyseq[i]));
  }

  template <typename T>
  struct OpV: class_<NLP::Config::Op<std::vector<T> >, bases<NLP::Config::Option> > {
    typedef class_<NLP::Config::Op<std::vector<T> >, bases<NLP::Config::Option> > OpBase;

    static void set_value_vec(NLP::Config::Op<std::vector<T> > &op, const object &pyseq){
      std::vector<T> tmp;
      list2vec(tmp, pyseq);
      op.set_value(tmp);
    }

    static void set_default_vec(NLP::Config::Op<std::vector<T> > &op, const object &pyseq){
      std::vector<T> tmp;
      list2vec(tmp, pyseq);
      op.set_default(tmp);
    }

    OpV(const char *name)
      : OpBase(name, init<const std::string &, const std::string &>()){
      this->def(init<const std::string &, const std::string &, std::vector<T> >())
	.def(init<Cfg &, const std::string &, const std::string &>()[ret_ir1()])
	.def(init<Cfg &, const std::string &, const std::string &, std::vector<T> >()[ret_ir1()])
	.add_property("value", &Op<std::vector<T> >::get_value, &set_value_vec)
	.add_property("default", &Op<std::vector<T> >::get_default, &set_default_vec);
    }
  };

  void main_parse(NLP::Config::Main &main, const boost::python::list &pyobj){
    int argc = len(pyobj);
    char **argv = new char *[argc + 1];
    for(int i = 0; i < argc; ++i)
      argv[i] = extract<char *>(pyobj[i]);
    argv[argc] = 0;
    main.parse(argc, argv);
  }
}

BOOST_PYTHON_MODULE_INIT(config){
  using namespace NLP::IO;
  using namespace NLP::Config;

  pyNLP::register_exception_translators();

  scope().attr("SPACE") = SPACE;

  class_<pyNLP::NodeWrap, boost::noncopyable>("Node", init<const std::string &, const std::string &, uchar>())
    .def_readonly("NAME", &Node::NAME)
    .def_readonly("DESC", &Node::DESC)
    .def_readwrite("flags", &Node::flags)
    .def("reg", pure_virtual(&Node::reg), wcw12())
    .def("has", pure_virtual(&Node::has))
    .def("get", pure_virtual(&Node::get), ret_ir1())
    .def("set", pure_virtual(&Node::set), wcw12())
    .def("check", pure_virtual(&Node::check))
    .def("needs_arg", pure_virtual(&Node::needs_arg))
    .def("__str__", &pyNLP::str_node);

  class_<Cfg, bases<Node> >("Cfg", init<const std::string &, const std::string &, uchar>())
    .def(init<Cfg &, const std::string &, const std::string &, uchar>()[ret_ir1()]);

  class_<pyNLP::OptionWrap, bases<Node>, boost::noncopyable>("Option", init<const std::string &, const std::string &, uchar>())
    .def(init<Cfg &, const std::string &, const std::string &, uchar>()[ret_ir1()])
    .def("is_valid", pure_virtual(&Option::is_valid))
    .def("has_default", &Option::has_default)
    .def("has_changed", &Option::has_changed)
    .def("is_defined", &Option::is_defined)
    .def("needs_arg", pure_virtual(&Node::needs_arg))
    .def("set", pure_virtual(&Node::set), wcw12());

  pyNLP::OpT<bool>("OpBool");
  pyNLP::OpT<long>("OpLong");
  pyNLP::OpT<ulong>("OpULong");
  pyNLP::OpT<double>("OpDouble");
  pyNLP::OpT<float>("OpFloat");
  pyNLP::OpT<std::string>("OpString");
  pyNLP::OpT<Format>("OpFormat");

  pyNLP::OpV<double>("OpVecDouble");
  pyNLP::OpV<ulong>("OpVecULong");

  class_<std::vector<double> >("VecDouble")
    .def(vector_indexing_suite<std::vector<double> >());

  class_<std::vector<ulong> >("VecULong")
    .def(vector_indexing_suite<std::vector<ulong> >());

  class_<OpPath, bases<NLP::Config::Op<std::string> > >("OpPath", init<const std::string &, const std::string &>())
    .def(init<const std::string &, const std::string &, std::string>())
    .def(init<Cfg &, const std::string &, const std::string &>())
    .def(init<Cfg &, Flags, const std::string &, const std::string &>())
    .def(init<Cfg &, const std::string &, const std::string &, std::string>())
    .def(init<Cfg &, Flags, const std::string &, const std::string &, std::string>())
    .add_property("value", &OpPath::get_value, &OpPath::set_value);

  class_<Alias, bases<Node> >("Alias", init<Node &, const std::string &, const std::string &>())
    .def(init<Cfg &, Node &, const std::string &, const std::string &>())
    .def(init<Cfg &, Flags, Node &, const std::string &, const std::string &>());

  class_<Directory, bases<Cfg> >("Directory", init<const std::string &, const std::string &>())
    .def(init<Cfg &, const std::string &, const std::string &>())
    .def_readonly("path", &Directory::path);

  class_<Main, bases<Cfg> >("Main", init<const std::string &>())
    .def("set", (void(Main::*)(const std::string &, const std::string &)) &Main::set)
    .def("parse", &pyNLP::main_parse);
}
