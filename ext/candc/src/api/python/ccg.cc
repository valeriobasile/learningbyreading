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

#include "candc.h"

#include "parser/variable.h"
#include "parser/dependency.h"
#include "parser/distance.h"
#include "parser/filled.h"
#include "parser/supercat.h"
#include "parser/unify.h"
#include "parser/rule.h"
#include "parser/cell.h"
#include "parser/equiv.h"
#include "parser/treebank.h"
#include "parser/chart.h"
#include "parser/rule_instances.h"
#include "tree/attributes.h"
#include "parser/depscore.h"
#include "parser/feature_type.h"
#include "parser/feature_dist_type.h"
#include "parser/feature_cat.h"
#include "parser/feature_rule.h"
#include "parser/feature_rule_head.h"
#include "parser/feature_rule_dep.h"
#include "parser/feature_rule_dep_dist.h"
#include "parser/feature_dep.h"
#include "parser/feature_dep_dist.h"
#include "parser/feature_genrule.h"
#include "parser/inside_outside.h"

using namespace std;

#include "boost.h"

#include "print_python.h"

namespace pyCCG {
  namespace py = boost::python;
  using namespace py;
  using namespace NLP;

  static void raise_IndexError(std::string name){
    name += " index out of range";

    PyErr_SetString(PyExc_IndexError, name.c_str());
    throw_error_already_set();
  }

  template <class T>
  struct ListIterator {
  private:
    const T *current;
  public:
    ListIterator(const T *current): current(current) {}
    ListIterator(const ListIterator<T> &other): current(other.current) {}
    ~ListIterator(void){}

    ListIterator iterator(void){ return *this; }
    const T *next(void){
      const T *res = current;
      if(res){
        current = res->next;
        return res;
      }else{
        PyErr_SetNone(PyExc_StopIteration);
        throw_error_already_set();
      }
      return 0;
    }
  };

  std::string str_atom(const CCG::Atom &atom){
    std::ostringstream out;
    out << "<Atom " << atom << ">";
    return out.str();
  }

  std::string str_feature(const CCG::Feature &feature){
    std::ostringstream out;
    out << "<Feature " << feature << ">";
    return out.str();
  }

  std::string str_varid(const CCG::VarID &varid){
    std::ostringstream out;
    out << "<VarID " << varid << ">";
    return out.str();
  }

  struct Cat {
    static std::string str(const CCG::Cat &cat){
      std::ostringstream out;
      out << cat;
      return out.str();
    }

    static std::string repr(const CCG::Cat &cat){
      std::ostringstream out;
      out << "<Cat " << cat << ">";
      return out.str();
    }

    static std::string str_novar(const CCG::Cat &cat){
      std::ostringstream out;
      cat.out_novar(out, false);
      return out.str();
    }

    static std::string str_novar_noX(const CCG::Cat &cat){
      std::ostringstream out;
      cat.out_novar_noX(out, false);
      return out.str();
    }

    static std::string str_short(const CCG::Cat &cat){
      std::ostringstream out;
      cat.out_short(out, false);
      return out.str();
    }

    static std::string str_js(const CCG::Cat &cat){
      std::ostringstream out;
      cat.out_js(out);
      return out.str();
    }

    static std::string flags(const CCG::Cat &cat){
      std::string res = "{";

      if(cat.flags & NLP::CCG::Cat::SLASH)
        res += " flags";
      if(cat.flags & NLP::CCG::Cat::RES_S)
        res += " res_s";
      if(cat.flags & NLP::CCG::Cat::ARG_S)
        res += " arg_s";
      if(cat.flags & NLP::CCG::Cat::RES_NP)
        res += " res_np";
      if(cat.flags & NLP::CCG::Cat::ARG_NP)
        res += " arg_np";
      if(cat.flags & NLP::CCG::Cat::RES_N)
        res += " res_n";
      if(cat.flags & NLP::CCG::Cat::ARG_N)
        res += " arg_n";
      if(cat.flags & NLP::CCG::Cat::RES_SbNP)
        res += " res_sbnp";

      res += " }";
      return res;
    }

    static ulong rhash(const NLP::CCG::Cat &cat){ return cat.rhash.value(); }
    static ulong uhash(const NLP::CCG::Cat &cat){ return cat.uhash.value(); }
  };

  std::string str_relation(const CCG::Relation &rel){
    std::ostringstream out;
    out << "<Relation ";
    rel.print_slot(out, false);
    out << ">";
    return out.str();
  }

  struct Markedup {
    static const CCG::Cat &cat(const CCG::Markedup &markedup, const std::string &s){
      const CCG::Cat *cat = markedup.cat(s);
      if(!cat)
        throw NLP::Exception("no markedup category for " + s + " exists");
      return *cat;
    }

    static void add(CCG::Markedup &markedup, const std::string &plain,
                    const std::string &mark, const CCG::Cat *cat){
      markedup.add(plain, mark, cat);
    }
  };

  struct Relations {
    static CCG::RelID get(const CCG::Relations &rels, const std::string &s, ulong slot){
      return rels.get(s, slot);
    }
  };

  struct Categories {
    static CCG::Cat &parse(CCG::Categories &cats, const char *s){
      return *cats.parse(s);
    }
  };

  struct Var {
    std::string str(const CCG::Variable &var){
      std::ostringstream out;
      out << "<Variable " << var << ">";
      return out.str();
    }

    static size_t id(const CCG::SuperCat &sc){
      return reinterpret_cast<size_t>(&sc);
    }

    static const CCG::Position *begin_fillers(const CCG::Variable &v){ return v.fillers; }
    static const CCG::Position *end_fillers(const CCG::Variable &v){
      return v.fillers + v.count_fillers();
    }
    static CCG::Position filler(const CCG::Variable &v, ulong index){
      if(index < CCG::Variable::NFILLERS)
        return v.fillers[index];
      else
        raise_IndexError("Position");
      return 0;
    }
  };

  struct Dep {
    std::string str(const CCG::Dependency &dep){
      std::ostringstream out;
      out << "<Dependency " << dep << ">";
      return out.str();
    }

    static const CCG::Dependency *next(const CCG::Dependency &dep){
      if(dep.next)
        return dep.next;
      return 0;
    }
  };

  struct SC {
    static std::string str(const CCG::SuperCat &sc){
      std::ostringstream out;
      out << "<SuperCat " << sc << ">";
      return out.str();
    }

    static size_t id(const CCG::SuperCat &sc){
      return reinterpret_cast<size_t>(&sc);
    }

    static const CCG::Variable *begin_vars(const CCG::SuperCat &sc){ return sc.vars; };
    static const CCG::Variable *end_vars(const CCG::SuperCat &sc){ return sc.vars + sc.nvars; };
    static const CCG::Variable *var(const CCG::SuperCat &sc, CCG::VarID varid){
      if(varid < sc.nvars)
        return &sc.vars[varid];
      else
        raise_IndexError("Cell");
      return 0;
    }

    static ListIterator<CCG::SuperCat> equiv(const CCG::SuperCat &sc){
      return &sc;
    }

    static const CCG::SuperCat *left(const CCG::SuperCat &sc){
      if(sc.left)
        return sc.left;
      return 0;
    }

    static const CCG::SuperCat *right(const CCG::SuperCat &sc){
      if(sc.right)
        return sc.right;
      return 0;
    }

    static const CCG::SuperCat *next(const CCG::SuperCat &sc){
      if(sc.next)
        return sc.next;
      return 0;
    }

    static const CCG::SuperCat *max(const CCG::SuperCat &sc){
      if(sc.max)
        return sc.max;
      return 0;
    }

    static ulong getwordnum(const CCG::SuperCat &sc) {
      return (ulong)(sc.vars[sc.cat->var]).pos();
    }

    static ListIterator<CCG::Dependency> unfilled(const CCG::SuperCat &sc){
      return sc.unfilled;
    }

    static ulong ehash(const NLP::CCG::SuperCat &sc){ return sc.ehash().value(); }
  };

  struct Cell {
    static const CCG::SuperCat *getitem(const CCG::Cell &cell, const ulong i){
      if(i < cell.size()){
        if (!cell[i])
          throw NLP::Exception("undefined chart cell");

        return cell[i];
      }
      else
        raise_IndexError("Cell");
      return 0;
    };
  };

  struct TBSentence {
    static const CCG::TBNode get(const CCG::TBSentence &sentence, const ulong i){
      if(i < sentence.size())
        return sentence[i];
      else
        raise_IndexError("Cell");

      return CCG::TBNode();
    };
  };

  struct Chart {
    static const CCG::Cell *cell(const CCG::Chart &chart, CCG::Position pos, CCG::Position span){
      if(pos < chart.nwords && static_cast<ulong>(pos + span) <= chart.nwords)
        return &chart(pos, span);
      else
        raise_IndexError("Chart");
      return 0;
    }

    static void load(CCG::Chart &chart, NLP::Sentence &sent, double BETA){
      chart.load(sent, BETA, false, true, false);
    }
  };

  struct DecoderWrap: NLP::CCG::Decoder, wrapper<NLP::CCG::Decoder> {
    DecoderWrap(void){}
    virtual ~DecoderWrap(void){ /* do nothing */ }

    virtual double best_score(const NLP::CCG::SuperCat *sc) {
      return get_override("best_score")(sc);
    }
  };

  py::tuple calc_stats(NLP::CCG::Parser &parser){
    NLP::CCG::Statistics stats;
    parser.calc_stats(stats);
    return py::make_tuple(stats.logderivs, stats.nequiv, stats.ntotal);
  }
}

BOOST_PYTHON_MODULE_INIT(ccg){
  using namespace NLP;

  pyNLP::register_exception_translators();

  class_<CCG::Atom>("Atom", init<>())
    .def(init<uchar>())
    .def(init<const char *>())
    .def(int_(self))
    .def("__repr__", &pyCCG::str_atom);

  class_<CCG::Feature>("Feature", init<>())
    .def(init<uchar>())
    .def(init<const char *>())
    .def(int_(self))
    .def("__repr__", &pyCCG::str_feature);

  class_<CCG::VarID>("VarID", init<>())
    .def(init<uchar>())
    .def(init<const char *>())
    .def(int_(self))
    .def("__repr__", &pyCCG::str_varid);

  class_<CCG::Cat, boost::noncopyable>("Cat", no_init)
    .def("__str__", &pyCCG::Cat::str)
    .def("__repr__", &pyCCG::Cat::repr)
    .def("str", &pyCCG::Cat::str)
    .def("str_novar", &pyCCG::Cat::str_novar)
    .def("str_novar_noX", &pyCCG::Cat::str_novar_noX)
    .def("str_short", &pyCCG::Cat::str_short)
    .def("str_js", &pyCCG::Cat::str_js)
    .def_readonly("atom", &CCG::Cat::atom)
    .def_readonly("feature", &CCG::Cat::feature)
    .def_readonly("var", &CCG::Cat::var)
    .def_readonly("rel", &CCG::Cat::rel)
    .def_readonly("lrange", &CCG::Cat::lrange)
    .add_property("arg", make_getter(&CCG::Cat::arg, ret_ir1()))
    .add_property("result", make_getter(&CCG::Cat::res, ret_ir1()))
    .add_property("res", make_getter(&CCG::Cat::res, ret_ir1()))
    .add_property("flags", &pyCCG::Cat::flags)
    .add_property("rhash", &pyCCG::Cat::rhash)
    .add_property("uhash", &pyCCG::Cat::uhash);

  class_<CCG::Markedup>("Markedup", init<const std::string &>())
    .def("__len__", &CCG::Markedup::size)
    .def("__getitem__", &pyCCG::Markedup::cat, ret_ir1())
    .def("__setitem__", &pyCCG::Markedup::add, wcw13());

  class_<CCG::Relation>("Relation", init<const char *, ulong, ulong>())
    .def(init<const CCG::Relation &>())
    .add_property("cat", make_getter(&CCG::Relation::cat, ret_ir1()))
    .def_readonly("cat_str", &CCG::Relation::cat_str)
    .def_readonly("slot", &CCG::Relation::slot)
    .def("__repr__", &pyCCG::str_relation);

  class_<CCG::Relations>("Relations", init<const std::string &>())
    .def("__len__", &CCG::Relations::size)
    .def("__call__", &pyCCG::Relations::get)
    .def("__getitem__", &CCG::Relations::rel_checked, ret_ccr());

  class_<CCG::Categories>("Categories", init<const std::string &, const std::string &, bool>())
    .def("parse", &pyCCG::Categories::parse, ret_ir1())
    .def_readonly("relations", &CCG::Categories::relations)
    .def_readonly("markedup", &CCG::Categories::markedup);

  class_<CCG::Variable>("Variable", init<>())
    .def("is_unfilled", &CCG::Variable::is_unfilled)
    .def("is_filled", &CCG::Variable::is_filled)
    .def("is_lexical", &CCG::Variable::is_lexical)
    .def("is_set", &CCG::Variable::is_set)
    .add_property("pos", &CCG::Variable::pos)
    .def("__getitem__", &pyCCG::Var::filler)
    .add_property("fillers", range(&pyCCG::Var::begin_fillers, &pyCCG::Var::end_fillers))
    .def("__repr__", &pyCCG::Var::str);

  typedef pyCCG::ListIterator<CCG::Dependency> DependencyIterator;
  class_<DependencyIterator>("DependencyIterator", init<const CCG::Dependency *>()[wcw12()])
    .def(init<const DependencyIterator &>())
    .def("__iter__", &DependencyIterator::iterator)
    .def("next", &DependencyIterator::next, ret_ir1());

  class_<CCG::Dependency>("Dependency", init<CCG::Position, CCG::RelID, CCG::VarID, CCG::RuleID, CCG::Dependency *>()[wcw12()])
    .def_readonly("head", &CCG::Dependency::head)
    .def_readonly("rel", &CCG::Dependency::rel)
    .def_readonly("var", &CCG::Dependency::var)
    .def_readonly("lrange", &CCG::Dependency::lrange)
    .add_property("next", make_function(&pyCCG::Dep::next, ret_ir1()))
    .def("__repr__", &pyCCG::Dep::str);

  typedef pyCCG::ListIterator<CCG::SuperCat> SuperCatIterator;
  class_<SuperCatIterator>("SuperCatIterator", init<const CCG::SuperCat *>()[wcw12()])
    .def(init<const SuperCatIterator &>())
    .def("__iter__", &SuperCatIterator::iterator)
    .def("next", &SuperCatIterator::next, ret_ir1());

  class_<CCG::SuperCat, boost::noncopyable>("SuperCat", no_init)
    .add_property("id", &pyCCG::SC::id)
    .add_property("cat", make_getter(&CCG::SuperCat::cat, ret_ir1()))
    .def_readonly("nvars", &CCG::SuperCat::nvars)
    .def_readonly("nactive", &CCG::SuperCat::nactive)
    .add_property("vars", range<ret_ir1>(&pyCCG::SC::begin_vars, &pyCCG::SC::end_vars))
    .def("__getitem__", &pyCCG::SC::var, ret_ir1())
    .def("conj", &CCG::SuperCat::conj)
    .def("tr", &CCG::SuperCat::tr)
    .def("lex", &CCG::SuperCat::lex)
    .def("conj_or_tr", &CCG::SuperCat::conj_or_tr)
    .def("conj_and_tr", &CCG::SuperCat::conj_and_tr)
    .def("getwordnum", &pyCCG::SC::getwordnum)
    .add_property("equiv", &pyCCG::SC::equiv)
    .add_property("l", make_function(&pyCCG::SC::left, ret_ir1()))
    .add_property("r", make_function(&pyCCG::SC::right, ret_ir1()))
    .add_property("next", make_function(&pyCCG::SC::next, ret_ir1()))
    .add_property("max", make_function(&pyCCG::SC::max, ret_ir1()))
    .add_property("unfilled", &pyCCG::SC::unfilled)
    .def_readonly("score", &CCG::SuperCat::score)
    .def_readonly("marker", &CCG::SuperCat::marker)
    .def_readonly("inside", &CCG::SuperCat::inside)
    .def_readonly("d_inside", &CCG::SuperCat::d_inside)
    .def_readonly("outside", &CCG::SuperCat::outside)
		.def_readonly("pos", &CCG::SuperCat::pos)
		.def_readonly("span", &CCG::SuperCat::span)
		.def_readonly("depth", &CCG::SuperCat::depth)
    .add_property("ehash", &pyCCG::SC::ehash)
    .add_property("comb", &CCG::SuperCat::flags2str)
    .def("__repr__", &pyCCG::SC::str);

  class_<CCG::Cell, boost::noncopyable>("Cell", no_init)
    .def("__len__", &CCG::Cell::size)
    .def("__getitem__", &pyCCG::Cell::getitem, ret_ir1());

  /*
  class_<CCG::TBNode>("TBNode", init<const std::string &>())
    .def_readonly("cat", &CCG::TBNode::cat)
    .def_readonly("pos", &CCG::TBNode::pos)
    .def_readonly("word", &CCG::TBNode::word)
    .def_readonly("type", &CCG::TBNode::type)
    .def_readonly("nchildren", &CCG::TBNode::nchildren);

  class_<CCG::TBSentence>("TBSentence", init<>())
    .def(init<const CCG::TBSentence &>())
    .def("__getitem__", &pyCCG::TBSentence::get)
    .def("__len__", &CCG::TBSentence::size)
    .def("add", &CCG::TBSentence::push_back);
  */

  class_<CCG::Chart, boost::noncopyable>("Chart", init<CCG::Categories &, bool, ulong>())
    .def_readonly("size", &CCG::Chart::nwords)
    .def("__len__", make_getter(&CCG::Chart::nwords))
    .def("__call__", &pyCCG::Chart::cell, ret_ir1())
    .def("lex", &CCG::Chart::lex)
    .def("tr", &CCG::Chart::tr)
    .def("load", &pyCCG::Chart::load)
    .def("load", (bool (CCG::Chart::*)(const CCG::TBSentence &))&CCG::Chart::load)
    .def("reset", &CCG::Chart::reset);


  class_<pyCCG::DecoderWrap, boost::noncopyable>("Decoder", no_init);

  class_<CCG::DecoderFactory, bases<CCG::Decoder> >("DecoderFactory",
                                                    init<const std::string &>());

  class_<CCG::Statistics>("Statistics", init<>())
    .def_readonly("logderivs", &CCG::Statistics::logderivs)
    .def_readonly("nequiv", &CCG::Statistics::nequiv)
    .def_readonly("ntotal", &CCG::Statistics::ntotal);

  class_<CCG::Printer>("Printer", init<CCG::Categories &>())
    .def_readonly("nsentences", &CCG::Printer::nsentences)
    .def_readonly("nparsed", &CCG::Printer::nparsed)
    .def_readonly("success", &CCG::Printer::success)
    .def_readonly("reason", &CCG::Printer::reason)
    .def_readonly("beta", &CCG::Printer::beta)
    .def_readonly("dict_cutoff", &CCG::Printer::dict_cutoff);

  class_<CCG::StreamPrinter, bases<CCG::Printer>, boost::noncopyable>("StreamPrinter", no_init);

  class_<CCG::PrinterFactory, bases<CCG::StreamPrinter>, boost::noncopyable>("PrinterFactory",
    init<const std::string &, IO::Output &, IO::Log &,
    CCG::Categories &, CCG::StreamPrinter::Format>()[wcw15()]);

  class_<CCG::PythonPrinter, bases<CCG::Printer> >("PythonPrinter", init<CCG::Categories &>())
    .def_readonly("deriv", &CCG::PythonPrinter::deriv)
    .def_readonly("deps", &CCG::PythonPrinter::deps)
    .def_readonly("grs", &CCG::PythonPrinter::grs);

  class_<CCG::Parser::Config, bases<Config::Directory> >("ParserConfig", init<>())
    .def(init<const Config::OpPath *>()[wcw12()])
    .def(init<const Config::OpPath *, const std::string &>()[wcw12()])
    .def(init<const Config::OpPath *, const std::string &, const std::string &>()[wcw12()])
    .def_readonly("cats", &CCG::Parser::Config::cats)
    .def_readonly("markedup", &CCG::Parser::Config::markedup)
    .def_readonly("weights", &CCG::Parser::Config::weights)
    .def_readonly("rules", &CCG::Parser::Config::rules)
    .def_readonly("maxwords", &CCG::Parser::Config::maxwords)
    .def_readonly("maxsupercats", &CCG::Parser::Config::maxsupercats)
    .def_readonly("alt_markedup", &CCG::Parser::Config::alt_markedup)
    .def_readonly("seen_rules", &CCG::Parser::Config::seen_rules)
    .def_readonly("extra_rules", &CCG::Parser::Config::extra_rules)
    .def_readonly("question_rules", &CCG::Parser::Config::question_rules)
    .def_readonly("partial_gold", &CCG::Parser::Config::partial_gold)
    .def_readonly("beam", &CCG::Parser::Config::beam);

  class_<CCG::Parser>("Parser", init<const CCG::Parser::Config &, NLP::Sentence &, CCG::Categories &>()[wcw12_13_14()])
    .def("parse", &CCG::Parser::parse)
    .def("best", &CCG::Parser::best, ret_ir1())
    .def("calc_stats", &pyCCG::calc_stats)
    .add_property("sentence", make_function(&CCG::Parser::sentence, ret_ir1()))
    .add_property("chart", make_function(&CCG::Parser::chart, ret_ir1()));

  class_<CCG::Integration::Config, bases<NLP::Config::Cfg> >("IntegrationConfig", init<>())
    .def(init<const std::string &>())
    .def(init<const std::string &, const std::string &>())
    .def_readonly("start", &CCG::Integration::Config::start)
    .def_readonly("betas", &CCG::Integration::Config::betas)
    .def_readonly("dict_cutoffs", &CCG::Integration::Config::dict_cutoffs);

  class_<CCG::Integration, boost::noncopyable>("Integration", init<CCG::Integration::Config &, Taggers::Super::Config &,
                                               CCG::Parser::Config &, NLP::Sentence &>()[wcw12_13_14_15()])
    .add_property("super", make_getter(&CCG::Integration::super))
    .add_property("cats", make_getter(&CCG::Integration::cats))
    .add_property("parser", make_getter(&CCG::Integration::parser))
    .def("parse", &CCG::Integration::parse);

  class_<CandC::Config, bases<NLP::Config::Directory> >("CandCConfig", init<>())
    .def(init<const Config::OpPath *>()[wcw12()])
    .def(init<const Config::OpPath *, const std::string &>()[wcw12()])
    .def(init<const Config::OpPath *, const std::string &, const std::string &>()[wcw12()])
    .def_readonly("pos", &CandC::Config::pos);

  def("equiv", (bool (*)(const CCG::SuperCat *, const CCG::SuperCat *))&CCG::equivalent);
  def("explain", &CCG::equivalent_explain);
}
