// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.


#include <string>
#include <iostream>
#include <sstream>
#include <exception>
#include <vector>

using namespace std;

#include "utils.h"
#include "exception.h"
#include "config/config.h"

using namespace NLP::Config;

void
test_option_reject(const char *test, const std::string &prefix, const std::string &name,
		   char short_flag, const std::string &desc, const std::string &msg){
  try {
    Option<char> op(prefix, name, short_flag, desc);
    cout << test << " should have thrown an exception" << endl;
  }catch(NLP::ConfigError e){
    if(e.what() != msg){
      cout << test << " failed: thrown " << e.what() << endl;
      cout << test << " correct:       " << msg << endl;
    }
    return;
  }
}

template <class Op, class Value>
void
test_accept(const char *test, Op &op, const char *s, const Value &v){
  try {
    op.set(s);
    if(op.value != v)
      cout << test << " failed: value '" << op.value << "' != '" << v << '\'' << endl;
  }catch(NLP::ConfigError e){
    cout << test << " failed: thrown " << e.what() << endl;
    return;
  }
}

template <class Op>
void
test_reject(const char *test, Op &op, const char *s, const std::string &msg){
  try {
    op.set(s);
    cout << test << " failed: value '" << op.value << "'" << endl;
  }catch(NLP::ConfigError e){
    if(e.what() != msg){
      cout << test << " failed: thrown " << e.what() << endl;
      cout << test << " correct:       " << msg << endl;
    }
    return;
  }
}

template <class Op>
void
test_reg_reject(const char *test, Config &config, Op &op, const std::string &msg){
  try {
    config.reg(op);
    cout << test << " should have thrown an exception" << endl;
  }catch(NLP::ConfigError e){
    if(e.what() != msg){
      cout << test << " failed: thrown " << e.what() << endl;
      cout << test << " correct:       " << msg << endl;
    }
    return;
  }
}

void
test_parse_reject(const char *test, Config &config, int argc, char **argv,
		  const std::string &name, const std::string &msg){
  try {
    config.parse(argc, argv);
    cout << test << " should have thrown exception" << endl;
    cout << test << " exception: " << msg << endl;
  }catch(NLP::ConfigError e){
    if(e.option() != name){
      cout << test << " failed: option name " << e.option() << endl;
      cout << test << " correct:       " << name << endl;
    }
    if(e.what() != msg){
      cout << test << " failed: thrown " << e.what() << endl;
      cout << test << " correct:       " << msg << endl;
    }
    return;
  }
}

void
test_usage(const char *test, Config &config, const std::string &msg){
  ostringstream out;
  config.usage(out);
  if(msg != out.str())
    cout << test << " failed: usage does not match expected output" << endl;
}

void
test_save(const char *test, Config &config, const std::string &data){
  ostringstream out;
  config.save(out);
  if(data != out.str())
    cout << test << " failed: saved state does not match expected output" << endl;
}

void
test_load(const char *test, Config &config, bool should_throw,
	  const std::string &name, int line, const std::string &msg,
	  const std::string &data){

  std::string uri = "blah.config";

  try {
    istringstream in(data);

    config.load(in, uri);
        if(should_throw){
      cout << test << " should have thrown exception" << endl;
      cout << test << " exception: " << msg << endl;
    }
  }catch(NLP::ConfigError e){
    if(e.option() != name){
      cout << test << " failed: option name " << e.option() << endl;
      cout << test << " correct:       " << name << endl;
    }
    if(e.what() != msg){
      cout << test << " failed: thrown " << e.what() << endl;
      cout << test << " correct:       " << msg << endl;
    }
    if(e.where() != uri){
      cout << test << " failed: uri " << e.where() << endl;
      cout << test << " correct:       " << uri << endl;
    }
    if(e.line() != line){
      cout << test << " failed: line number " << e.line() << endl;
      cout << test << " correct:       " << line << endl;
    }
    return;
  }
}

void
test_check(const char *test, Config &config, bool should_throw,
	   const std::string &name, const std::string &msg){
  try {
    config.check();
    if(should_throw){
      cout << test << " should have thrown exception" << endl;
      cout << test << " exception: " << msg << endl;
    }
  }catch(NLP::ConfigError e){
    if(e.option() != name){
      cout << test << " failed: option name " << e.option() << endl;
      cout << test << " correct:       " << name << endl;
    }
    if(e.what() != msg){
      cout << test << " failed: thrown " << e.what() << endl;
      cout << test << " correct:       " << msg << endl;
    }
    return;
  }
}


int
main(int argc, char **argv){
  test_option_reject("op1", "", "blah", 'n', "this is an option",
		     "option prefix must not be empty string (name 'blah')");
  test_option_reject("op2", "blah", "", 'n', "this is an option",
		     "option name must not be an empty string (prefix 'blah')");
  test_option_reject("op3", "", "", 'n', "this is an option",
		     "option prefix must not be empty string (name '')");
  test_option_reject("op4", "blah-blah", "x", 'n', "this is an option",
		     "option prefix must not contain a hyphen (prefix 'blah-blah')");
  test_option_reject("op5", "blah", "x-x", 'n', "this is an option",
		     "option name must not contain a hyphen (name 'x-x')");
  test_option_reject("op6", "blah", "xx", '!', "this is an option",
		     "option short name must be an alphanumeric character");

  Option<bool> op("prefix", "name", 'n', "this is a boolean option");
  test_accept("bool1", op, "true", true);
  test_accept("bool2", op, "false", false);
  test_reject("bool3", op, " true", "option argument is invalid (argument ' true')");
  test_reject("bool4", op, "false ", "option argument is invalid (argument 'false ')");
  test_accept("bool6", op, "1", true);
  test_accept("bool5", op, "0", false);
  test_accept("bool7", op, "yes", true);
  test_accept("bool8", op, "no", false);
  test_accept("bool9", op, "True", true);
  test_accept("bool10", op, "False", false);
  test_accept("bool11", op, "TRUE", true);
  test_accept("bool12", op, "FALSE", false);
  test_reject("bool13", op, "", "option argument is invalid (argument '')");

  Option<int> op2("prefix", "name", 'n', "this is an integer option");
  test_accept("int1", op2, "1234", 1234);
  test_accept("int2", op2, "-1", -1);
  test_accept("int3", op2, "0", 0);
  test_reject("int4", op2, "abc", "option argument is invalid (argument 'abc')");
  test_reject("int5", op2, "1234a", "option argument is invalid (argument '1234a')");
  test_reject("int6", op2, "99999999999999999",
	      "option argument is invalid (argument '99999999999999999')");
  test_reject("int7", op2, "-9999999999999999",
	      "option argument is invalid (argument '-9999999999999999')");
  test_reject("int8", op2, "", "option argument is invalid (argument '')");

  Option<uint> op3("prefix", "name", 'n', "this is an unsigned integer option");
  test_accept("uint1", op3, "1234", 1234u);
  test_accept("uint2", op3, "0", 0u);
  test_reject("uint3", op3, "-1",
	      "option argument must be a non-negative integer (argument '-1')");
  test_reject("uint4", op3, "99999999999999999",
	      "option argument is invalid (argument '99999999999999999')");
  test_reject("uint5", op3, "", "option argument is invalid (argument '')");

  Option<char> op4("prefix", "name", 'n', "this is a character option");
  test_accept("char1", op4, "c", 'c');
  test_accept("char2", op4, "\\n", '\n');
  test_accept("char3", op4, "\\t", '\t');
  test_accept("char4", op4, "\\\\", '\\');
  test_accept("char5", op4, "\\'", '\'');
  test_accept("char6", op4, "\\\"", '\"');
  test_accept("char7", op4, " ", ' ');
  test_accept("char10", op4, "\\s", ' ');
  test_reject("char8", op4, "xa",
	      "option argument must be a single character or escape sequence (argument 'xa')");
  test_reject("char9", op4, "",
	      "option argument must be a single character or escape sequence (argument '')");

  Option<std::string> op5("prefix", "name", 'n', "this is a string option");
  test_accept("str1", op5, "abcdefg", string("abcdefg"));
  test_accept("str2", op5, "", string(""));
  test_accept("str3", op5, "\\\"ab\\t\\nc\\'", string("\"ab\t\nc\'"));
  test_accept("str4", op5, "    ", string("    "));
  test_reject("str5", op5, "abc\\", "unexpected end of escape sequence in 'abc\\'");
  test_reject("str6", op5, "\\\\\\", "unexpected end of escape sequence in '\\\\\\'");

  Option<double> op6("prefix", "name", 'n', "this is a double precision option");
  test_accept("dbl1", op6, "0.5", 0.5);
  test_accept("dbl2", op6, "1.23445", 1.23445);
  test_accept("dbl3", op6, "1.234e56", 1.234e56);
  test_accept("dbl4", op6, "1e303", 1e303);
  test_reject("dbl5", op6, "1e3456", "option argument is invalid (argument '1e3456')");

  Option<std::vector<int> > op7("prefix", "name", 'n', "this is an integer list option");
  std::vector<int> ivec;
  test_accept("ivec1", op7, "", ivec);
  ivec.push_back(1);
  test_accept("ivec1a", op7, "1", ivec);
  ivec.push_back(2);
  ivec.push_back(3);
  test_accept("ivec2", op7, "1 2 3", ivec);
  test_accept("ivec3", op7, "1,2,3", ivec);
  test_reject("ivec4", op7, "1:2:3", "option argument is invalid (argument '1:2:3')");
  test_reject("ivec5", op7, "1,2 3", "option argument is invalid (argument '1,2 3')");
  test_reject("ivec6", op7, "1 2,3", "option argument is invalid (argument '1 2,3')");
  test_reject("ivec7", op7, "a b c", "option argument is invalid (argument 'a b c')");
  test_reject("ivec8", op7, "1 2 a", "option argument is invalid (argument '1 2 a')");
  test_reject("ivec9", op7, "1,,", "option argument is invalid (argument '1,,')");

  Option<std::vector<std::string> > op8("prefix", "name", 'n', "this is a string list");
  std::vector<std::string> svec;
  test_accept("svec1", op8, "", svec);
  svec.push_back("abc");
  test_accept("svec2", op8, "abc", svec);
  svec.push_back("xyz");
  test_accept("svec3", op8, "abc xyz", svec);
  // this one doesn't work because you don't know where to split on
  //  test_reject("svec4", op8, "abc,xyz", svec);

  // this one doesn't work because the string reader in istream
  // does not involve handling escape sequences (we do it ourselves
  // for Option<std::string>
  // one day it would be nice to add that here too but none of the
  // current options require anything that fancy

  // svec.push_back("\n\t\a");
  // test_accept("svec4", op8, "abc xyz \\n\\t\\a", svec);

  Option<std::string> op9("prefix", "name", 'n', "this is a string option");
  Option<std::string> op10("prefix", "name", 'n', "this is a string option too");
  Config config1("blah");
  config1.reg(op9);
  test_reg_reject("reg1", config1, op10,
		  "option already exists with the name and prefix 'prefix-name'");

  Option<std::string> op11("op11", "name", 'n', "this is a string option");
  Option<std::string> op12("op12", "name", 'n', "this is a string option too");
  Config config2("blah");
  config2.reg(op11);
  test_usage("usage1", config2,
	     "usage: blah [options]\n"
	     "options:\n"
	     "--help: display this usage message\n"
	     "--name <arg> (-n <arg>): this is a string option\n");

  config2.reg(op12);
  test_usage("usage2", config2,
	     "usage: blah [options]\n"
	     "options:\n"
	     "--help: display this usage message\n"
	     "--name <arg> (-n <arg>): this is a string option\n"
	     "  --op11-name <arg>: this is a string option\n"
	     "  --op12-name <arg>: this is a string option too\n");

  Option<std::string> op13("op13", "name2", 'n', "this is a string option as well");
  config2.reg(op13);
  test_usage("usage3", config2,
	     "usage: blah [options]\n"
	     "options:\n"
	     "--help: display this usage message\n"
	     "--name <arg>: this is a string option\n"
	     "  --op11-name <arg>: this is a string option\n"
	     "  --op12-name <arg>: this is a string option too\n"
	     "--name2 <arg>: this is a string option as well\n");

  Option<int> op14("op14", "name2", 'n', "this is an integer option");
  test_reg_reject("reg2", config2, op14,
		  "option is not type compatible with existing options with the same name"); 

  test_usage("usage4", config2,
	     "usage: blah [options]\n"
	     "options:\n"
	     "--help: display this usage message\n"
	     "--name <arg>: this is a string option\n"
	     "  --op11-name <arg>: this is a string option\n"
	     "  --op12-name <arg>: this is a string option too\n"
	     "--name2 <arg>: this is a string option as well\n");

  int argc1 = 3;
  char *argv1[] = {"progname", "--X", "hello", 0};
  test_parse_reject("parse1", config2, argc1, argv1, "--X",
		    "unrecognised or ambiguous command line option");

  int argc2 = 3;
  char *argv2[] = {"progname", "-x", "hello", 0};
  test_parse_reject("parse2", config2, argc2, argv2, "-x",
		    "unrecognised or ambiguous command line option");

  int argc3 = 3;
  char *argv3[] = {"progname", "--op11x", "hello", 0};
  test_parse_reject("parse3", config2, argc3, argv3, "--op11x",
		    "unrecognised or ambiguous command line option");

  int argc4 = 2;
  char *argv4[] = {"progname", "--name", 0};
  test_parse_reject("parse4", config2, argc4, argv4, "--name",
		    "command line option is missing the required argument");

  int argc5 = 2;
  char *argv5[] = {"progname", "--name2", 0};
  test_parse_reject("parse5", config2, argc5, argv5, "--name2",
		    "command line option is missing the required argument");

  int argc6 = 2;
  char *argv6[] = {"progname", "--op11-name2", 0};
  test_parse_reject("parse6", config2, argc6, argv6, "--op11-name2",
		    "unrecognised or ambiguous command line option");

  int argc7 = 2;
  char *argv7[] = {"progname", "--op1", 0};
  test_parse_reject("parse7", config2, argc7, argv7, "--op1",
		    "unrecognised or ambiguous command line option");

  int argc8 = 3;
  char *argv8[] = {"progname", "--name", "hello", 0};
  config2.parse(argc8, argv8);

  test_save("save1", config2,
	    "#this is a string option\n"
	    "op11-name=\"hello\"\n"
	    "\n"
	    "#this is a string option too\n"
	    "op12-name=\"hello\"\n"
	    "\n"
	    "#this is a string option as well\n"
	    "op13-name2=\"\"\n"
	    "\n");

  int argc9 = 3;
  char *argv9[] = {"progname", "--name2", "bye", 0};
  config2.parse(argc9, argv9);

  test_save("save2", config2,
	    "#this is a string option\n"
	    "op11-name=\"hello\"\n"
	    "\n"
	    "#this is a string option too\n"
	    "op12-name=\"hello\"\n"
	    "\n"
	    "#this is a string option as well\n"
	    "op13-name2=\"bye\"\n"
	    "\n");

  int argc10 = 3;
  char *argv10[] = {"progname", "--op12-name", "howdy", 0};
  config2.parse(argc10, argv10);

  test_save("save3", config2,
	    "#this is a string option\n"
	    "op11-name=\"hello\"\n"
	    "\n"
	    "#this is a string option too\n"
	    "op12-name=\"howdy\"\n"
	    "\n"
	    "#this is a string option as well\n"
	    "op13-name2=\"bye\"\n"
	    "\n");

  Config config3("blah");

  Option<bool> op15("op15", "name3", 'f', "this is a boolean option");
  config3.reg(op15);

  int argc11 = 2;
  char *argv11[] = {"progname", "--name3", 0};
  config3.parse(argc11, argv11);

  test_save("save3", config3,
	    "#this is a boolean option\n"
	    "op15-name3=true\n"
	    "\n");

  int argc12 = 2;
  char *argv12[] = {"progname", "--name3=TRUE", 0};
  config3.parse(argc12, argv12);

  test_save("save4", config3,
	    "#this is a boolean option\n"
	    "op15-name3=true\n"
	    "\n");

  int argc13 = 3;
  char *argv13[] = {"progname", "--name3", "yes", 0};
  config3.parse(argc13, argv13);

  test_save("save5", config3,
	    "#this is a boolean option\n"
	    "op15-name3=true\n"
	    "\n");

  int argc14 = 2;
  char *argv14[] = {"progname", "--name3=false", 0};
  config3.parse(argc14, argv14);

  test_save("save6", config3,
	    "#this is a boolean option\n"
	    "op15-name3=false\n"
	    "\n");

  int argc15 = 2;
  char *argv15[] = {"progname", "--name3=XXX", 0};
  test_parse_reject("parse8", config3, argc15, argv15, "--name3",
		    "option argument is invalid (argument 'XXX')");

  test_save("save7", config3,
	    "#this is a boolean option\n"
	    "op15-name3=false\n"
	    "\n");

  int argc16 = 2;
  char *argv16[] = {"progname", "-f", 0};
  config3.parse(argc16, argv16);

  test_save("save8", config3,
	    "#this is a boolean option\n"
	    "op15-name3=true\n"
	    "\n");

  int argc17 = 2;
  char *argv17[] = {"progname", "-f", 0};
  config3.parse(argc17, argv17);

  Config config4("blah");

  Option<uint> op16("op16", "name", 'f', "this is an unsigned int option");
  config4.reg(op16);

  test_check("check1", config4, false, "op16-name",
	     "required option argument is undefined");

  int argc18 = 2;
  char *argv18[] = {"progname", "-f", 0};
  test_parse_reject("parse9", config4, argc18, argv18, "-f",
		    "command line option is missing the required argument");

  int argc19 = 3;
  char *argv19[] = {"progname", "-f", "-1", 0};
  test_parse_reject("parse9", config4, argc19, argv19, "-f",
		    "option argument must be a non-negative integer (argument '-1')");

  int argc20 = 3;
  char *argv20[] = {"progname", "-f", "111111111111111111", 0};
  test_parse_reject("parse9", config4, argc20, argv20, "-f",
		    "option argument is invalid (argument '111111111111111111')");

  test_check("check2", config4, false, "op16-name",
	     "required option argument is undefined");

  int argc21 = 3;
  char *argv21[] = {"progname", "-f", "12345", 0};
  config4.parse(argc21, argv21);

  test_save("save9", config4,
	    "#this is an unsigned int option\n"
	    "op16-name=12345\n"
	    "\n");

  Option<uint> op17("op16", "name2", 'f', "this is also an unsigned int option", 9876);
  config4.reg(op17);

  test_usage("usage5", config4, 
	     "usage: blah [options]\n"
	     "options:\n"
	     "--help: display this usage message\n"
	     "--name <arg>: this is an unsigned int option\n"
	     "--name2 <arg>: this is also an unsigned int option (default = 9876)\n");

  test_save("save10", config4,
	    "#this is an unsigned int option\n"
	    "op16-name=12345\n"
	    "\n"
	    "#this is also an unsigned int option\n"
	    "op16-name2=9876\n"
	    "\n");

  int argc22 = 2;
  char *argv22[] = {"progname", "-f", 0};
  test_parse_reject("parse10", config4, argc22, argv22, "-f",
		    "unrecognised or ambiguous command line option");

  int argc23 = 3;
  char *argv23[] = {"progname", "--name2", "567", 0};
  config4.parse(argc23, argv23);

  test_save("save11", config4,
	    "#this is an unsigned int option\n"
	    "op16-name=12345\n"
	    "\n"
	    "#this is also an unsigned int option\n"
	    "op16-name2=567\n"
	    "\n");

  test_load("load1", config4, false,
	    "", 0, "",
	    "");

  test_load("load2", config4, false,
	    "", 0, "",
	    "# blah blah blah\n");

  test_load("load3", config4, false,
	    "", 0, "",
	    "\n\n");

  test_load("load4", config4, false,
	    "", 0, "",
	    "name=0");

  test_load("load5", config4, false,
	    "", 0, "",
	    "op16-name2=1");

  test_save("save12", config4,
	    "#this is an unsigned int option\n"
	    "op16-name=0\n"
	    "\n"
	    "#this is also an unsigned int option\n"
	    "op16-name2=1\n"
	    "\n");

  test_load("load6", config4, false,
	    "", 0, "",
	    "op16-name2  =  5");

  test_load("load7", config4, false,
	    "", 0, "",
	    "   name  =  1234");

  test_save("save13", config4,
	    "#this is an unsigned int option\n"
	    "op16-name=1234\n"
	    "\n"
	    "#this is also an unsigned int option\n"
	    "op16-name2=5\n"
	    "\n");

  test_load("load8", config4, true,
	    "name", 1, "option argument is invalid (argument 'abcd')",
	    "name=abcd\n");

  test_load("load9", config4, true,
	    "name", 4, "option argument is invalid (argument 'abcd')",
	    "\n"
	    "# blah blah\n"
	    "\n"
	    "name=abcd\n");

  test_load("load10", config4, true,
	    "name", 1, "option argument must be a non-negative integer (argument '-1')",
	    "name=-1\n");

  test_load("load11", config4, true,
	    "name", 1, "option argument is invalid (argument '')",
	    "name=\n");

  test_load("load12", config4, true,
	    "XXX-name", 1, "option 'XXX-name' does not exist",
	    "XXX-name=5");

  test_load("load13", config4, false,
	    "", 0, "",
	    "op16-name=10\n"
	    "name2=20\n");

  test_save("save14", config4,
	    "#this is an unsigned int option\n"
	    "op16-name=10\n"
	    "\n"
	    "#this is also an unsigned int option\n"
	    "op16-name2=20\n"
	    "\n");

  Config config5("blah");

  std::vector<int> defaults;
  defaults.push_back(1);
  defaults.push_back(-1);
  defaults.push_back(0);
  Option<std::vector<int> > op18("op18", "ints", 'i', "this is a vector of unsigned int option", defaults);
  config5.reg(op18);
  Option<std::vector<double> > op19("op19", "floats", 'f', "this is a vector of unsigned int option");
  config5.reg(op19);
  Option<std::vector<std::string> > op20("op20", "names", 'n', "this is a vector of string option");
  config5.reg(op20);

  test_check("check3", config4, false, "op19-floats",
	     "required option argument is undefined");

  int argc24 = 3;
  char *argv24[] = {"progname", "--floats", "0.0 3.1415 1", 0};
  config5.parse(argc24, argv24);

  int argc25 = 2;
  char *argv25[] = {"progname", "--names=bob bill tom", 0};
  config5.parse(argc25, argv25);

  test_save("save15", config5,
	    "#this is a vector of unsigned int option\n"
	    "op18-ints=1 -1 0\n"
	    "\n"
	    "#this is a vector of unsigned int option\n"
	    "op19-floats=0 3.1415 1\n"
	    "\n"
	    "#this is a vector of string option\n"
	    "op20-names=bob bill tom\n"
	    "\n");

  int argc26 = 2;
  char *argv26[] = {"progname", "--floats=0.0", 0};
  config5.parse(argc26, argv26);

  int argc27 = 2;
  char *argv27[] = {"progname", "--names=", 0};
  config5.parse(argc27, argv27);

  int argc28 = 2;
  char *argv28[] = {"progname", "--ints=1,2,4,8,16", 0};
  config5.parse(argc28, argv28);

  test_save("save16", config5,
	    "#this is a vector of unsigned int option\n"
	    "op18-ints=1 2 4 8 16\n"
	    "\n"
	    "#this is a vector of unsigned int option\n"
	    "op19-floats=0\n"
	    "\n"
	    "#this is a vector of string option\n"
	    "op20-names=\n"
	    "\n");

  test_load("load14", config5, false,
	    "", 0, "",
	    "floats=1,3,5.0,7,9.5\n");

  test_load("load15", config5, false,
	    "", 0, "",
	    "ints=1 3 5 7 9\n");

  test_load("load15", config5, false,
	    "", 0, "",
	    "names=tony ted thomas\n");

  test_save("save17", config5,
	    "#this is a vector of unsigned int option\n"
	    "op18-ints=1 3 5 7 9\n"
	    "\n"
	    "#this is a vector of unsigned int option\n"
	    "op19-floats=1 3 5 7 9.5\n"
	    "\n"
	    "#this is a vector of string option\n"
	    "op20-names=tony ted thomas\n"
	    "\n");

  Config config6("blah");

  Option<char> op22("op22", "char", 'c', "this is a character option", 'x');
  config6.reg(op22);
  Option<std::string> op21("op21", "string", 's', "this is a string option", "hello\tworld");
  config6.reg(op21);

  test_save("save18", config6,
	    "#this is a string option\n"
	    "op21-string=\"hello\\tworld\"\n"
	    "\n"
	    "#this is a character option\n"
	    "op22-char='x'\n"
	    "\n");

  int argc29 = 2;
  char *argv29[] = {"progname", "--char= ", 0};
  config6.parse(argc29, argv29);

  int argc30 = 3;
  char *argv30[] = {"progname", "--string", "\\t", 0};
  config6.parse(argc30, argv30);

  test_save("save19", config6,
	    "#this is a string option\n"
	    "op21-string=\"\\t\"\n"
	    "\n"
	    "#this is a character option\n"
	    "op22-char=' '\n"
	    "\n");

  int argc31 = 3;
  char *argv31[] = {"progname", "--string", "\\", 0};
  test_parse_reject("parse11", config6, argc31, argv31, "--string",
		    "unexpected end of escape sequence in '\\'");

  int argc32 = 3;
  char *argv32[] = {"progname", "--char", "", 0};
  test_parse_reject("parse12", config6, argc32, argv32, "--char",
		    "option argument must be a single character or escape sequence (argument '')");

  test_load("load16", config6, false,
	    "", 0, "",
	    "string=\"hello\"\n"
	    "char='a'\n");

  test_save("save20", config6,
	    "#this is a string option\n"
	    "op21-string=\"hello\"\n"
	    "\n"
	    "#this is a character option\n"
	    "op22-char='a'\n"
	    "\n");

  test_load("load17", config6, false,
	    "", 0, "",
	    "string=\"\\\"\"\n"
	    "char='\\\''\n");

  test_save("save21", config6,
	    "#this is a string option\n"
	    "op21-string=\"\\\"\"\n"
	    "\n"
	    "#this is a character option\n"
	    "op22-char='\\''\n"
	    "\n");

  test_load("load18", config6, true,
	    "char", 2, "option argument must be a single character or escape sequence (argument '')",
	    "# blah blah\n"
	    "char=''\n");

  return 0;
}
