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


#include <string>
#include <sstream>
#include <iostream>
#include <cassert>

namespace NLP {
  namespace Config {

    class OpFlags : public FlagOption {
    private:
      unsigned long myflags;
    public:


  BaseFlagOption(const std::string &prefix, const std::string &name, char short_name,
		 const std::string &desc, unsigned long mask, bool default_value):
      FlagOption(prefix, name, short_name, desc, &myflags, mask, default_value){
    set_value(default_value);
  }

  /**
   * This constructor doesn't have a default... an ConfigError
   * will be thrown if it is used before a default is set.
   *
   * \param prefix	should be the 'name' of subprogram. DO NOT
   * 					set this to ""; everything must have a prefix.
   * \param name		long name of option (--name)
   * \param short_name	shortcut (-n)
   * \param desc		how the usage stuff should describe it
   * \param mask		which bits to set on/off in the long; all must
   * 					be on for it to be 'true'; setting it to either
   * 					true/false sets all the relevant bits
   */
  BaseFlagOption(const std::string &prefix, const std::string &name, char short_name,
		 const std::string &desc, unsigned long *flags, unsigned long mask):
    FlagOption(prefix, name, short_name, desc, &myflags, mask){}

  ~BaseFlagOption(void){}

  friend class ChildFlagOption;
};

  }
}

