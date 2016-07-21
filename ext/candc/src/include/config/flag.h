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

namespace NLP {
  namespace Config {

    class OpFlag : public Op<bool> {
    private:
      unsigned long &flags;
      unsigned long mask;
    public:
      OpFlag(const std::string &prefix, const std::string &name, char short_name,
	     const std::string &desc, unsigned long &flags, unsigned long mask)
	: Op<bool>(name, desc), flags(flags), mask(mask){}

      OpFlag(const std::string &name, const std::string &desc,
	     unsigned long &flags, unsigned long mask, bool default_value)
	: Op<bool>(name, desc, default_value), flags(flags), mask(mask){
	set_value(default_value);
      }

      ~OpFlag(void){}
      
      virtual void set_value(bool val){
	Op<bool>::set_value(val);

	if(val){
	  flags |= mask;
	}else{
	  flags &= ~mask;
	}
      }

      virtual bool get(void) const {
	return (flags & mask) == mask;
      }
    };

  }
}
