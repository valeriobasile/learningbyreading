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

    //to convert 0x?? into text: \n,\t,...
    template<>
    inline void
    Op<IO::Format>::write_config(std::ostream &out, std::string prefix, bool root) const {
      Option::write_config(out, prefix, root);
      out << " = \"" << bin2asc(value) << "\"";
    }

    //to convert 0x?? into text: \n,\t,...
    template<>
    inline void
    Op<IO::Format>::write_desc(std::ostream &out, bool full) const {
      Option::write_desc(out, full);
      if(has_default())
	out << " (def = \"" << bin2asc(DEFAULT_VALUE) << "\")";
      else if(!is_optional())
	out << REQUIRED;
    }

    //to convert escape sequences into 0x??
    template<>
    inline void
    Op<IO::Format>::set(const std::string &value_str){
      try{
	set_value(asc2bin(value_str));
      }catch(NLP::Exception e){
	die(e.what());
      }
    }

  }
}
