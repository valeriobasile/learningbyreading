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


// convert a control character into its escaped version \t -> t
extern char char2esc(char c);
// convert a control character into its escaped version \t -> \\t
extern std::string char2esc_str(char c);
// convert an escaped character into its control version t -> \t
extern char esc2char(char c);


//to convert escape sequences into 0x??
extern std::string asc2bin(const std::string &s);
  
//to convert escape sequences into 0x??
extern std::string bin2asc(const std::string &s);
	
//remove quotes if they exist
extern std::string strip_quotes(const std::string &s);

//removes whitespace at the beginning and end of string
extern std::string strip_whitespace(const std::string &s);
