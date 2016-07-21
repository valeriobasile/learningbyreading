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

  extern const std::string PREFACE_HEADER;
  const bool PREFACE_MANDATORY = true;
  const bool PREFACE_OPTIONAL = false;

  extern std::string read_preface(const std::string &uri, std::istream &in,
				  ulong &nlines, bool mandatory = PREFACE_MANDATORY);

  extern std::string start_preface(int argc, char **argv);

}
