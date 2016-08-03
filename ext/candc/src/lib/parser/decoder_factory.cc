// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_decoder.h"
#include "parser/decoder_factory.h"
#include "parser/decoder_deps_recall.h"
#include "parser/decoder_derivs.h"
#include "parser/decoder_derivs_random.h"

using namespace std;

namespace NLP { namespace CCG {

void
DecoderFactory::check(const std::string &name){
  if(name != "deps" && name != "derivs" && name != "random")
    throw NLP::Exception("unrecognised decoder name '" + name + "' [deps, derivs, random]");
}

DecoderFactory::DecoderFactory(const std::string &name){
  if(name == "deps")
    decoder = new DepsRecallDecoder;
  else if(name == "derivs")
    decoder = new DerivsDecoder;
  else if(name == "random")
    decoder = new DerivsRandomDecoder;
  else
    throw NLP::Exception("unrecognised decoder name '" + name + "'");
}

} }
