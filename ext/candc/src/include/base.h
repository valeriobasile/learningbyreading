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


#include "std.h"

#include "version.h"

namespace NLP {

  struct None {
  public:
    const static char *str;
    const static int val = 0;
    const static std::string::size_type len = 8;
    None(void){}
  };

  struct Sentinel {
  public:
    const static char *str;
    const static int val = 1;
    const static std::string::size_type len = 12;
    Sentinel(void){}
  };

  extern const None NONE;
  extern const Sentinel SENTINEL;

}

#include "port.h"

#include "exception.h"

#include "hash.h"

#include "raw.h"
#include "word.h"
#include "tag.h"

#include "utils/offset_vector.h"
#include "utils/escape.h"
#include "utils/string.h"

#include "sequence.h"
#include "parser/constraint.h"
#include "sentence.h"

#include "input.h"
#include "io/stream.h"

#include "tagset.h"
#include "lexicon.h"
