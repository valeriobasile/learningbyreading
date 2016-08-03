// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include <cerrno>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <unistd.h>

using namespace std;

namespace NLP { namespace Port {

const int SOCK_FLAGS = SO_NOSIGPIPE;
const int BIND_FLAGS = SO_REUSEADDR;

static const int KILOBYTE = 1024;

ulong
get_usage(void){
  // rusage actually works on BSD/Mac OS X
  rusage usage;
  getrusage(RUSAGE_SELF, &usage);

  return usage.ru_maxrss*KILOBYTE;
}


void
setup_fpu(void) { /* nothing */ }


} }
