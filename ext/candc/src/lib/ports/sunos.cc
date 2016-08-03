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
#include <signal.h>
#include <unistd.h>

using namespace std;

namespace NLP { namespace Port {

// a very dirty trick to ensure SIGPIPE is ignored since
// SunOS doesn't define SO_NOSIGPIPE or MSG_NOSIGNAL flags for sockets
//
// we should probably have a separate initialisation function
// for each port which is called by main but we don't
const int SOCK_FLAGS = sigignore(SIGPIPE) && 0;
const int BIND_FLAGS = SO_REUSEADDR;

ulong
get_usage(void){
  throw Exception("Port::get_usage not implemented for SunOS yet");

  return 0;
}

void
setup_fpu(void) { /* nothing */ }

} }
