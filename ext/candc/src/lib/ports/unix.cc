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
#include <fpu_control.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/socket.h>

#include <unistd.h>

using namespace std;

namespace NLP { namespace Port {

const int SOCK_FLAGS = MSG_NOSIGNAL;
const int BIND_FLAGS = SO_REUSEADDR;

ulong
get_usage(void){
  ifstream statm("/proc/self/statm");
  if(!statm)
    return 0;

  ulong usage = 0;
  statm >> usage;

  return usage*getpagesize();
}


void
setup_fpu(void) {
  fpu_control_t cw = 0x1372;
  _FPU_SETCW(cw);
}


} }
