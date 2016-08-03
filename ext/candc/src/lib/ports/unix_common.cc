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
#include <unistd.h>

using namespace std;

namespace NLP { namespace Port {

const char DIR_SEP = '/';

void
gettimes(Time &usr, Time &sys, Time &total){
  rusage usage;
  getrusage(RUSAGE_SELF, &usage);

  usr.secs = usage.ru_utime.tv_sec;
  usr.usecs = usage.ru_utime.tv_usec;
  sys.secs = usage.ru_stime.tv_sec;
  sys.usecs = usage.ru_stime.tv_usec;

  total.secs = usr.secs + sys.secs;
  total.usecs = usr.usecs + sys.usecs;
}

void
make_directory(const std::string &dir){
  if(mkdir(dir.c_str(), 0755)){
    if(errno == EEXIST)
      cerr << "using existing directory " << dir << endl;
    else
      throw NLP::IOException("could not create directory", dir);
  }
}

} }
