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

#include <windows.h>
#include <io.h>
#include <cerrno>

using namespace std;

namespace NLP { namespace Port {

const char DIR_SEP = '/';
const int SOCK_FLAGS = 0; // no MSG_NOSIGNAL under Windows;
const int BIND_FLAGS = SO_REUSEADDR;

void
gettimes(Time &usr, Time &sys, Time &total){
  usr.secs = 0;
  usr.usecs = 0;
  sys.secs = 0;
  sys.usecs = 0;

  // this isn't the best way to do this in Windows but it works
  // on everything from Win9X onwards ...

  SYSTEMTIME systime;
  GetSystemTime(&systime);
  FILETIME filetime;
  SystemTimeToFileTime(&systime, &filetime);

  ULONGLONG micro_secs;
  memcpy(&micro_secs, &filetime, sizeof(ULONGLONG));
  micro_secs /= 10;
  total.secs = static_cast<int>(micro_secs/1000000);
  total.usecs = static_cast<int>(micro_secs%1000000);
}

void
make_directory(const std::string &dir){
  if(_mkdir(dir.c_str()))
    if(errno == EEXIST)
      cerr << "using existing directory " << dir << endl;
    else
      throw NLP::IOException("could not create directory", dir);
}

ulong
get_usage(void){
  throw Exception("Port::get_usage not implemented for MinGW yet");

  return 0;
}

void
setup_fpu(void) { /* nothing */ }

} }
