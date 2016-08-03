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
#include "timer.h"

#include "cluster.h"

using namespace std;
using namespace NLP::Port;

double
duration(Time &begin, Time &end){
  return (end.secs - begin.secs) + (end.usecs - begin.usecs)/1e6;
}

class Timer::_PImpl {
protected:
  string _msg;

  Time start_usr;
  Time start_sys;
  Time start_total; 
public:
  _PImpl(string msg): _msg(msg) {
    gettimes(start_usr, start_sys, start_total);
  }

  ~_PImpl(void) {
    Time end_usr, end_sys, end_total;

    gettimes(end_usr, end_sys, end_total);

    if(NLP::Cluster::rank != 0)
      return;

    cerr.setf(ios::fixed, ios::floatfield);
    cerr << _msg << ' ';
    cerr << "total: " << setprecision(2) << setw(7) << duration(start_total, end_total) << 's';
    cerr << " usr: " << setprecision(2) << setw(7) << duration(start_usr, end_usr) << 's';
    cerr << " sys: " << setprecision(2) << setw(7) << duration(start_sys, end_sys) << 's' << endl;
  }
};

Timer::Timer(string msg): _pimpl(new _PImpl(msg)) {}
Timer::~Timer(void) { delete _pimpl; }

class Stopwatch::_PImpl {
protected:
  Time start_usr;
  Time start_sys;
  Time start_total;
public:
  _PImpl(void){ reset(); }

  void reset(void){
    gettimes(start_usr, start_sys, start_total);
  }

  double stop(void){
    Time end_usr, end_sys, end_total;

    gettimes(end_usr, end_sys, end_total);
    return duration(start_total, end_total);
  };
};

Stopwatch::Stopwatch(void): _pimpl(new _PImpl()) {}
Stopwatch::~Stopwatch(void) { delete _pimpl; }

void
Stopwatch::reset(void) { _pimpl->reset(); }

double
Stopwatch::stop(void) { return _pimpl->stop(); }
