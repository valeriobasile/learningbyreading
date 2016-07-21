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

// Timer and Stopwatch
// classes for easy user/system timing in the history
// of the time function

// Timer works using initialisation as resource consumption
// design principle, i.e. the timer starts on construction
// and finishes printing the given message with the time
// when the Timer object is destroyed

// typically this is used as an object put on the stack
// so when a function exits, the timing is displayed
class Timer {
public:
  Timer(std::string msg);
  ~Timer(void);
private:
  // private implementation trick
  class _PImpl;
  _PImpl *_pimpl;
};

// Stop watch has the added feature of stopping/resetting
// before the object is destroyed
class Stopwatch {
public:
  Stopwatch(void);
  ~Stopwatch(void);

  void reset(void);
  double stop(void);
private:
  // private implementation trick
  class _PImpl;
  _PImpl *_pimpl;
};
