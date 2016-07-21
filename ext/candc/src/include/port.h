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
  namespace Port {

    extern const char DIR_SEP;
    extern const int BIND_FLAGS;
    extern const int SOCK_FLAGS;

    extern const char *BOLD;
    extern const char *RED;
    extern const char *OFF;

    struct Time {
      int secs;
      int usecs;
    };

    extern void make_directory(const std::string &dir);
    extern void gettimes(Time &usr, Time &sys, Time &total);
    extern ulong get_usage(void);
    extern void setup_fpu(void);

  }
}
