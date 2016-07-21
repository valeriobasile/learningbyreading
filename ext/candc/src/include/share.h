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

// NLP::Shared
// service class for implementing reference counted
// smart pointers (usually used in conjunction with
// the private implementation technique for large
// objects such as hash tables)

// there is no locking in this code so it is not thread safe
namespace NLP {

  class Shared {
  private:
    ulong _nrefs;	// number of references
  public:
    Shared(void): _nrefs(1) {};
    ~Shared(void) { /* assert(_nrefs == 0); */ };

    void inc_ref(void){ _nrefs++; };
    bool dec_ref(void){ return --_nrefs == 0; };
  };

  // take a reference to a ref counted object
  template <class T>
  T *share(T *t){ t->inc_ref(); return t; }

  // relinquish a ref counted object
  template <class T>
  void release(T *&t) {
    if(t && t->dec_ref()){
      delete t;
      t = 0;
    }
  }

}
