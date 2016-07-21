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

// NLP::MaxEnt::Context
// represents a training instance as a sequence of Attributes
// with an associated class and empirical value
// used in the maximum entropy estimation code
// this Context -> Attributes representation is the core of
// our fast loop reordering which makes model expectations 
// as efficient to calculate as possible

namespace NLP {
  namespace MaxEnt {

    // warning: this class cannot be subclassed because it
    // assumes that _begin is at the end of the object
    // warning: you can only create a Context on the heap
    // because new creates space for the _begin array
    class Context {
    public:
      const long emp;		// frequency of this training instance
      const ushort klass;	// classification
    private:
      const ushort _size;	// number of attributes
      Attribute _begin[1];	// array of attributes which is dynamically allocated
				// using operator new to follow on the end of the object
    public:
      // allocate the extra space needed at the end of the Context object
      // for the extra attributes.  Subtract one to take into account _begin[1]
      // which stops compilers whinging about _begin[0]
      void *operator new(size_t size, ushort n) {
        return static_cast<void *>(::new char[size + (n-1)*sizeof(Attribute)]);
      };
      void operator delete(void *ptr, ushort) { ::delete [] static_cast<char *>(ptr); };

      Context(long emp, ushort klass, ushort size):
          emp(emp), klass(klass), _size(size) {};
      ~Context(void) {};

      size_t size(void) const { return _size; };

      Attribute *begin(void) { return _begin; };
      const Attribute *begin(void) const { return _begin; };

      const Attribute *end(void) const { return _begin + _size; };
      Attribute *end(void) { return _begin + _size; };

      // log-likelihood with the number of training instances
      // which is a constant over all contexts ignored
      double llhood(const std::vector<double> &p_classes){
        return emp*log(p_classes[klass]/emp);
      };
    };

    // Contexts have to be used in pointer form otherwise the
    // allocation of the attributes vector at the end doesn't work
    typedef std::vector<Context *> Contexts;
  }
}
