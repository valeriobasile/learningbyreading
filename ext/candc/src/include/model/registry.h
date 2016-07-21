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
  namespace Model {

    class Attribute;
    class Attributes;
    
    // class for registering Attributes objects and mapping their
    // text representation (in the model/attributes file) to the
    // correct Attributes object
    class Registry {
    public:
      Registry(const std::string &name);
      // shared, reference counted copy constructor
      Registry(const Registry &other);

      ~Registry(void);

      // shared, reference counted assignment
      Registry &operator=(const Registry &other);

      const std::string name(void) const;
      size_t size(void) const;

      // register the mapping from text representation (stored in Type objects)
      // to the correct Attributes storage

      // This allows an Attributes subclass (e.g. UniAttributes) to be used to
      // store different types of contextual predicate in the same structure.
      // Other subclasses (e.g. TagAttributes) have a different representation
      // which makes it more efficient to have a separate TagAttributes object
      // for each type of contextual predicate
      void reg(const Type &type, Attributes &attribs);

      void reg(const Type &type);
      
      // get back a registered Attributes object with a given id
      Attributes &get(const std::string &id);
      
      const char *canonize(const std::string &id) const;

      // ask a registered Attributes object with a given
      // attributes id to load an Attribute from the given stream 
      Attribute &load(const std::string &id, std::istream &in);
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

  }
}
