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

// NLP::Extract::Context and NLP::Extract::Attributes
// storage for contexts, and features and attributes in
// the model extraction process

// contexts are represented as a vector of attribute IDs

// attributes are stored with the list of features they
// are associated with, that is, the classes and their
// frequencies that a particular attribute appears with

namespace NLP {
  namespace Extract {

    using namespace NLP::Model;

    // a vector of Attribute identifiers
    // after each context vector has been created it is
    // sorted and dumped out to the models/context file
    typedef std::vector<ulong> Context;

    // hash table storage for features and attributes
    // each attribute is stored with the number of times it
    // appears with each class
 
    // Attributes is used in pass2 and pass3 of the extraction
    // process to count the features and attributes, sort them
    // in frequency order and then translate from attributes
    // to attribute identifiers

    // these identifiers are stored in model/attributes
    // the sorted features are stored in model/features

    class Attributes {
    public:
      // name for reporting errors and hashtable
      Attributes(const std::string &name, const NLP::Model::Registry &registry, const NLP::TagSet &klasses);
      // shared, reference counted copy constructor
      Attributes(const Attributes &other);

      ~Attributes(void);

      // shared, reference counted assignment
      Attributes &operator=(const Attributes &other);

      const std::string name(void) const;
      size_t size(void) const;

      // total number of attributes (by type)
      ulong nattributes(void) const { return size(); };
      // total number of features (by type)
      ulong nfeatures(void) const;

      // add the attribute id for attribute (type, value) to the context
      void operator()(Context &context, const Type &type, const std::string &value) const;
      void operator()(Context &context, const Type &type, const std::string &v1, const std::string &v2) const;
      void operator()(Context &context, const Type &type, const std::string &v1, const std::string &v2, const std::string &v3) const;

      // increment the count for feature (type, value, tag)
      void operator()(Tag klass, const Type &type, const std::string &value);
      void operator()(Tag klass, const Type &type, const std::string &v1, const std::string &v2);
      void operator()(Tag klass, const Type &type, const std::string &v1, const std::string &v2, const std::string &v3);

      // remove features with frequency less than freq
      void apply_cutoff(ulong freq);

      // apply a cutoff to a specific feature type
      void apply_cutoff(const Type &type, ulong freq);

      // apply a cutoff to a specific feature type or
      // apply a default cutoff
      void apply_cutoff(const Type &type, ulong freq, ulong def);
     
      void apply_attrib_cutoff(ulong freq);
      
      // merge attributes across the cluster
      void merge(void);

      // broadcast attributes across the cluster
      void bcast_indices(void);

      // dump out to the model/features and model/attributes files
      void save(const std::string &attributes, const std::string &features, const std::string &PREFACE);
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

  }
}
