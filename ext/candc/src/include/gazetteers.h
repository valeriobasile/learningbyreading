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

// NLP::Gazettteers
// allows fast lookup of up to 32 gazetteers simultaneously
// this number could be larger if the internal representation is changed
// each word is represented using a bit vector indicating which gazetteers
// it is a member of

namespace NLP {

  // the bit vector is currently stored in a single unsigned long
  // hence (on most modern machines) has 32 bits available for
  // different types of gazetteer information
  typedef ulong GazFlags;

  class Gazetteers {
  private:
    // private implementation trick
    class Impl_;
    Impl_ *impl_;
  public:
    // for the freq(lower) > freq(upper) gazetteer
    // used in the gaz_common features in NER/Exp
    static const GazFlags COMMON = 1 << 0;

    // first names (personal names)
    static const GazFlags FIRST = 1 << 1;
    // last names (surnames)
    static const GazFlags LAST = 1 << 2;

    // three location gazetteers
    // uses a relatively simple breakdown of longer names

    // first word in name goes in first gazetteer e.g. New in New York
    static const GazFlags LOC1 = 1 << 3;
    // second word goes in second gazetteer e.g. York in New York
    static const GazFlags LOC2 = 1 << 4;
    // third, fourth etc ... word goes in third gazetteer
    static const GazFlags LOC3 = 1 << 5;

    // look at all of the location gazetteer files simultaneously
    static const GazFlags LOCATION = LOC1 | LOC2 | LOC3;

    // create an empty gazetteer
    Gazetteers(const std::string &name);
    // load the gazetteer using the base directory dir and the
    // gazetteer configuration file config
    Gazetteers(const std::string &name, const std::string &dir, const std::string &config);
    // shared, reference counted copy constructor
    Gazetteers(const Gazetteers &other);

    ~Gazetteers(void);

    // shared, reference counted assignment
    Gazetteers &operator=(const Gazetteers &other);

    const std::string name(void) const;
    const std::string config(void) const;
    size_t size(void) const;

    // retrieve the gazetteer flags for a string in its current case
    GazFlags exists(const std::string &str) const;
    GazFlags operator[](const std::string &str) const { return exists(str); };

    // retrieve the gazetteer flags for a string converted to lowercase
    GazFlags lower(const std::string &str) const;

    // add these flags to the entry for a given string
    // bits are bitwise OR'ed with existing flags
    void add(const std::string &str, GazFlags flags);

    // load words from a file into the gazetteer with particular flags
    void load(const std::string &type, const std::string &filename, GazFlags flags);
  };

}
