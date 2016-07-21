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

// NLP::Lexicon
// stores canonical strings and their frequencies
// used in many places in the system for lexicons and
// counting other kinds of strings such as classes,
// postags and tag-dict pairs in the Extraction classes

// canonical strings (i.e. one instance of each string
// seen in the training data) are an important efficiency
// strategy because they reduce the number of feature
// lookups on features involving words (because we know
// that if we haven't seen the word, then we haven't seen
// any features involving the word).  It also makes string
// comparisons in these feature lookups fast because we
// can compare string pointers directly.

// canonical strings are wrapped in Word objects to distinguish
// them from regular const char * values in the code

namespace NLP {

  class Huge {
  private:
    // private implementation trick
    class Impl_;
    Impl_ *impl_;
  public:
    const std::string &PREFACE;

    // an empty lexicon
    Huge(const std::string &name);
    // a lexicon loaded from the file src
    Huge(const std::string &name, const std::string &filename);
    // shared, reference counted copy constructor
    Huge(const Huge &other);

    ~Huge(void);

    // shared, reference counted assignment
    Huge &operator=(const Huge &other);

    const std::string &name(void) const;
    size_t size(void) const;

    // return the canonical version of a given string
    // returns a null Word object (const char *)0 if the
    // string does not exist

    Word check(const std::string &str) const;
    Word can(const std::string &str) const;

    Word operator[](const std::string &str) const { return can(str); };

    void can(const RawWords &raws, Words &words) const;
    void can(const RawWords &raws, OffsetWords &words) const;

    // return the frequency of a given string
    ulong freq(const std::string &str) const;

    // sorting alphabetically, by frequency or in reverse by frequency
    void sort_by_alpha(void);
    void sort_by_freq(void);
    void sort_by_rev_freq(void);

    void clear(void);

    // load additional strings into the lexicon from the given file
    void load(const std::string &filename);

    // dump out the lexicon to disk
    void save(const std::string &filename, const std::string &preface) const;
    void save(std::ostream &stream, const std::string &preface) const;

    // add strings to the lexicon (when you know they don't already exist)
    // does not check to see if the entry is already there
    void insert(const std::string &str, ulong freq);

    // add words to the lexicon, checking to see if an entry already exists
    Word add(const std::string &str);
    void add(const std::string &str, ulong freq);

    // remove entries with a frequency less than freq
    void apply_cutoff(ulong freq);

    Word attribute_check(const std::string &str) const;
    Word attribute_can(const std::string &str) const;
    Word attribute_add(const std::string &str);

    void print_stats(std::ostream &out) const;
  };

}
