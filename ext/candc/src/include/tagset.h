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

// NLP::TagSet
// translate between the names of tags and classes (strings)
// and their internal representation, which is an enumeration
// wrapped in Tag objects

namespace NLP {

  class TagSet {
  private:
    // private implementation trick
    class Impl_;
    Impl_ *impl_;
  public:
    const std::string &PREFACE;

    // create an empty tag set
    TagSet(const std::string &name);
    // load a tag set from a file
    TagSet(const std::string &name, const std::string &filename);

    // shared, reference counted copy constructor
    TagSet(const TagSet &other);

    ~TagSet(void);

    // shared, reference counted assignment operator
    TagSet &operator=(TagSet &other);

    const std::string &name(void) const;
    size_t size(void) const;

    // get the name of a tag (the string) from the Tag object
    const char *can(const Tag tag) const;
    std::string str(const Tag tag) const;
    std::string str_checked(const Tag tag) const;

    // convenient conversion methods using operator overloading
    const char *operator[](const Tag tag) const { return can(tag); };
    Tag operator[](const std::string &str) const { return tag(str); };
  
    // get the Tag object from the name of the tag
    // return the NONE tag (ordinal value zero) if unknown
    Tag tag(const std::string &str) const;
    std::string load_tags(const std::string &filename, Tags &tags) const;

    // get the Tag object from the name of the tag
    // throw NLP::Exception if the tag is unknown
    Tag check(const std::string &str) const;

    // load a tag dictionary from a file
    void load(const std::string &filename);

    void tag(const RawTags &raws, Tags &tags) const;
    void tag(const RawTags &raws, OffsetTags &tags) const;

    void str(const Tags &tags, RawTags &raws) const;
    void str(const MultiTags &tags, MultiRaws &raws) const;

    // add a tag (with a given frequency) to the tag dictionary
    // assigning the next free value in the enumeration
    // don't check whether it alread exists
    void insert(const std::string &str, ulong freq);

    // as above, but check for existance first
    void add(const std::string &str);
  };

}
