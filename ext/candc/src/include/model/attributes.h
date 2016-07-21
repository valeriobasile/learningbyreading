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

// NLP::Model::TagAttributes, NLP::Model::BiTagAttributes,
// NLP::Model::UniAttributes and NLP::Model::AffixAttributes
// specialised attribute lookup for different types of attribute
// each lookup returns an Attribute object which points to the
// sequence of features which have that particular attribute

namespace NLP {
  namespace Model {

    // Abstract base class for classes which efficiently represent
    // different contextual predicates in the various taggers
    // Each Attributes subclass must provide a method which knows
    // how to load its representation from disk
    class Attributes {
    public:
      const std::string &name;

      Attributes(const std::string &name): name(name) {};
      virtual ~Attributes(void) { /* do nothing */ };

      // override this function to polymorphically load attributes
      // from a stream, with a given type, and whether the
      // attribute values need to be added to the internel representation
      virtual Attribute &load(const Type &type, std::istream &in) = 0;
    };

    // represent binary features without values (e.g. has_uppercase)
    class BinAttributes: public Attributes {
    private:
      Attribute _attrib;
    public:
      BinAttributes(void): Attributes("BinAttributes") {};

      Attribute get(void) const { return _attrib; };
      Attribute &insert(void) { return _attrib; };

      Attribute operator()(void) const { return get(); };
      Attribute &operator()(void) { return insert(); };

      virtual Attribute &load(const Type &type, std::istream &in){
//        if(type.index)
//          throw NLP::Exception("cannot represent multiple types in BinAttributes");
        if(type.flags)
          throw NLP::Exception("unrecognised flag in BinAttributes::load");

        // TODO remove this dummy loading
        std::string dummy;
        in >> dummy;
        if(dummy != None::str)
          throw NLP::Exception("expected an empty place holder in BinAttributes::load");

        return _attrib;
      };
    };

    // represent tag based features (e.g. current POS tag)
    // these have a fixed number of possible values (e.g. the set of POS tags)
    // and are already translated into enumerated values, so an array lookup
    // is much more efficient than using a hash table
    class TagAttributes: public Attributes {
    private:
      const TagSet _tagset;
      std::vector<Attribute> _attributes;
    public:
      TagAttributes(TagSet tagset): Attributes("TagAttributes"),
          _tagset(tagset), _attributes(tagset.size()) {};

      Attribute get(Tag t) const { return _attributes[t.value()]; };
      Attribute &insert(Tag t) { return _attributes[t.value()]; };

      Attribute operator()(Tag t) const { return get(t); };
      Attribute &operator()(Tag t) { return insert(t); };

      Attribute &load(const Type &type, std::istream &in){
        if(type.index)
          throw NLP::Exception("cannot represent multiple types in TagAttributes");
        if(type.flags)
          throw NLP::Exception("unrecognised flag in TagAttributes::load");

        std::string value;
        in >> value;

        return insert(_tagset[value]); // _tagset.check(value));
      };
    };

    // same as the TagAttributes class except that it represents
    // bigrams of tag features, in particular, this is used for the
    // bigram feature of the two previously assigned tags
    // representation is equivalent to a 2 dimensional array
    class BiTagAttributes: public Attributes {
    private:
       const TagSet _tagset;
       const size_t _ntags;
       std::vector<Attribute> _attributes;
    public:
      BiTagAttributes(const TagSet &tagset): Attributes("BiTagAttributes"),
          _tagset(tagset), _ntags(tagset.size()), _attributes(_ntags*_ntags) {};

      Attribute get(Tag t1, Tag t2) const {
        return _attributes[t2.value()*_ntags + t1.value()];
      };
      Attribute &insert(Tag t1, Tag t2) {
        return _attributes[t2.value()*_ntags + t1.value()];
      };

      Attribute operator()(Tag t1, Tag t2) const { return get(t1, t2); };
      Attribute &operator()(Tag t1, Tag t2) { return insert(t1, t2); };

      Attribute &load(const Type &type, std::istream &in){
        if(type.index)
          throw NLP::Exception("cannot represent multiple types in BiTagAttributes");
        if(type.flags)
          throw NLP::Exception("unrecognised flag in BiTagAttributes::load");

        std::string value1, value2;
        in >> value1 >> value2;

        return insert(_tagset[value1], _tagset[value2]); // _tagset.check(value1), _tagset.check(value2));
      };
    };


    // same as the BiTagAttributes class except that it represents
    // trigrams of tag features, in particular, this is used for the
    // trigram feature of the three previously assigned tags
    // representation is equivalent to a 3 dimensional array
    class TriTagAttributes: public Attributes {
    private:
       const TagSet _tagset;
       const size_t _ntags;
       std::vector<Attribute> _attributes;
    public:
      TriTagAttributes(const TagSet &tagset): Attributes("TriTagAttributes"),
          _tagset(tagset), _ntags(tagset.size()), _attributes(_ntags*_ntags*_ntags) {};

      Attribute get(Tag t1, Tag t2, Tag t3) const {
        return _attributes[t3.value()*_ntags*_ntags + t2.value()*_ntags + t1.value()];
      };
      Attribute &insert(Tag t1, Tag t2, Tag t3) {
        return _attributes[t3.value()*_ntags*_ntags + t2.value()*_ntags + t1.value()];
      };

      Attribute operator()(Tag t1, Tag t2, Tag t3) const { return get(t1, t2, t3); };
      Attribute &operator()(Tag t1, Tag t2, Tag t3) { return insert(t1, t2, t3); };

      Attribute &load(const Type &type, std::istream &in){
        if(type.index)
          throw NLP::Exception("cannot represent multiple types in TriTagAttributes");
        if(type.flags)
          throw NLP::Exception("unrecognised flag in TriTagAttributes::load");

        std::string value1, value2, value3;
        in >> value1 >> value2 >> value3;

        return insert(_tagset[value1], _tagset[value2], _tagset[value3]);
        // _tagset.check(value1), _tagset.check(value2)), _tagset.check(value3));
      };
    };


    // hash table for storing unigram (i.e. word features)
    // however, it is also used to store more general string
    // features such as the word types as well
    // a type field is used to disambiguate entries with the
    // same value (e.g. word type 'a' is the same as word 'a'
    // so different values for type are used)
    class UniAttributes: public Attributes {
    public:
      UniAttributes(Lexicon lexicon);
      UniAttributes(UniAttributes &other);
      ~UniAttributes(void);

      size_t size(void) const;

      Attribute get(const Type &type, Word value) const;
      Attribute &insert(const Type &type, Word word);

      Attribute operator()(const Type &type, Word value) const { return get(type, value); };
      Attribute &operator()(const Type &type, Word value) { return insert(type, value); };

      Attribute &load(const Type &type, std::istream &in);

      void print_stats(std::ostream &out) const;
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

    class BigramAttributes: public Attributes {
    public:
      BigramAttributes(Lexicon lexicon);
      BigramAttributes(BigramAttributes &other);
      ~BigramAttributes(void);

      size_t size(void) const;

      Attribute get(const Type &type, Word word1, Word word2) const;
      Attribute &insert(const Type &type, Word word1, Word word2);

      Attribute operator()(const Type &type, Word word1, Word word2) const { return get(type, word1, word2); };
      Attribute &operator()(const Type &type, Word word1, Word word2) { return insert(type, word1, word2); };

      Attribute &load(const Type &type, std::istream &in);

      void print_stats(std::ostream &out) const;
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

    class TrigramAttributes: public Attributes {
    public:
      TrigramAttributes(Lexicon lexicon);
      TrigramAttributes(TrigramAttributes &other);
      ~TrigramAttributes(void);

      size_t size(void) const;

      Attribute get(const Type &type, Word word1, Word word2, Word word3) const;
      Attribute &insert(const Type &type, Word word1, Word word2, Word word3);

      Attribute operator()(const Type &type, Word word1, Word word2, Word word3) const { return get(type, word1, word2, word3); };
      Attribute &operator()(const Type &type, Word word1, Word word2, Word word3) { return insert(type, word1, word2, word3); };

      Attribute &load(const Type &type, std::istream &in);

      void print_stats(std::ostream &out) const;
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };


    // similar to UniAttributes except designed to store
    // prefix/suffix features represented using the Affix class
    class AffixAttributes: public Attributes {
    public:
      AffixAttributes(void);
      AffixAttributes(AffixAttributes &other);
      ~AffixAttributes(void);

      size_t size(void) const;

      Attribute get(const Type &type, Affix value) const;
      Attribute &insert(const Type &type, Affix vlaue);

      Attribute operator()(const Type &type, Affix value) const { return get(type, value); };
      Attribute &operator()(const Type &type, Affix value) { return insert(type, value); };

      Attribute &load(const Type &type, std::istream &in);

      void print_stats(std::ostream &out) const;
    private:
      // private implementation trick
      class _Impl;
      _Impl *_impl;
    };

  }
}
