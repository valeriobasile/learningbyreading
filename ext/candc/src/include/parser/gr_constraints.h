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

// GRConstraints

namespace NLP {
  namespace CCG {

    class GRConstraints {
    public:
      // create an empty tag history
      GRConstraints(const std::string &name = "grconstraints");
      // shared, reference counted copy constructor
      GRConstraints(const GRConstraints &other);

      ~GRConstraints(void);

      GRConstraints &operator=(const GRConstraints &other);

      const std::string &name(void) const;
      size_t size(void) const;

      bool get(const std::string &label, const std::string &word) const;
      bool operator()(const std::string &label, const std::string &word) const {
      	return get(label, word);
      }

      // if the word has been tagged before in the current document
      // change the tag, otherwise add a new entry for the current word
      void add(const std::string &label, const std::string &word);
    private:
      // private implementation trick
      class Impl_;
      Impl_ *impl_;
    };

  }
}
