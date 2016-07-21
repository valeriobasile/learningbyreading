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

    class Cache {
    private:
      class Impl_;
      Impl_ *impl_;
    public:
      // create an empty Cache
      Cache(const std::string &name);

      // shared, reference counted copy constructor
      Cache(const Cache &other);

      ~Cache(void);

      const std::string name(void) const;
      size_t size(void) const;

      void add(const std::string &str, const PDF &dist);

      const PDF *get(const std::string &str) const;
      const PDF *operator[](const std::string &str) const { return get(str); };

      void clear(void);
    };

  }
}
