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
  namespace CCG {

    class RuleInstances{
    public:
      RuleInstances(void);
      RuleInstances(RuleInstances &other);
      ~RuleInstances(void);
      
      size_t size(void) const;
      
      void insert(const Cat *cat1, const Cat *cat2);
      bool get(const Cat *cat1, const Cat *cat2) const;
      bool operator()(const Cat *cat1, const Cat *cat2) const { return get(cat1, cat2); };
    private:
      class _Impl;
      _Impl *_impl;
    };


  }
}
