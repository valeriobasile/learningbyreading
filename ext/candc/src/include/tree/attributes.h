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
  namespace Tree {

    using namespace NLP::CCG;

    typedef ulong Type;

    const static Type LEX = 0;
    const static Type LEX_WORD = 1;
    const static Type LEX_POS = 2;
    const static Type ROOT = 3;
    const static Type ROOT_WORD = 4;
    const static Type ROOT_POS = 5;
    const static Type DEP_WORD = 6;
    const static Type DEP_POS = 7;
    const static Type DEP_WORD_POS = 8;
    const static Type DEP_POS_WORD = 9;
    const static Type DEP_DIST = 10;
    const static Type BRULE = 11;
    const static Type URULE = 12;
    const static Type BRULE_HEAD = 13;
    const static Type URULE_HEAD = 14;
    const static Type BRULE_POS = 15;
    const static Type URULE_POS = 16;
    const static Type BRULE_HEAD_HEAD = 17;
    const static Type BRULE_POS_HEAD = 18;
    const static Type BRULE_HEAD_POS = 19;
    const static Type BRULE_POS_POS = 20;
    const static Type GEN_RULE = 21;
    const static Type DIST_ADJ_HEAD = 22;
    const static Type DIST_VERBS_HEAD = 23;
    const static Type DIST_PUNCT_HEAD = 24;
    const static Type DIST_ADJ_POS = 25;
    const static Type DIST_VERBS_POS = 26;
    const static Type DIST_PUNCT_POS = 27;

    class CatValueAttributes {
    public:
      CatValueAttributes(void);
      CatValueAttributes(CatValueAttributes &other);
      ~CatValueAttributes(void);
      
      size_t size(void) const;
      
      void insert(ulong id, Type t, const Cat *cat, Word value);
      ulong get_id(Type t, const Cat *cat, Word value) const;
      ulong operator()(Type t, const Cat *cat, Word value) const {
	return get_id(t, cat, value);
      };
      double weight(Type t, const Cat *cat, Word value) const;
      void set_weights(const double *weights);
    private:
      class Impl;
      Impl *_impl;
    };
    
    class DependencyAttributes {
    public:
      DependencyAttributes(void);
      DependencyAttributes(DependencyAttributes &other);
      ~DependencyAttributes(void);
      
      size_t size(void) const;
      
      void insert(ulong id, Type t, Word head, RelID rel, Word var, RuleID rule,
		  CatID lrange, ushort dist);
      ulong get_id(Type t, Word head, RelID rel, Word var, RuleID rule, CatID lrange,
		   ushort dist) const;
      ulong operator()(Type t, Word h, RelID rel, Word var, RuleID r, CatID lrange,
		       ushort dist) const {
	return get_id(t, h, rel, var, r, lrange, dist);
      };
      double weight(Type t, Word h, RelID rel, Word var, RuleID r, CatID lrange,
		    ushort dist) const;
      void set_weights(const double *weights);
    private:
      class Impl;
      Impl *_impl;
    };

    class DistanceAttributes {
    public:
      DistanceAttributes(void);
      DistanceAttributes(DistanceAttributes &other);
      ~DistanceAttributes(void);
      
      size_t size(void) const;
      
      void insert(ulong id, RelID rel, RuleID rule, CatID lrange, Dir dir, 
		  VerbDistance nverbs, PunctDistance npunct, ConjDistance nconj);
      ulong get_id(RelID rel, RuleID rule, CatID lrange, Dir dir, VerbDistance nverbs, 
		   PunctDistance npunct, ConjDistance nconj) const;
      ulong operator()(RelID rel, RuleID r, CatID lrange, Dir dir, VerbDistance nverbs,
		       PunctDistance npunct, ConjDistance nconj) const {
	return get_id(rel, r, lrange, dir, nverbs, npunct, nconj);
      }
      double weight(RelID rel, RuleID rule, CatID lrange, Dir dir, VerbDistance nverbs, 
		    PunctDistance npunct, ConjDistance nconj) const;
      void set_weights(const double *weights);
    private:
      class Impl;
      Impl *_impl;
    };

    class RuleAttributes {
    public:
      RuleAttributes(void);
      RuleAttributes(RuleAttributes &other);
      ~RuleAttributes(void);
      
      size_t size(void) const;
      
      void insert(ulong id, Type t, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		  Word value1, Word value2);
      void add1(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		Word value1, Word value2);
      void print_entries(std::ostream &out);
      ulong get_id(Type t, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		Word value1, Word value2) const;
      ulong operator()(Type t, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		       Word value1, Word value2) const {
	return get_id(t, cat1, cat2, cat3, value1, value2);
      }
      double weight(Type t, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		    Word value1, Word value2) const;
      void set_weights(const double *weights);
    private:
      class Impl;
      Impl *_impl;
    };

    class DepDistAttributes {
    public:
      DepDistAttributes(void);
      DepDistAttributes(DepDistAttributes &other);
      ~DepDistAttributes(void);
      
      size_t size(void) const;
      
      void insert(ulong id, Type t, const Cat *cat1, const Cat *cat2, const Cat *cat3, Word head, ushort count);
      void add1(Type type, const Cat *cat1, const Cat *cat2, const Cat *cat3, Word head, ushort count);
      void print_entries(std::ostream &out);
      ulong get_id(Type t, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		   Word head, ushort count) const;
      ulong operator()(Type t, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		       Word head, ushort count) const {
	return get_id(t, cat1, cat2, cat3, head, count);
      };

      double weight(Type t, const Cat *cat1, const Cat *cat2, const Cat *cat3,
		     Word head, ushort count) const;
      void set_weights(const double *weights);
    private:
      class Impl;
      Impl *_impl;
    };

    class GenRuleAttributes{
    public:
      GenRuleAttributes(void);
      GenRuleAttributes(GenRuleAttributes &other);
      ~GenRuleAttributes(void);
      
      size_t size(void) const;
      
      void insert(ulong id, const Cat *cat, ushort rule);
      ulong get(const Cat *cat, ushort rule) const;

      // don't need colon here:

      ulong operator()(const Cat *cat, ushort rule) const { return get(cat, rule); };

      double weight(const Cat *cat, ushort rule) const;
      void set_weights(const double *weights);
    private:
      class Impl;
      Impl *_impl;
    };
    
  }
}
