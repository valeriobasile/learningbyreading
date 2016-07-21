/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) University of Sydney
// Copyright (c) James R. Curran and Tara Murphy
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

namespace NLP {
  namespace Bootstrap {

    const static double LOG1p4 = 0.33647223662121289;
    typedef ulong Cat;

    struct Term;

    struct NGram {
      typedef ulong id_type;

      const static size_t NFREQBITS = 6;
      const static size_t BYTES2BITS = 8;
      const static size_t NBITS = sizeof(id_type)*BYTES2BITS;

      id_type bits;

      NGram(void): bits(0){}
      NGram(id_type id, ulong lfreq): bits(lfreq << (NBITS - NFREQBITS) | id){}

      id_type id(void) const { return bits << NFREQBITS >> NFREQBITS; }
      ulong lfreq(void) const { return static_cast<ulong>(bits >> (NBITS - NFREQBITS)); }
    };

    struct Template {
      Cat cat;
      ushort hits;
      ushort errs;
      ushort nelems;
      ushort last;
      std::string tmpl;
      NGram ngrams[0];

      Template(ulong nterms): cat(0), hits(0), errs(0), nelems(nterms), last(0){}

      void *operator new(size_t size, Pool *pool, size_t nngrams){
	return (void *)pool->alloc(size + nngrams*sizeof(NGram));
      }

      void operator delete(void *, Pool *, size_t) { /* do nothing */ }
    };

    inline
    std::ostream &operator <<(std::ostream &out, const Template &tmpl){
      return out << tmpl.tmpl << " +" << tmpl.hits << '-' << tmpl.errs << '/' << tmpl.nelems;
    }

    typedef std::vector<Template *> Templates;

    struct Term {
      Word word;
      Cat cats;
      ushort hits;
      ushort errs;
      ushort nelems;
      ushort added;
      Template *templates[0];

      Term(Word word): word(word), cats(0), hits(0), errs(0), nelems(0), added(0){}

      void *operator new(size_t size, Pool *pool, size_t ntemplates){
	return (void *)pool->alloc(size + ntemplates*sizeof(Template *));
      }

      void operator delete(void *, Pool *, size_t) { /* do nothing */ }
    };

    typedef std::vector<Term *> Terms;

    template <class T>
    class HitCmp {
    public:
      bool operator()(const T *t1, const T *t2){
	if(t1->hits > t2->hits)
	  return true;
	if(t1->hits == t2->hits)
	  return t1->nelems > t2->nelems;
	return false;
      }
    };

    class Bootstrap {
    protected:
      void load_terms_(void);
      void load_ngrams_(void);
    public:
      Pool *pool;
      Lexicon lexicon;

      Template **templates;
      Term **terms;

      ulong nngrams;
      ulong ntemplates;
      ulong nterms;
      std::string lexicon_filename;
      std::string ngrams_filename;

      Bootstrap(const std::string &config);
      ~Bootstrap(void);

      Term *get_term(const Raw &s) const;
      std::string get_word(const Term *term) const;

      void words2terms(const Raws &raws, Terms &terms) const;
      void terms2words(const Terms &terms, Raws &raws) const;
      void load_terms(const std::string &filename, Terms &terms, Cat cats = 0) const;
    };


    class Category {
    public:
      const std::string name;
      ulong id;
      Cat cat;

      const Bootstrap &bootstrap;

      Terms previous;
      Terms current;

      Templates tmpls;

      Category(const std::string &name, ulong id, const Bootstrap &bootstrap)
	: name(name), id(id), cat(1 << id), bootstrap(bootstrap){}

      void get_templates(ushort ITERATION);
      void get_terms(ushort ITERATION, ulong NBEST_TEMPS, ulong DEPTH_TEMPS);
      void add_terms(ushort ITERATION, ulong NBEST_TERMS, ulong DEPTH_TERMS);
      void cleanup(ushort ITERATION);
    };

    typedef std::vector<Category *> Categories;

  }
}
