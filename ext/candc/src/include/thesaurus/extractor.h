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
  namespace Thesaurus {

    struct Result {
      std::string term;
      float score;
      Result(void): score(0.0) {};
      Result(std::string term, float score): term(term), score(score) {};
    };
    inline std::ostream &operator <<(std::ostream &out, const Result &result){
      return out << result.term << ' ' << setprecision(4) << result.score;
    }
    typedef std::vector<Result> Results;

    class Extractor {
    private:
      class _Impl;
      _Impl *_impl;
    public:
      Extractor(const Options &op);
      Extractor(const Extractor &other);
      Extractor &operator=(const Extractor &other);
      ~Extractor(void);

      void load(const std::string &filename);
      void load_attributes(const std::string &filename);
      void load_globals(const std::string &filename);
      void load_common(const std::string &filename);

      void load(std::istream &in, const std::string &fname);

      void load_attributes(std::istream &in, const std::string &filename);
      void load_globals(std::istream &in, const std::string &filename);
      void load_common(std::istream &in, const std::string &filename);

      void load_ascii(std::istream &in, const std::string &filename);
      void load_binary(std::istream &in, const std::string &filename);

      void dump_binary(const std::string &filename, int version = 1);
      void dump_binary(std::ostream &out, const std::string &filename, int version = 1);

      void pseudo(const std::string &newstr, const std::string &filename);
      void split(const std::string &objstr, float c, const std::string &newstr);
      void diff(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr);
      void sum(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr);
      void intersect(const std::string &obj1str, float c1, const std::string &obj2str, float c2, const std::string &newstr);

      void scale(const std::string &targetstr, float c);
      void add(const std::string &targetstr, const std::string &srcstr, float c);
      void sub(const std::string &targetstr, const std::string &srcstr, float c);
      void intersect(const std::string &targetstr, const std::string &srcstr, float c);

      void globals(std::ostream &out) const;

      void thesaurus(const std::string &outfile);
      void thesaurus(const std::string &infile, ulong nresults);

      void thesaurus(const std::string &obj, ulong nresults, Results &results, std::ostream &out);
      ulong rank(const std::string &obj1, const std::string &obj2, std::ostream &out);
      Result best(const std::string &obj, std::ostream &out);

      void explain(const std::string &obj1, const std::string &obj2, std::ostream &out);

      void object(const std::string &obj, std::ostream &out) const;
      void relations(const std::string &obj, ulong nresults, std::ostream &out) const;
      void canonical(const std::string &obj, std::ostream &out) const;
      void type(const std::string &obj, std::ostream &out) const;
      void attribute(const std::string &attr, std::ostream &out) const;

      float freq(const std::string &obj) const;
      float nrelations(const std::string &obj) const;
      float maxrelation(const std::string &obj) const;

      void clear(void);
      void set_weight(const std::string &s);
      void print_weights(std::ostream &out);

      void set_measure(const std::string &s);
      void print_measures(std::ostream &out);
    };

  }
}
