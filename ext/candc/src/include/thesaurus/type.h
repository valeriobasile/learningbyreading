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

    namespace HT = NLP::HashTable;

    class Type: public HT::FCounter {
    public:
      static float ftotal;
      static ulong ntotal;

      // dump shared attributes
      static void global_dump_2(std::ostream &out){
        dump_float(out, ftotal);
        dump_ulong(out, ntotal);
      };

      static void global_load_2(std::istream &in, char *fname){
        if(!load_float(in, ftotal))
          throw NLP::IOException("could not load Type::ftotal", fname, in.tellg());
        if(!load_ulong(in, ntotal))
          throw NLP::IOException("could not load Type::ntotal", fname, in.tellg());
      }
    protected:
      ulong _nrelations;
      const bool _isverb;

      bool _isstrverb(const char *const str){
        return  str[0] == 'D' || str[0] == 'S' || str[0] == 'I' ||	// relations for SEXTANT
               (str[0] == 's' && str[1] == 'u') ||			// subject relations for MINIPAR
                str[0] == 'o';						// object relations for MINIPAR
      };
    public:
      Type(const char *const str, ulong index, Hash hash, Type *next):
          HT::FCounter(str, index, hash, next), _nrelations(0),
          _isverb(_isstrverb(str)) { ntotal++; };
      ~Type(void) {};

      Type *find(const char *const str, const Hash hash){
        return (Type *)HT::FCounter::find(str, hash);
      }

      ulong unique(void) const { return _nrelations; };
      bool isverb(void) const { return _isverb; };

      void add(float freq){
        inc(freq);
        ftotal += freq;
        _nrelations++;
      };

      void add(float freq, ulong nrelations){
        inc(freq);
        ftotal += freq;
        _nrelations += nrelations;
      };

      void printstats(std::ostream &out) const {
        out << _str << ' ' << _freq << "f " << _nrelations << 'n';
      };

      // relation tests

      // framework interface
      float f(void) const { return _freq; };
      float n(void) const { return _nrelations; };
      float p(void) const { return _freq/(float)ftotal; };
      float q(void) const { return _nrelations/(float)ntotal; };

      ulong bit(const ulong nbits) const { return _hash % nbits; };
    };
  }
}
