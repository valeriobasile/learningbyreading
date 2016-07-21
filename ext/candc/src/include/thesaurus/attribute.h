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

/*
 * Schwa (c) 2000
 * copyright James Richard Curran
 * jamesc@cogsci.ed.ac.uk
 *
 * Institute for Communicating and Collaborative Systems,
 * Division of Informatics,
 * University of Edinburgh
 ********************************************************
 * Lexicon header file
 ********************************************************
 * Lexicon is the class which manages individual lexical entries.
 * To improve the time and memory efficiency of the system,
 * it stores only one copy of the C-string for each lexical entry.
 * This means that strings can be compared by direct comparison.
 * Also, each lexical entry represented by a unique numerical identifier.
 * The lexicon can be saved and loaded from a file.
 * LexemeBucket is the class which is used in the implementation
 * of the Lexicon hash table as a light-weight bucket for chaining
 * the entries.
 * Lexeme is the class which contains the information about an individual lexeme.
 * Location is the class which contains the information about the location
 * of individual entries.
 */

namespace NLP {
  namespace Thesaurus {

    namespace HT = NLP::HashTable;

    class Attribute: public HT::FCounter {
    public:
      static float ftotal;
      static ulong ntotal;

      // dump shared attributes
      static void global_dump_2(std::ostream &out){
        dump_float(out, ftotal);
        dump_ulong(out, ntotal);
      };

      static void global_load_2(std::istream &in, const std::string &fname){
        if(!load_float(in, ftotal))
          throw NLP::IOException("could not load Attribute::ftotal", fname, in.tellg());
        if(!load_ulong(in, ntotal))
          throw NLP::IOException("could not load Attribute::ntotal", fname, in.tellg());
      }
    protected:
      Type *_type;
      ulong _nrelations;
    public:
      Attribute(const char *const str, Type *const type, ulong index, Hash hash, Attribute *next):
          HT::FCounter(str, index, hash, next), _type(type), _nrelations(0) { ntotal++; };
      ~Attribute(void) {};

      Attribute *find(const char *const str, const Hash hash){
        return (Attribute *)HT::FCounter::find(str, hash);
      }

      bool isverb(void) const { return _type->isverb(); };

      ulong unique(void) const { return _nrelations; };
      Type *type(void) const { return _type; };

      void set(float freq, ulong nrelations){
        _freq = freq;
        ftotal += freq;
        _nrelations = nrelations;
        ntotal += nrelations;
        _type->add(freq, nrelations);
      };

      void add(float freq){
        inc(freq);
        ftotal += freq;
        _nrelations++;
        _type->add(freq);
      };

      void printstats(std::ostream &out) const {
        out << _str << ' ' << _freq << "f " << _nrelations << 'n';
      };

      // framework interface
      float f(void) const { return _freq; };
      float n(void) const { return _nrelations; };
      float p(void) const { return _freq/ftotal; };
      float q(void) const { return _nrelations/(float)ntotal; };

      ulong bit(const ulong nbits) const { return _hash % nbits; };
    };

  }
}
