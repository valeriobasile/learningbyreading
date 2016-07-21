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

    class Object;
    class Weight;

    class Relation {
    private:
      Attribute *_attrib;
      float _freq;

      float _score;
      float _remainder;
    public:
      static ulong ntotal;
      static float ftotal;
      static ulong ncutoff;
      static float fcutoff;
      static ulong nzero;
      static float fzero;

      static void global_dump_2(std::ostream &out){
        dump_ulong(out, ntotal);
        dump_float(out, ftotal);
        dump_ulong(out, ncutoff);
        dump_float(out, fcutoff);
        dump_ulong(out, nzero);
        dump_float(out, fzero);
      }

      static void global_load_2(std::istream &in, const std::string &fname){
        if(!load_ulong(in, ntotal))
          throw NLP::IOException("could not load Relation::ntotal", fname, in.tellg());
        if(!load_float(in, ftotal))
          throw NLP::IOException("could not load Relation::ftotal", fname, in.tellg());
        if(!load_ulong(in, ncutoff))
          throw NLP::IOException("could not load Relation::ncutoff", fname, in.tellg());
        if(!load_float(in, fcutoff))
          throw NLP::IOException("could not load Relation::fcutoff", fname, in.tellg());
        if(!load_ulong(in, nzero))
          throw NLP::IOException("could not load Relation::nzero", fname, in.tellg());
        if(!load_float(in, fzero))
          throw NLP::IOException("could not load Relation::fzero", fname, in.tellg());
      }

      Relation(void): _attrib(0), _freq(0.0), _score(0.0), _remainder(0.0) {};

      Relation(Attribute *attrib, float freq):
        _attrib(attrib), _freq(freq), _score(0.0), _remainder(0.0) {
        ntotal++;
        ftotal += freq;
      };
      Relation(const Relation &other, float c):
        _attrib(other._attrib), _freq(other._freq * c), _score(0.0), _remainder(0.0) {
        ntotal++;
        ftotal += _freq;
      };
      ~Relation(void) {};

      const char *str(void) const { return _attrib->str(); };
      ulong bit(const ulong nbits) const { return _attrib->bit(nbits); };
      ulong index(void) const { return _attrib->index(); };
      Attribute *attrib(void) const { return _attrib; };

      int equal(const Relation &other) const { return _attrib == other._attrib; };
      int equal(const Relation *other) const { return _attrib == other->_attrib; };

      int compare(const Relation &other) const { return (int)index() - (int)other.index(); };
      int compare(const Relation *other) const { return (int)index() - (int)other->index(); };

      Type *type(void) const { return _attrib->type(); };
      bool isverb(void) const { return _attrib->isverb(); };

      void printstats(std::ostream &out) const {
        _attrib->printstats(out);
        out << ' ' << _freq << "f " << setprecision(4) << _score << "w";
      };

      // serialization interface
      void dump(std::ostream &out) const {
        dump_ulong(out, _attrib->index());
        dump_ulong(out, _freq);
      };

      static Relation Load(Attributes attributes, std::istream &in, const std::string &fname){
        ulong id;
        if(!load_ulong(in, id))
          throw NLP::IOException("could not load relation attribute identifier",
                                 fname, in.tellg());

        if(id >= attributes.size())
          throw NLP::IOException("attribute id larger than nattributes",
                                 fname, static_cast<ulong>(in.tellg()) - sizeof(ulong));

        Attribute *attribute = attributes[id];
        float freq;
        if(!load_float(in, freq))
          throw NLP::IOException("could not load relation frequency", fname, in.tellg());

        attribute->add(freq);
        return Relation(attribute, freq);
      };

      float score(Weight &weight, Object *object, float remainder){
        _score = weight(object, _attrib, this);
        return _remainder = remainder + _score;
      };

      void inc(float val){ _freq += val; };
      void scale(float c){ _freq *= c; };

      float freq(void) const { return _freq; };
      float afreq(void) const { return _attrib->freq(); };
      ulong aunique(void) const { return _attrib->unique(); };
      float score(void) const { return _score; };
      float remainder(void) const { return _remainder; };

      // framework interface

      float f(void) const { return _freq; };
      float n(void) const { return 1; };
      float p(void) const { return _freq/ftotal; };
      float q(void) const { return 1/(float)ntotal; };

      float af(void) const { return _attrib->f(); };
      float an(void) const { return _attrib->n(); };
    };

    typedef std::vector<Relation> Relations;
    typedef std::vector<Relation *> Canonicals;

    class RelationGTComp{
    public:
      bool operator ()(const Relation *r1, const Relation *r2){
        return r1->score() > r2->score();
      };
    };

    class RelationAlphaComp{
    public:
      bool operator ()(const Relation *r1, const Relation *r2){
        return r1->index() < r2->index();
      };

      bool operator ()(const Relation &r1, const Relation &r2){
        return r1.index() < r2.index();
      };
    };

    class RelationLTZero{
    public:
      ulong &n;
      float &f;

      RelationLTZero(ulong &n, float &f): n(n), f(f) {};

      bool operator ()(const Relation &r){
        if(r.score() <= 0){
          n++;
          f += r.freq();
          return true;
        }
        return false;
      };
    };

  }
}
