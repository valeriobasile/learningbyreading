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
 */

namespace NLP {
  namespace Thesaurus {

    namespace HT = NLP::HashTable;

    class Object: public HT::FCounter {
    public:
      static const ulong USE_MATCH_RATIO = 2;
      static const ulong NTYPES = 10;

      static const ulong TEST_SET = 1 << 0;

      static ulong ntotal;
      static float ftotal;
      static ulong ncutoff;
      static float fcutoff;

      static void global_dump_2(std::ostream &out);
      static void global_load_2(std::istream &in, const std::string &fname);
    private:
      ulong _n;
      float _score;
      ulong _flags;
    public:
      Relations relations;
      Canonicals canonical;

      Object(const char *const str, ulong index, Hash hash, Object *next):
          HT::FCounter(str, index, hash, next), _n(0), _flags(0), _score(0.0), relations() {
        ntotal++;
      };
      Object(const Object *other, ulong index, Object *next):
          HT::FCounter(other, index, next), _n(other->_n), _flags(0), _score(other->_score),
          relations(other->relations), canonical(other->canonical){
        for(Canonicals::iterator i = canonical.begin(); i != canonical.end(); ++i)
          *i = &relations[*i - &*other->relations.begin()];
      };

      ~Object(void) {};

      void reset(void) {
        _freq = 0.0;
        _n = 0;
        _score = 0.0;
        _flags = 0;
        relations.resize(0);
        canonical.resize(0);
      }

      void testset(void){ _flags |= TEST_SET; };
      bool is_testset(void) const { return _flags & TEST_SET; };

      Object *find(const char *const str, const Hash hash){
        return (Object *)HT::FCounter::find(str, hash);
      }

      ulong unique(void) const { return _n; };
      void unique(ulong value) { _n = value; };

      void add(Attribute *attrib, float freq){
        relations.push_back(Relation(attrib, freq));
        inc(freq);
        ftotal += freq;
        _n++;
      }

      void add(Relation r){
        relations.push_back(r);
        inc(r.freq());
        ftotal += r.freq();
        _n++;
      }

      void dump_1(std::ostream &out) const;
      void load_1(Attributes attributes, std::istream &in, const std::string &fname);
      void dump_2(std::ostream &out) const;
      void load_2(Attributes attributes, std::istream &in, const std::string &fname);

      void printstats(std::ostream &out) const {
        out << _str << ' ' << _freq << "f " << _n << "n\n"; 
        for(Relations::const_iterator i = relations.begin(); i != relations.end(); i++){
          out << "   ";
          i->printstats(out);
          out << endl;
        }
      };

      void printcanonical(std::ostream &out) const {
        out << _str << ' ' << _freq << "f " << _n;
        out << "n " << canonical.size() << "c\n";
        for(Relations::const_iterator i = relations.begin(); i != relations.end(); i++){
          out << "   ";
          i->printstats(out);
          out << endl;
        }
      };

      void scale(float c){
        for(Relations::iterator i = relations.begin(); i != relations.end(); ++i)
          i->scale(c);
      };

      void score(Weight &weight){
        float remainder = 0.0;
        _score = 0.0;
        weight.init(this);
        for(Relations::reverse_iterator i = relations.rbegin(); i != relations.rend(); i++){
          remainder = i->score(weight, this, remainder);
          _score += i->score()*i->score();
        }
        _score = sqrt(_score);
      };
      float score(void) const { return _score; };

      void compact(void);
      void optimize(const Options &op);
      void heuristic(const Options &op);
      ulong npotentials(void){
        ulong sum = 0;
        for(vector<Relation *>::iterator i = canonical.begin(); i != canonical.end(); i++)
          sum += (*i)->aunique();
        return sum;
      };

      // framework interface

      float f(void) const { return _freq; };
      float n(void) const { return _n; };
      float p(void) const { return _freq/ftotal; };
      float q(void) const { return _n/(float)ntotal; };
    };

  }
}
