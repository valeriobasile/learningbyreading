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

    class Match {
    public:
      Object *object;
      float score;

      Match(void): object(0), score(0) {};
      Match(Object *object, float score): object(object), score(score) {};
    };

    inline std::ostream &operator <<(std::ostream &out, const Match &match){
      return out << match.object->str() << ' ' << setprecision(4) << match.score;
    }

    class MatchGTComp{
    public:
      bool operator ()(const Match &m1, const Match &m2){
        return m1.score > m2.score;
      };
    };

    typedef std::vector<Match> Matches;

  }
}
