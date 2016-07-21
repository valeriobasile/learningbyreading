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

    typedef std::valarray<double> Gradient;
    typedef std::valarray<double> Direction;
    typedef std::valarray<double> Buffer;

    class BFGS: public GIS {
    private:
      class _Solver;
      _Solver *_solver;
    public:
      BFGS(std::string filename);
      ~BFGS(void);

      Buffer local;
      Buffer global;

      double llhood(void);
      double compute(Gradient &grad);
      void update(Direction &dir, double scale);

      void iterate(void);
      bool iteration(void);
    };

  }
}
