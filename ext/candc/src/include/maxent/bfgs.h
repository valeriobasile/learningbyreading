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
  namespace MaxEnt {

    typedef std::valarray<double> doubles;
    typedef doubles Gradient;
    typedef doubles Direction;

    template <typename T>
    class Solver;

    class BFGS: public GIS {
    private:
      Solver<BFGS> *_solver;
    public:
      BFGS(const Model::Model &cfg, bool verbose);
      ~BFGS(void);

      double llhood(void);
      double compute(Gradient &grad);
      void update(Direction &dir, double scale);

      virtual void init(void);
      virtual void iterate(void);
      virtual bool iteration(void);
    };

  }
}
