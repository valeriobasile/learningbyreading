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

// NLP::MaxEnt::Feature
// representation for features and also the GIS update functions
// used in the maximum entropy estimation code


namespace NLP {
  namespace MaxEnt {

    inline bool
    converged(double a, double b, const double EPSILON = 1e-6){
      return b != 0.0 && fabs(1.0 - fabs(a/b)) <= EPSILON;
    }

    class Feature {
    public:
      double lambda;	// feature weight (may be product or sum form)
      double est;	// total estimated feature expectation
      long emp;		// total empirical feature expectation
      ulong klass;	// class index

      Feature(double initial, ulong klass, long emp):
          lambda(initial), est(0.0), emp(emp), klass(klass){}
      ~Feature(void){}

      // apply GIS update rule and reset the estimated expectation
      // the representation can be either in product or sum (log) form
      void update_product(double invC){
        lambda *= pow(emp/est, invC);
        est = 0.0;
      }

      void update_sum(double invC){
        lambda += log(emp/est)*invC;
        est = 0.0;
      }

      // Gaussian smoothing update rule, where alpha is the smoothing parameter
      void update_gaussian(double C, double alpha){
        double old = 0.0;
        double delta = 0.1;
        double demp = emp;

        // update rule when Gaussian smoothing is on - there is no
        // closed-form solution which finds the update delta, so this
	// uses a Newton Raphson iterative solver to find the update

        for(ulong i = 0; i < 100; ++i){
          if(old == delta)
            break;
          old = delta;
          double expx = est*exp(C*delta);
          double f = expx - demp + alpha*(lambda + delta);
          double df = C*expx + alpha;
          delta -= f/df;
        }

        lambda += delta;
        est = 0.0;
      }

      // methods needed for L-BFGS optimisation
      double grad(void) { return emp - est; }
      double grad_gaussian(double alpha) { return emp - est - lambda*alpha; }
      double penalty_gaussian(double alpha) { return 0.5*lambda*lambda*alpha; }
    };

    typedef std::vector<Feature> Features;
  }
}
