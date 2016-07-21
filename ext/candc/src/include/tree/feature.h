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

    class Feature {
    public:
      double lambda;        // feature weight in sum form
      double est;        // estimated feature expectation
      double emp;        // empirical feature expectation

      Feature(void): lambda(0.0), est(0.0), emp(0.0) {};
      Feature(double freq): lambda(0.0), est(0.0), emp(freq) {};
      ~Feature(void) {}

      void normalise(double invZ){ est *= invZ; };
      void update(double invC){
        if(est == 0.0)
          return;

        lambda += log(emp/est)*invC;
        est = 0.0;
      }
      void update_gaussian(double C, double alpha){
        if(est == 0.0)
          return;

        double old = 0.0;
        double delta = 0.1;
        for(unsigned int i=0; i<100; ++i){
          if(old == delta)
            break;
          old = delta;
          double expx = est*exp(C*delta);
          double f = expx - emp + alpha*(lambda + delta);
          double df = C*expx + alpha;
          delta -= f/df;
        }
        lambda += delta;

        est = 0.0;
      }

      // sc: put the emp check in
      double grad(void) { 
        if(emp != 0.0)
          return emp - est;
        return 0.0;
      };

      double grad_gaussian(double alpha) {
        /*
        if(emp >= 1000)
          alpha = 0.0;
        else
          alpha = alpha / (exp(emp / 100));
        */
        if(emp != 0.0)
          return emp - est - lambda*alpha;
        return 0.0;
      };

      double penalty_gaussian(double alpha) {
        /*
        if(emp >= 1000)
          alpha = 0.0;
        else
          alpha = alpha / (exp(emp / 100));
        */
        return 0.5*lambda*lambda*alpha; 
      };
    };

    typedef std::vector<Feature> Features;
  }
}
