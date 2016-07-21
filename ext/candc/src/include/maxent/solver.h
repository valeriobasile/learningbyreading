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

    typedef doubles Buffer;

    inline double
    norm(const doubles &vec){
      double sum = 0.0;
      for(ulong i = 0; i < vec.size(); ++i)
        sum += fabs(vec[i]);
      return sum;
    }

    inline double
    dot(doubles &x, doubles &y){
      return std::inner_product(&x[0], &x[x.size()], &y[0], 0.0);
    }

    inline bool
    is_zero(const doubles &vec, const double EPSILON){
      for(ulong i = 0; i < vec.size(); ++i)
        if(fabs(vec[i]) > EPSILON)
          return false;
      return true;
    }

    static const double C1 = 0.0001;     // tolerance of decrease of function value in line search
    static const double C2 = 0.9;        // tolerance of decrease of gradient in line search
    static const int SEARCH_NITER = 100; // maximum number of iterations of line search

    template <typename Objective>
    class Solver {
    public:
      const double EPSILON;                // machine precision
      size_t NVARIABLES;                   // number of variables (in our case features)
      size_t NHISTORY;                           // the size of the history to create

      Objective &func;

      Gradient grad;                       // current gradient of the function
      Gradient old_grad;               // previous gradient of the function

      doubles diff_grad;            // difference between current and prev gradients 

      Direction dir;                // direction to minimize

      std::deque<Direction> dirs;        // Set of differences of input vectors
      std::deque<doubles> diff_grads;        // Set of differences of gradients

      std::deque<double> rho;        // Set of 1 on products of s and y
      doubles alpha;                        // temporary storage

      bool is_beginning;                // is the beginning of optimization?
      bool has_converged;                // has the minimisation converged?

      double value;                        // current function value

      const bool REPORT;
    public:
      Solver(Objective &func, size_t nvars, size_t nhistory, double epsilon, bool REPORT = false)
        : EPSILON(epsilon), NVARIABLES(nvars), NHISTORY(nhistory), func(func),
          grad(NVARIABLES), old_grad(NVARIABLES), diff_grad(NVARIABLES), dir(NVARIABLES),
          alpha(NHISTORY), is_beginning(true), has_converged(false), value(0.0),
          REPORT(REPORT){}

      ~Solver(void){ /* do nothing */ }

      void remember(std::deque<doubles> &hist, doubles &vec) const {
        if(hist.size() == NHISTORY)
          hist.pop_front();
        hist.push_back(vec);
      }

      void remember(std::deque<double> &hist, double value) const {
        if(hist.size() == NHISTORY)
          hist.pop_front();
        hist.push_back(value);
      }

      void zoom_search(double lo_alpha, double hi_alpha,
                       const double zero_value, const double zero_del_phi,
                       double lo_value, double hi_value,
                       double lo_del_phi, double hi_del_phi,
                       double old_alpha){

        const double SUFF_DEC = C1*zero_del_phi;
        const double CURVE_COND = -C2*zero_del_phi;

        double new_alpha;
        double new_value;
        double new_del_phi;
        double old_value = 0;

        for(int i = 0; i < SEARCH_NITER; ++i){

          new_alpha = (hi_alpha + lo_alpha)*0.5;
          double diff_alpha = hi_alpha - lo_alpha;

          if(REPORT && (!Cluster::USE_MPI || Cluster::rank == 0))
            cout << ", zoom" << flush;

          func.update(dir, new_alpha - old_alpha);
          new_value = func.compute(grad);
          new_del_phi = dot(grad, dir);

          if(REPORT && (!Cluster::USE_MPI || Cluster::rank == 0)){
            cout << "\nalpha = " << new_alpha << ", phi(alpha) = " << new_value;
            cout << ", phi'(alpha) = " << new_del_phi;
            cout << ", COND = " << CURVE_COND << endl;
          }

          old_alpha = new_alpha;

          // phi(alpha_j) > phi(0) + c_1 * alpha_j * phi'(0)
          if(new_value > zero_value + new_alpha*SUFF_DEC ||
             // phi(alpha_j) >= phi(alpha_lo) 
             (new_value >= lo_value)){
            hi_alpha = new_alpha;
            hi_value = new_value;
            hi_del_phi = new_del_phi;
            old_value = new_value;
            continue;
          }

          // Condition 2: Curvature condition
          // or if the values have converged
          if(fabs(new_del_phi) <= CURVE_COND || converged(new_value, old_value)){
            // we are finished

            // update the direction
            if(new_alpha != 1.0)
              dir *= new_alpha;

            // check for convergence of the function value
            has_converged = converged(value, new_value);

            value = new_value;

            return;
          }

          if(new_del_phi*diff_alpha >= 0.0){
            hi_alpha = lo_alpha;
            hi_value = lo_value;
            hi_del_phi = lo_del_phi;
          }

          lo_alpha = new_alpha;
          lo_value = new_value;
          lo_del_phi = new_del_phi;

          old_value = new_value;
        }

        throw NLP::Exception("zoom_search did not terminate");
      }

      void line_search(void){
        const double zero_value = value;
        const double zero_del_phi = dot(grad, dir);

        if(zero_del_phi >= 0.0)
          throw NLP::Exception("direction is not descent");

        const double SUFF_DEC = C1*zero_del_phi;
        const double CURVE_COND = -C2*zero_del_phi;

        double old_alpha = 0.0;
        double old_value = zero_value;
        double old_del_phi = zero_del_phi;


        if(REPORT && (!Cluster::USE_MPI || Cluster::rank == 0))
          cout << "search" << endl << flush;
        double new_alpha = 1.0;
        func.update(dir, new_alpha - old_alpha);
        double new_value = func.compute(grad);
        double new_del_phi = dot(grad, dir);

        for(int i = 0; i < SEARCH_NITER; ++i){
          // Condition 1: Sufficient decrease

          // phi(alpha_i) > phi(0) + c_1 * alpha_i * phi'(0)
          if(new_value > zero_value + new_alpha*SUFF_DEC ||
             // phi(alpha_i) >= phi(alpha_i-1) && i > 1 
             (i && new_value >= old_value)){
            return zoom_search(old_alpha, new_alpha, zero_value, zero_del_phi,
                               old_value, new_value, old_del_phi, new_del_phi, new_alpha);
          }

          // Condition 2: Curvature condition
          if(fabs(new_del_phi) <= CURVE_COND){
            // we are finished

            // update the direction
            if(new_alpha != 1.0)
              dir *= new_alpha;

            // check for convergence of the function value
            has_converged = converged(value, new_value);

            value = new_value;
 
            return;
          }

          if(new_del_phi >= 0.0){
            return zoom_search(new_alpha, old_alpha, zero_value, zero_del_phi,
                               new_value, old_value, new_del_phi, old_del_phi, new_alpha);
          }

          if(REPORT && (!Cluster::USE_MPI || Cluster::rank == 0)){
            cout << ", alpha = " << new_alpha << ", phi(alpha) = " << new_value;
            cout << ", phi'(alpha) = " << new_del_phi;
            cout << ", COND = " << CURVE_COND << endl;
          }

          old_alpha = new_alpha;
          new_alpha *= 2.0;

          func.update(dir, new_alpha - old_alpha);

          if(REPORT && (!Cluster::USE_MPI || Cluster::rank == 0))
            cout << "grow" << flush;

          old_value = new_value;
          new_value = func.compute(grad);

          old_del_phi = new_del_phi;
          new_del_phi = dot(grad, dir);
        }

        throw NLP::Exception("line search did not terminate");
      }

    public:
      void init(void){
        is_beginning = true;
        has_converged = false;
        if(REPORT && (!Cluster::USE_MPI || Cluster::rank == 0))
          cout << "initialising BFGS solver" << endl;
        value = func.compute(grad);
      }

      void lbfgs_two_loop(double gamma){
        // taken from Nocedal and Wright, algorithm 9.1, p.225

        // q = del f_k
        dir = grad;

        // for i = k - 1, ..., k - m
        for(int i = dirs.size() - 1; i >= 0; --i){
          // alpha_i = rho_i * s^T_i . q
          alpha[i] = rho[i]*dot(dirs[i], dir);
          // q = q - alpha_i . y_i
          dir -= alpha[i]*diff_grads[i];
        }

        // r = H^0_k * q
        dir *= gamma;
        // for i = k - m, ..., k - 1
        for(ulong i = 0; i < dirs.size(); ++i){
          // beta = rho_i * y^T_i . r
          double beta = rho[i]*dot(diff_grads[i], dir);
          // r = r + s_i(alpha_i - beta)
          dir += dirs[i]*(alpha[i] - beta);
        }
      }

      void lbfgs(void){
        // taken from Nocedal and Wright, algorithm 9.2, p.226

        if(is_beginning){
          // function value and gradient have already been calculated
          dir = grad;
          dir /= norm(grad);
          is_beginning = false;
        }else{
          // sy = s^T_k-1 . y_k-1
          double sy = dot(dir, diff_grad);
          double gamma = sy/dot(diff_grad, diff_grad);

          // discard and save to update history for rho
          remember(rho, 1.0/sy);

          // compute H_k * del f_k 
          lbfgs_two_loop(gamma);
        }

        // p_k = -H_k * del f_k
        dir *= -1.0;

        // save del f_k
        old_grad = grad;

        // choose alpha_k (scale factor) to satisfy the Wolfe conditions
        // changes dir (by multiplying by alpha_k)
        // changes grad and value by recomputing function
        line_search();

        // calculating gamma_k using equation 9.6
        // y_k = del f_k+1 - del f_k
        diff_grad = grad;
        diff_grad -= old_grad;

        // discard and save to update history for s_k and y_k
        remember(dirs, dir);
        remember(diff_grads, diff_grad);
      }

      void iterate(bool VERBOSE, const ulong NITERATIONS){
        for(ulong iteration = 0; iteration < NITERATIONS; ++iteration){
          if(VERBOSE && (!Cluster::USE_MPI || Cluster::rank == 0))
            cout << iteration << ": obj = " << value << endl;

          if(has_converged || is_zero(grad, EPSILON)){
            if(VERBOSE && (!Cluster::USE_MPI || Cluster::rank == 0))
              cout << "BFGS has converged, objective = " << value << endl;
            return;
          }

          lbfgs();
          if(REPORT && (!Cluster::USE_MPI || Cluster::rank == 0))
            cout << endl;
        }
        if(VERBOSE && (!Cluster::USE_MPI || Cluster::rank == 0))
          cout << "BFGS did not converge" << endl;
      }

      bool iteration(void){
        if(is_zero(grad, EPSILON) || has_converged)
          return false;

        lbfgs();

        return true;
      }

    };

  }
}
