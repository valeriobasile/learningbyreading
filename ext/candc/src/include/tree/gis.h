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

    class GIS {
    protected:
      void _read_features(void);
      void _read_forests(void);
      void _calc_emp(void);
      void _calc_correction(void);
      void _update(void);
      void _normalise(double invZ);
      void _update_gaussian(void);
      void _reduce_counts(void);
      void _count_correct(void);
    public:
      const Options op;
      Stopwatch watch;

      std::string comment;

      Forests forests;
      Features features;		// all the features
      double *local_est;
      double *global_est;
      ulong tfeatures;			// total number of features (tokens)

      double C;                     // maximum number of features
      double invC;                  // 1/C

      ulong niterations;

      //sc: added this - needed for perceptron
      long nforests;
      long nread;
      long nreject;

      GIS(std::string filename, bool load_forests = true);
      ~GIS(void);

      void profile(void);
      void iterate(void);
      void save(void);
      void save(long iteration);
      void stats(void);
    };

  }
}
