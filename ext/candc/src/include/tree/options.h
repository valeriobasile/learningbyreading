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

    class Options {
    public:
      struct Smoothing {
        public:
        const static ulong NONE = 0;
        const static ulong GAUSSIAN = 1;
        const static ulong GAMMA = 2;

        static ulong from_string(std::string str);
        static std::string to_string(ulong value);
      };
    public:
      std::string control;	// control filename

      std::string base;		// training file
      std::string model;	// GIS output directory
      std::string weights;	// GIS weights output
      ulong niterations;	// number of GIS iterations

      std::string info;		// info filename

      ulong nsentences;		// number of sentences in the data
      ulong nfeatures;		// number of features
      ulong nevents;		// number of events (tokens), i.e. # training instances
      double mactive;	        // mean number of active features
      ulong smoothing;		// apply smoothing: 0 none, 1 gaussian, 2 gamma

      bool gaussian(void) const { return smoothing == Smoothing::GAUSSIAN; }
      bool gamma(void) const { return smoothing == Smoothing::GAMMA; }

      //sc: added this
      bool norm_form;

      double alpha;		// smoothing constant = N/sigma^2 from Chen & Rosenfeld 1999

      Options(std::string filename, std::string weights);
      Options(std::string filename);
      ~Options(void) {};
    };

  }
}

