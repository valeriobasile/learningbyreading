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

// NLP::MaxEnt::GIS
// Generalised Iterative Scaling implementation

namespace NLP {
  namespace MaxEnt {

    class GIS {
    protected:
      // read in the model/features file to translate
      // attribute identifiers in each context to lists of features
      void _read_features(void);
      // read in the model/contexts file into a list of Context objects
      void _read_contexts(void);
      // calculate C the correction coefficient
      void _calc_correction(void);

      // calculate the maximum number of active features for a given context
      ulong _max_attributes(const Context *context, Actives &nactives);

      // sums the feature weights for a given context
      void _sum(const Context *context, PDF &p_classes);

      // normalise the PDF and scale it by the number of occurrences of this context
      void _normalise(const Context *context, PDF &p_classes);

      // calculate the model features expectations for a given context
      void _estimate(Context *context, PDF &p_classes);

      // calculate the current feature model expectations by
      // distributing the model's probability mass of the features
      double _distribute(PDF &p_classes);

      // sum estimates globally over all nodes in the cluster
      void _sum_estimates(void);

      // apply the update function on each attribute
      void _update(void);

      // apply the different types of update function on each feature
      void _update_none(void);
      void _update_gaussian(void);

      Features features;		// features (in attribute sorted order)
      Attributes attributes;		// attributes lookup
      Contexts contexts;		// contexts
    public:
      double C;			// maximum number of features (C or f#) (as a double)
      double invC;		// 1/C
      ulong tfeatures;		// total number of features (tokens)

      const Model::Model &model;
      Model::Info info;

      const ulong NKLASSES;
      const double ALPHA;
      const bool VERBOSE;
      double* global_est;
      double* local_est;

      GIS(const Model::Model &model, bool verbose);
      virtual ~GIS(void);

      void load_weights(const std::string &filename);

      std::string to_string(void) const;
      std::string profile(void) const;

      // initialise the solver
      virtual void init(void);

      // estimate the model
      virtual void iterate(void);

      // perform a single iteration
      virtual bool iteration(void);

      // dump the feature weights into model/weights
      void save(const std::string &PREFACE);
    };

    inline std::ostream &operator<<(std::ostream &s, const GIS &gis){
      return s << gis.to_string();
    }

  }
}
