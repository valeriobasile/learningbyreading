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
    class Perceptron: public GIS {
    public:
      const bool SHUFFLE;

      Perceptron(const Model::Model &cfg, bool verbose);
      ~Perceptron(void);

      void perceptron_update(Context *context, ushort klass, ulong cur_iter, double delta);
      bool perceptron_iteration(void);
      ulong perceptron_iteration(PDF &p_classes, ulong cur_iter);
      void perceptron_dump(ulong cur_iter);
      void perceptron_finalise(ulong nupdates);

      virtual void init(void);
      virtual bool iteration(void){return false; }
      virtual void iterate(void);
    private:
      void send(const ulong &errors, const ulong &cur_iter, const ulong &dest);
      void recv(ulong &errors, ulong &cur_iter, ulong &src);
      void determine_order(ulong &first, ulong &last, ulong& src, ulong& dest);
    };
  }
}
