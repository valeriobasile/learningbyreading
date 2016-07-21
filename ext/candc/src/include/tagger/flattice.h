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

// full lattice. Keeps all sequences allowed by the tag dictionaries

namespace NLP {
  namespace Taggers {
    
    class FNode {
    public:
      FNode *prev;         // node of previous tag
      NLP::Tag id;           // current tag
      NLP::Tag pid;           // cached id of previous tag
      double forward;
      double backward;

      void *operator new(size_t size, NodePool<FNode> *pool) { return pool->alloc(size); }
      void operator delete(void *, NodePool<FNode> *) { /* do nothing */ }
      
      FNode(FNode *prev, NLP::Tag id, NLP::Tag pid, double forward, double backward):
        prev(prev), id(id), pid(pid), forward(forward), backward(backward) {};
      
      ~FNode(void){ /* do nothing */ };
    };

    typedef std::vector<FNode *> FNodes;

    class Flattice {
    public:
      typedef offset_vector<FNodes, 1, 0> Lattice;
      typedef offset_vector<ulong, 2, 0> Permitted;

      NodePool<FNode> *pool;

      Lattice lattice;
      Permitted permitted;
      std::vector<PDF> PDFs;
      FNode start;
      
      Flattice(const ulong MAXWORDS, const ulong nklasses)
        : pool(new NodePool<FNode>()),
          start(0, NLP::SENTINEL, NLP::SENTINEL, 1.0, 0.0){

        lattice.reserve(MAXWORDS + 1);
        lattice.push_back(FNodes(1, &start));

        for(ulong i = 0; i < MAXWORDS; ++i){
          FNodes nodes;
          nodes.reserve(nklasses);
          lattice.push_back(nodes);
        }

        permitted.reserve(MAXWORDS);
        permitted.push_back(1);
        permitted.push_back(1);
      }

      ~Flattice(void){
        delete pool;
      }

      FNode *create(FNode *prev, NLP::Tag pid, NLP::Tag id, double forward, double backward){
        return new (pool) FNode(prev, pid, id, forward, backward);
      };

      void free(FNode *node){ pool->free(node); };

      void reset(void){  // don't clear start node
        pool->clear();

        for(Lattice::iterator i = lattice.begin(); i != lattice.end_buffer(); ++i)
          i->resize(0);

        permitted.resize(0);
        PDFs.resize(0);
      }

      FNodes &operator[](ulong i){ return lattice[i]; }
      std::vector<FNodes>::iterator begin(){ return lattice.begin(); }
      std::vector<FNodes>::iterator end(){ return lattice.end(); }
      ulong size(){ return lattice.size(); }

    };

  }
}
