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

// NLP::Tagger::Node, NLP::Tagger::NodePool and NLP::Tagger::NodeMatrix
// these classes implement the representation of the lattice for the
// Beam search

namespace NLP {
  namespace Taggers {

    // the Node class is a linked list of tags and their scores
    // it also caches the previous tag so we don't have to follow
    // the pointer along each time

    // this would have to be changed if we wanted to handle
    // longer Markov windows
    class Node {
    public:
      const Node *prev;        // best node in the previous column of the lattice
      NLP::Tag pid;        // cached best previous tag
      NLP::Tag id;        // current tag
      float sum;        // current score

      void *operator new(size_t size, NodePool<Node> *pool) { return pool->alloc(size); }
      void operator delete(void *, NodePool<Node> *) { /* do nothing */ }

      Node(const Node *prev, NLP::Tag pid, NLP::Tag id, float sum):
        prev(prev), pid(pid), id(id), sum(sum) {}

      // if we have found a new best previous node
      // replace the existing link and score
      void update(const Node *prev, float sum){
        this->prev = prev;
        this->pid = prev->id;
        this->sum = sum;
      }
    };

    typedef std::vector<Node *> Lattice;

    // used to select the highest scoring path ending in a specific pair of tags
    // i.e. it eliminates paths which can no longer be the most probable
    // because a path with the same tags in the window has been found with a
    // higher score
    class NodeMatrix {
    public:
      const ulong nklasses;
      const ulong nklasses2;
      NodePool<Node> *pool;
      Lattice nodes;
      Lattice used;

      NodeMatrix(ulong nklasses):
          nklasses(nklasses), nklasses2(nklasses*nklasses),
          pool(new NodePool<Node>()), nodes(nklasses2) {
        used.reserve(nklasses2);
      }
      ~NodeMatrix(void) { delete pool; }

      Node *&get(NLP::Tag pid, NLP::Tag id) { return nodes[pid.value()*nklasses + id.value()]; }
      Node *&operator()(NLP::Tag pid, NLP::Tag id) { return get(pid, id); }

      void clear(void) {
        for(Lattice::iterator i = used.begin(); i != used.end(); ++i)
          get((*i)->pid, (*i)->id) = 0;
      }

      Lattice::iterator begin(void) { return used.begin(); }
      Lattice::iterator end(void) { return used.end(); }

      Lattice::const_iterator begin(void) const { return used.begin(); }
      Lattice::const_iterator end(void) const { return used.end(); }

      Node *create(const Node *prev, NLP::Tag pid, NLP::Tag id, float sum){
        return new (pool) Node(prev, pid, id, sum);
      }
      Node *start(void) { return create(0, NLP::SENTINEL, NLP::SENTINEL, 0.0); }

      void add(const Node *prev, NLP::Tag pid, NLP::Tag id, float sum){
        Node *&pos = get(pid, id);
        if(!pos){
          pos = create(prev, pid, id, sum);
          used.push_back(pos);
        }else if(sum > pos->sum)
          pos->update(prev, sum);
      }

      void free(Node *node){ pool->free(node); }
      void reset(void){ pool->clear(); }
    };

    class NodeGTCmp{
    public:
      bool operator()(const Node *n1, const Node *n2){ return n1->sum > n2->sum; }
    };

    class NodeLTCmp{
    public:
      bool operator()(const Node *n1, const Node *n2){ return n1->sum < n2->sum; }
    };

  }
}
