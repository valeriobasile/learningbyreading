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
  namespace Taggers {

    // fast memory allocation and caching of Node objects
    template <class Node>
    class NodePool: public Pool {
    private:
      Node *_free;
    public:
      NodePool(void): Pool(512*1024), _free(0) {};

      void *alloc(size_t size){
        if(_free){
          Node *top = _free;
          _free = const_cast<Node *>(_free->prev);
          return top;
        }else
          return (void *)Pool::alloc(size);
      };

      void free(Node *){
	//        node->prev = _free;
        //_free = node;
      };

      void clear(void){
        _free = 0;
        Pool::clear();
      };
    };

  }
}
