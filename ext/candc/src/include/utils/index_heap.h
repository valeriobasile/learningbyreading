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


template <typename _RandomAccessIterator, typename _IndexIterator, typename _Compare>
inline void 
make_iheap(_RandomAccessIterator __first, _RandomAccessIterator __last,
           _IndexIterator __index, _Compare __comp)
{
  make_heap(__first, __last, __comp);
  for(_RandomAccessIterator i = __first; i != __last; i++)
    __index[(*i)->index] = i - __first;
}

template <typename _RandomAccessIterator, typename _IndexIterator,
          typename _DistanceType, typename _ValueType, typename _Compare>
inline void
__push_iheap(_RandomAccessIterator __first, _IndexIterator __index, _DistanceType __holeIndex,
             _DistanceType __topIndex, _ValueType __value, _Compare __comp)
{
  _DistanceType __parent = (__holeIndex - 1) / 2;
  while (__holeIndex > __topIndex && __comp(*(__first + __parent), __value)) {
    *(__first + __holeIndex) = *(__first + __parent);
    *(__index + (*(__first + __holeIndex))->index) = __holeIndex;
    __holeIndex = __parent;
    __parent = (__holeIndex - 1) / 2;
  }
  *(__first + __holeIndex) = __value;
  *(__index + __value->index) = __holeIndex;
}

template <typename _RandomAccessIterator, typename _IndexIterator, typename _Compare>
inline void 
push_iheap(_RandomAccessIterator __first, _RandomAccessIterator __last,
          _IndexIterator __index, _Compare __comp)
{
  typedef typename iterator_traits<_RandomAccessIterator>::value_type _ValueType;
  typedef typename iterator_traits<_RandomAccessIterator>::difference_type  _DistanceType;

  __push_iheap(__first, __index, _DistanceType((__last - __first) - 1), _DistanceType(0),
               _ValueType(*(__last - 1)), __comp);
}

template <typename _RandomAccessIterator, typename _IndexIterator, typename _DistanceType,
          typename _ValueType, typename _Compare>
void
__adjust_iheap(_RandomAccessIterator __first, _IndexIterator __index, _DistanceType __holeIndex,
              _DistanceType __len, _ValueType __value, _Compare __comp)
{
  _DistanceType __topIndex = __holeIndex;
  _DistanceType __secondChild = 2 * __holeIndex + 2;
  while (__secondChild < __len) {
    if (__comp(*(__first + __secondChild), *(__first + (__secondChild - 1))))
      __secondChild--;
    *(__first + __holeIndex) = *(__first + __secondChild);
    *(__index + (*(__first + __holeIndex))->index) = __holeIndex;
    
    __holeIndex = __secondChild;
    __secondChild = 2 * (__secondChild + 1);
  }
  if (__secondChild == __len) {
    *(__first + __holeIndex) = *(__first + (__secondChild - 1));
    *(__index + (*(__first + __holeIndex))->index) = __holeIndex;
    __holeIndex = __secondChild - 1;
  }
  __push_iheap(__first, __index, __holeIndex, __topIndex, __value, __comp);
}

template <typename _RandomAccessIterator, typename _IndexIterator,
          typename _ValueType, typename _Compare>
inline void
__pop_iheap(_RandomAccessIterator __first, _RandomAccessIterator __last,
            _IndexIterator __index,  _RandomAccessIterator __result,
            _ValueType __value, _Compare __comp)
{
  typedef typename iterator_traits<_RandomAccessIterator>::difference_type  _DistanceType;

  *__result = *__first;
  __index[(*__first)->index] = ULONG_MAX;
  __adjust_iheap(__first, __index, _DistanceType(0), _DistanceType(__last - __first), 
                __value, __comp);
}

template <typename _RandomAccessIterator, typename _IndexIterator, typename _Compare>
inline void 
pop_iheap(_RandomAccessIterator __first, _RandomAccessIterator __last,
          _IndexIterator __index, _Compare __comp)
{
  typedef typename iterator_traits<_RandomAccessIterator>::value_type _ValueType;

  __pop_iheap(__first, __last - 1, __index, __last - 1, _ValueType(*(__last - 1)), __comp);
}

template <typename _RandomAccessIterator, typename _IndexIterator, typename _Compare>
inline void 
decrease_key_iheap(_RandomAccessIterator __first, _RandomAccessIterator __last,
                   _IndexIterator __index, _RandomAccessIterator __changed, _Compare __comp){

  typedef typename iterator_traits<_RandomAccessIterator>::difference_type  _DistanceType;
  typedef typename iterator_traits<_RandomAccessIterator>::value_type _ValueType;

  if (__last - __first < 2 || __changed == __last)
    return;

  _DistanceType __len = __last - __first;
  _DistanceType __pos = __changed - __first;
  _DistanceType __child1 = 2*__pos + 1;
  _DistanceType __child2 = __child1 + 1;

  _ValueType value = *__changed;

  while(__child2 < __len){
    if(__comp(*(__first + __child1), *(__first + __child2))){
      if(__comp(value, *(__first + __child2))){
        *__changed = *(__first + __child2);
        *(__index + (*__changed)->index) = __pos;
        __changed = (__first + __child2);
        __pos = __child2;
      }else
        break;
    }else{
      if(__comp(value, *(__first + __child1))){
        *__changed = *(__first + __child1);
        *(__index + (*__changed)->index) = __pos;
        __changed = (__first + __child1);
        __pos = __child1;
      }else
        break;
    }
    __child1 = 2*__pos + 1;
    __child2 = __child1 + 1;
  }
  if(__child1 == __len - 1 && __comp(value, *(__first + __child1))){
    *__changed = *(__first + __child1);
    *(__index + (*__changed)->index) = __pos;
    *(__first + __child1) = value;
    *(__index + (*(__first + __child1))->index) = __len - 1;
  }else{
    *__changed = value;
    *(__index + (*__changed)->index) = __pos;
  }
}

template <typename _RandomAccessIterator, typename _IndexIterator, typename _Compare>
inline void 
increase_key_iheap(_RandomAccessIterator __first, _RandomAccessIterator __last,
                  _IndexIterator __index, _RandomAccessIterator __changed, _Compare __comp){

  typedef typename iterator_traits<_RandomAccessIterator>::difference_type  _DistanceType;
  typedef typename iterator_traits<_RandomAccessIterator>::value_type _ValueType;

  if (__last - __first < 2 || __changed == __first)
    return;

  _DistanceType __pos = __changed - __first;

  _ValueType value = *__changed;

  while(__pos != 0){
    _DistanceType __parent = (__pos - 1)/2;

    if(__comp(*(__first + __parent), value)){
      *__changed = *(__first + __parent);
      *(__index + (*__changed)->index) = __pos;
      __changed = (__first + __parent);
      __pos = __parent;
    }else
      break;
  }
  *__changed = value;
  *(__index + (*__changed)->index) = __pos;
}

template<typename T, typename Compare>
class index_heap {
// fast search heap for types which have index values
private:
  vector<T *> _heap;
  vector<ulong> _index;
  Compare _cmp;
  const static ulong NONMEMBER = ULONG_MAX;
public:
  index_heap(const vector<T *> &heap, Compare cmp):
    _heap(heap), _index(heap.size()), _cmp(cmp) {
    assert(_heap.size() < NONMEMBER);
    for(ulong i = 0; i < _index.size(); i++)
      _index[i] = NONMEMBER;
    make_iheap(_heap.begin(), _heap.end(), _index.begin(), _cmp);
    for(ulong i = 0; i < _index.size(); i++)
      assert(_index[i] != NONMEMBER);
  };
  ~index_heap(void) {};

  void restart(vector<T *> &heap){
    _heap = heap;
    _index.resize(_heap.size());
    for(ulong i = 0; i < _index.size(); i++)
      _index[i] = NONMEMBER;
    make_iheap(_heap.begin(), _heap.end(), _index.begin(), _cmp);
    for(ulong i = 0; i < _index.size(); i++)
      assert(_index[i] != NONMEMBER);
  };

  void push(T *elem){
    push_iheap(_heap.begin(), _heap.end(), _index.begin(), elem, _cmp);
  };

  void pop(void){
    pop_iheap(_heap.begin(), _heap.end(), _index.begin(), _cmp);
    _index[_heap.back()->index] = NONMEMBER;
    _heap.pop_back();
  };

  T *front(void) { return _heap.front(); };

  bool empty(void){ return _heap.size() == 0; };

  bool member(T *elem){
    return _index[elem->index] != NONMEMBER;
  };

  void decrease(T *elem){
    ulong index = _index[elem->index];
    assert(index != NONMEMBER);
    decrease_key_iheap(_heap.begin(), _heap.end(), _index.begin(), _heap.begin() + index, _cmp);
  };
  void increase(T *elem){
    ulong index = _index[elem->index];
    assert(index != NONMEMBER);
    increase_key_iheap(_heap.begin(), _heap.end(), _index.begin(), _heap.begin() + index, _cmp);
  };
};
