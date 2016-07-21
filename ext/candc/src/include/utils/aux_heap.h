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


template <typename _RandomAccessIterator>
inline void
decrease_key_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
                  _RandomAccessIterator __changed){

  typedef typename iterator_traits<_RandomAccessIterator>::value_type _ValueType;
  typedef typename iterator_traits<_RandomAccessIterator>::difference_type _DistanceType;

  if (__last - __first < 2 || __changed == __last)
    return;

  _DistanceType __len = __last - __first;
  _DistanceType __pos = __changed - __first;
  _DistanceType __child1 = 2*__pos + 1;
  _DistanceType __child2 = __child1 + 1;

  _ValueType value = *__changed;

  while(__child2 < __len){
    if(*(__first + __child1) <  *(__first + __child2)){
      if(value < *(__first + __child2)){
        *__changed = *(__first + __child2);
        __changed = (__first + __child2);
        __pos = __child2;
      }else
        break;
    }else{
      if(value < *(__first + __child1)){
        *__changed = *(__first + __child1);
        __changed = (__first + __child1);
        __pos = __child1;
      }else
        break;
    }
    __child1 = 2*__pos + 1;
    __child2 = __child1 + 1;
  }
  if(__child1 == __len - 1 && value < *(__first + __child1)){
    *__changed = *(__first + __child1);
    *(__first + __child1) = value;
  }else
    *__changed = value;
}

template <typename _RandomAccessIterator, typename _Compare>
inline void
decrease_key_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
                  _RandomAccessIterator __changed, _Compare __comp){

  typedef typename iterator_traits<_RandomAccessIterator>::value_type _ValueType;
  typedef typename iterator_traits<_RandomAccessIterator>::difference_type _DistanceType;

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
        __changed = (__first + __child2);
        __pos = __child2;
      }else
        break;
    }else{
      if(__comp(value, *(__first + __child1))){
        *__changed = *(__first + __child1);
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
    *(__first + __child1) = value;
  }else
    *__changed = value;
}

template <typename _RandomAccessIterator>
void 
increase_key_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
                  _RandomAccessIterator __changed){

  typedef typename iterator_traits<_RandomAccessIterator>::value_type _ValueType;
  typedef typename iterator_traits<_RandomAccessIterator>::difference_type _DistanceType;

  if (__last - __first < 2 || __changed == __first)
    return;

  _DistanceType __pos = __changed - __first;
  _ValueType value = *__changed;

  while(__pos != 0){
    _DistanceType __parent = (__pos - 1)/2;

    if(*(__first + __parent) <  value){
      *__changed = *(__first + __parent);
      __changed = (__first + __parent);
      __pos = __parent;
    }else
      break;
  }
  *__changed = value;
}

template <typename _RandomAccessIterator, typename _Compare>
void 
increase_key_heap(_RandomAccessIterator __first, _RandomAccessIterator __last,
                  _RandomAccessIterator __changed, _Compare __comp){

  typedef typename iterator_traits<_RandomAccessIterator>::value_type _ValueType;
  typedef typename iterator_traits<_RandomAccessIterator>::difference_type _DistanceType;

  if (__last - __first < 2 || __changed == __first)
    return;

  _DistanceType __pos = __changed - __first;

  _ValueType value = *__changed;

  while(__pos != 0){
    _DistanceType __parent = (__pos - 1)/2;

    if(__comp(*(__first + __parent), value)){
      *__changed = *(__first + __parent);
      __changed = (__first + __parent);
      __pos = __parent;
    }else
      break;
  }
  *__changed = value;
}

template <typename _RandomAccessIterator>
inline bool
check_heap(_RandomAccessIterator __first, _RandomAccessIterator __last){

  typedef typename iterator_traits<_RandomAccessIterator>::difference_type _DistanceType;

  _DistanceType __len = __last - __first;
  if(__len < 2)
    return true;

  _DistanceType __parent = (__len - 2)/2;
  _DistanceType __child1 = 2*__parent + 1;
  _DistanceType __child2 = 2*__parent + 2;

  if(*(__first + __parent) < *(__first + __child1))
    return false;

  if(__child2 <= __len)
    if(*(__first + __parent) < *(__first + __child2))
       return false;
  __parent--;
  while(__parent > 0){
    __child1 = 2*__parent + 1;
    __child2 = 2*__parent + 2;

    if(*(__first + __parent) < *(__first + __child1))
      return false;
    if(*(__first + __parent) < *(__first + __child2))
      return false;

    __parent--;
  }

  return true;
}

template <typename _RandomAccessIterator, typename _Compare>
inline bool
check_heap(_RandomAccessIterator __first, _RandomAccessIterator __last, _Compare __comp){

  typedef typename iterator_traits<_RandomAccessIterator>::difference_type _DistanceType;

  _DistanceType __len = __last - __first;

  if(__len < 2)
    return true;

  _DistanceType __parent = (__len - 2)/2;
  _DistanceType __child1 = 2*__parent + 1;
  _DistanceType __child2 = 2*__parent + 2;

  if(__comp(*(__first + __parent), *(__first + __child1))){
    fprintf(stderr, "parent %d (%d) cmp child1 %d (%d)\n",
            __parent, *(__first + __parent), __child1, *(__first + __child1));
    return false;
  }

  if(__child2 < __len)
    if(__comp(*(__first + __parent), *(__first + __child2))){
       fprintf(stderr, "parent %d (%d) cmp child2 %d (%d)\n",
               __parent, *(__first + __parent), __child2, *(__first + __child2));
       return false;
    }
  __parent--;
  while(__parent > 0){
    __child1 = 2*__parent + 1;
    __child2 = 2*__parent + 2;

    if(__comp(*(__first + __parent), *(__first + __child1))){
      fprintf(stderr, "parent %d (%d) cmp child1 %d (%d)\n",
              __parent, *(__first + __parent), __child1, *(__first + __child1));
      return false;
    }
    if(__comp(*(__first + __parent), *(__first + __child2))){
     fprintf(stderr, "parent %d (%d) cmp child2 %d (%d)\n",
             __parent, *(__first + __parent), __child2, *(__first + __child2));
      return false;
    }

    __parent--;
  }

  return true;
}
