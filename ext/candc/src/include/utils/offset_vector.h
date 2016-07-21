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

  template <class T, int BEFORE, int AFTER, class Alloc = std::allocator<T> >
  class offset_vector {
  private:
    typedef std::vector<T, Alloc> impl_type;
    impl_type _impl;

    typedef offset_vector<T, BEFORE, AFTER, Alloc> self_type;
  public:
    typedef T value_type;
    typedef value_type *pointer;
    typedef const value_type *const_pointer;

    typedef typename impl_type::iterator iterator;
    typedef typename impl_type::const_iterator const_iterator;

    typedef value_type &reference;
    typedef const value_type &const_reference;

    typedef typename impl_type::difference_type size_type;
    typedef typename impl_type::difference_type difference_type;

    typedef typename impl_type::allocator_type allocator_type;

    typedef typename impl_type::reverse_iterator reverse_iterator;
    typedef typename impl_type::const_reverse_iterator const_reverse_iterator;

    static const size_type before_size = BEFORE;
    static const size_type after_size = AFTER;
    static const size_type padding_size = BEFORE + AFTER;

    iterator begin(void) { return _impl.begin() + BEFORE; }
    const_iterator begin(void) const { return _impl.begin() + BEFORE; }

    iterator begin_buffer(void) { return _impl.begin(); }
    const_iterator begin_buffer(void) const { return _impl.begin(); }

    iterator end(void) { return _impl.end() - after_size; }
    const_iterator end(void) const { return _impl.end() - after_size; }

    iterator end_buffer(void) { return _impl.end(); }
    const_iterator end_buffer(void) const { return _impl.end(); }

    reverse_iterator rbegin(void) { return _impl.rbegin() + after_size; }
    const_reverse_iterator rbegin(void) const {
      return _impl.rbegin() + after_size;
    }

    reverse_iterator rend(void) { return _impl.rend() - BEFORE; }
    const_reverse_iterator rend(void) const {
      return _impl.rend() - BEFORE;
    }

    size_type size(void) const {
      return static_cast<size_type>(_impl.size()) - padding_size;
    }
    size_type size_buffer(void) const {
      return static_cast<size_type>(_impl.size());
    }
    size_type capacity(void) const {
      return static_cast<size_type>(_impl.capacity()) - padding_size;
    }

    size_type empty(void) const { return begin() == end(); }

    reference operator [](size_type n) { return _impl[n + BEFORE]; }
    const_reference operator [](size_type n) const { return _impl[n + BEFORE]; }

    reference at(size_type n) { return _impl.at(n + BEFORE); }
    const_reference at(size_type n) const { return _impl.at(n + BEFORE); }

    explicit offset_vector(const allocator_type a = allocator_type())
      : _impl(a) {}

    offset_vector(size_type n, const_reference val,
		  const allocator_type a = allocator_type())
      : _impl(n + padding_size, val, a) {}

    explicit offset_vector(size_type n)
      : _impl(n + padding_size) {}

    offset_vector(const self_type &other)
      : _impl(other._impl) {}

    void reserve(size_type n) { _impl.reserve(n + padding_size); }
 
    void assign(size_type n, const value_type &val){
      _impl.assign(n + padding_size, val);
    }

    reference front(void) { return *begin(); }
    reference front_buffer(void) { return *_impl.begin(); }

    const_reference front(void) const { return *begin(); }
    const_reference front_buffer(void) const { return *_impl.begin(); }

    reference back(void) { return *(end() - 1); }
    reference back_buffer(void) { return *(_impl.end() - 1); }

    const_reference back(void) const { return *(end() - 1); }
    const_reference back_buffer(void) const { return *(_impl.end() - 1); }

    void push_back(const value_type &val) { _impl.push_back(val); }
    void swap(self_type &other) { _impl.swap(other._impl); }
    void pop_back(void) { _impl.pop_back(); }

    iterator insert(iterator pos) { _impl.insert(pos); }
    iterator insert(iterator pos, const value_type &val){
      _impl.insert(pos, val);
    }
    iterator insert(iterator pos, size_type n, const value_type &val){
      _impl.insert(pos, n, val);
    }

    iterator erase(iterator pos) { _impl.erase(pos); }
    iterator erase(iterator first, iterator last) { _impl.erase(first, last); }

    void resize(size_type n){ _impl.resize(n + padding_size); }
    void resize_buffer(size_type n){ _impl.resize(n); }

    void resize(size_type n, const value_type &val){
      _impl.resize(n + padding_size, val);
    }
    void resize_buffer(size_type n, const value_type &val){
      _impl.resize(n, val);
    }

    void clear(void) { _impl.clear(); }

    void pad_front(const value_type &val){
      _impl.insert(_impl.begin(), before_size, val);
    }

    void pad_back(const value_type &val){
      _impl.insert(_impl.end(), after_size, val);
    }
  };

}
