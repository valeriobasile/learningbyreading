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

// NLP::MemoryArena and NLP::Pool
// very fast, efficient bulk memory allocation used in
// hash tables and other data structures with a large number
// of small objects

#include	<cstring>

namespace NLP {

  // wraps a large chunk of memory which is determined
  // when the chunk is created.  Chunks of memory are not
  // reallocated, instead another MemoryArena needs to be
  // created.
  class MemoryArena {
  protected:
    const size_t _SIZE;
    char *const _begin;
    char *const _end;

    char *_current;
    char *_mark;
  public:
    // creates an empty StringPool of given size
    MemoryArena(size_t size):
        _SIZE(size), _begin(new char[_SIZE]), _end(_begin + _SIZE),
        _current(_begin), _mark(_begin){};
    ~MemoryArena(void){ delete [] _begin; };

    char *alloc(size_t size){
      if(size + _current > _end)
        return 0;

      char *result = _current;
      _current += size;
      return result;
    };

    void mark(void){ _mark = _current; };
    void undo(void){ _current = _mark; };
    void clear(void) { _mark = _current = _begin; };

    size_t size(void) const { return _SIZE; };
    size_t used(void) const { return _current - _begin; };
    size_t unused(void) const { return _end - _current; };
  };

  typedef std::vector<MemoryArena *> MemoryArenas;

  // very fast memory allocation for bulk access
  // however, you cannot free individual objects
  // only free all objects simultaneously by resetting
  // the pool.  This functionality is often what is required
  // for classes such as hash tables which create lots of
  // rather small objects which all need to be destroyed at
  // the same time.
  class Pool{
  private:
    const ulong _MINSIZE;

    MemoryArenas _used;
    MemoryArenas _unused;

    MemoryArena *_current;
  public:
    Pool(size_t minsize):
        _MINSIZE(minsize), _current(new MemoryArena(_MINSIZE)) {};

    ~Pool(void) {
      delete _current;

      for(MemoryArenas::iterator i = _used.begin(); i != _used.end(); i++)
        delete *i;
      for(MemoryArenas::iterator i = _unused.begin(); i != _unused.end(); i++)
        delete *i;
    };

    // allocate size bytes from the Pool
    // get a new MemoryArena if we don't have size amount of
    // space in the current MemoryArena.
    char *alloc(size_t size){
      char *buf = _current->alloc(size);
      if(buf)
        return buf;

      if(size < _MINSIZE){
        _used.push_back(_current);

        if(!_unused.empty()){
          _current = _unused.back();
          _unused.pop_back();
        }else
          _current = new MemoryArena(_MINSIZE);
        buf = _current->alloc(size);
      }else{
        MemoryArena *tmp = new MemoryArena(size);
        buf = tmp->alloc(size);
        _used.push_back(tmp);
      }
      return buf;
    };

    // allocate space and copy
    void *dup(const void *mem, size_t size){
      void *buf = alloc(size);
      memmove(buf, mem, size);
      return buf;
    };

    // allocate space and copy, putting a '\0' on the end
    const char *strdup(const char *const str, size_t len){
      char *buf = alloc(len + 1);
      memmove(buf, str, len);
      buf[len] = '\0';
      return buf;
    };

    const char *strdup(const char *const str){
      return strdup(str, strlen(str));
    }

    // free all of the objects in the Pool
    // makes no attempt to call any destructors for these
    // objects, this needs to have been managed by the client code
    void clear(void) {
      _current->clear();

      _unused.insert(_unused.end(), _used.begin(), _used.end());
      for(MemoryArenas::iterator i = _used.begin(); i != _used.end(); ++i)
        (*i)->clear();

      _used.resize(0);
    }

    // support for rolling-back the last allocation (and only the last)
    void mark(void){ _current->mark(); };
    void undo(void){ _current->undo(); };

    // some Pool statistics
    size_t narenas(void) const { return _used.size(); }
    size_t minsize(void) const { return _MINSIZE; }

    size_t size(void) const {
      size_t nbytes = _current->size();
      for(MemoryArenas::const_iterator i = _used.begin(); i != _used.end(); ++i)
        nbytes += (*i)->size();
      for(MemoryArenas::const_iterator i = _unused.begin(); i != _unused.end(); ++i)
        nbytes += (*i)->size();
      return nbytes;
    }

    size_t used(void) const {
      size_t nbytes = _current->used();
      for(MemoryArenas::const_iterator i = _used.begin(); i != _used.end(); ++i)
        nbytes += (*i)->used();
      return nbytes;
    }

    void stats(std::ostream &os){
      size_t u = used();
      size_t b = size();
      os << "number of arenas " << narenas() << '\n';
      os << "minimum arena size " << _MINSIZE << '\n';
      os << "total memory  " << b << " bytes\n";
      os << "used memory   " << u << " bytes\n";
      os << "unused memory " << b - u << " bytes\n";
      os << "utilisation   " << std::setprecision(5) << u/(float)b*100.0 << "%\n";
    }
  };

}
