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

#include <cstring>

namespace NLP {
  namespace Cluster {

    extern ulong rank;
    extern ulong size;
    extern std::string rank_str;
    extern std::string size_str;
    extern std::string processor;
    extern bool USE_MPI;

    void init(int &argc, char **&argv, bool mpi);
    void finalize(void);
    void barrier(void);

    void send(const std::vector<double> &vals, int dest, int tag);
    void send(const std::vector<ulong> &vals, int dest, int tag);
    void send(const std::vector<long> &vals, int dest, int tag);
    void send(const std::vector<int> &vals, int dest, int tag);
    void send(const std::string &s, int dest, int tag);

    inline void send(const ulong &val, int dest, int tag){
      std::vector<ulong> vals(1, val);
      send(vals, dest, tag);
    }
    
    void recv(std::vector<double> &vals, int src, int tag);
    void recv(std::vector<ulong> &vals, int src, int tag);
    void recv(std::vector<long> &vals, int src, int tag);
    void recv(std::vector<int> &vals, int src, int tag);
    void recv(std::string &s, int src, int tag);

    inline void recv(ulong &val, int dest, int tag){
      std::vector<ulong> vals(1, val);
      recv(vals, dest, tag);
      val = vals[0];
    }

    void sum(double *local_vals, double *global_vals, int len_vals);
    inline void sum(double &val){
      double local_val = val;
      sum(&local_val, &val, 1);
    }

    void sum(ulong *local_vals, ulong *global_vals, int len_vals);
    inline void sum(ulong &val){
      ulong local_val = val;
      sum(&local_val, &val, 1);
    }

    void max(ulong *local_vals, ulong *global_vals, int len_vals);
    inline void max(ulong &val){
      ulong local_val = val;
      max(&local_val, &val, 1);
    }

    typedef std::vector<std::pair<std::string, ulong> > Counts;
    void bcast(Counts &counts);
  
    class KeyValue {
    public:
      void add(const char *key, int keylen, const char *value, int valuelen);

      template <class Value>
      void add(const char *key, int keylen, const Value &value){
        add(key, keylen, reinterpret_cast<char *>(const_cast<Value *>(&value)),
	    sizeof(Value));
      }

      template <class Value>
      void add(const char *key, const Value &value){
        add(key, strlen(key) + 1, value);
      }
      
      template <class Value>
      void add(const std::string &key, const Value &value){
        add(key.c_str(), key.size() + 1, value);
      }
    };

    typedef void (*LoadFunction)(int, KeyValue *, void *);
    typedef void (*ReduceFunction)(char *, int, char *, int, int *,
        KeyValue *, void *);
    typedef void (*MapUpdate)(int, char *, int, char *, int, KeyValue *, 
        void *);

    class MapReduce {
    public: 
      void *_impl;

      MapReduce(void);
      ~MapReduce(void);
      
      KeyValue *kv(void);

      int map(int nmap, LoadFunction func, void *ptr);
      int map(KeyValue *kv, MapUpdate func, void *ptr);
      int collate(void);
      int reduce(ReduceFunction func, void *ptr);
      int gather(int nprocs);

      void add(KeyValue *kv);
    };

  }
}
