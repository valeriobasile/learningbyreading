// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <cassert>
#include <string>
#include <sstream>

#include "base.h"

#include "cluster.h"

namespace NLP {
  namespace Cluster {

    ulong rank = 0;
    ulong size = 1;
    std::string rank_str;
    std::string size_str;
    std::string processor;
    bool USE_MPI;

    void init(int &, char **&, bool mpi){
      USE_MPI = mpi;
      
      std::ostringstream out;
      out.str("");
      out << "Running on one machine (no MPI)";

      rank_str = std::string("0");
      size_str = std::string("1");
      processor = std::string("unknown - MPI not in use");
    }

    void finalize(void){}

    void barrier(void){}

    void send(const std::vector<double> &, int, int){
      assert(!"send called without mpi");
    }

    void send(const std::vector<ulong> &, int, int){
      assert(!"send called without mpi");
    }

    void send(const std::vector<long> &, int, int){
      assert(!"send called without mpi");
    }

    void send(const std::vector<int> &, int, int){
      assert(!"send called without mpi");
    }

    void send(const std::string &, int, int){
      assert(!"send called without mpi");
    }

    void recv(std::vector<double> &, int, int){
      assert(!"recv called without mpi");
    }

    void recv(std::vector<ulong> &, int, int){
      assert(!"recv called without mpi");
    }

    void recv(std::vector<long> &, int, int){
      assert(!"recv called without mpi");
    }

    void recv(std::vector<int> &, int, int){
      assert(!"recv called without mpi");
    }

    void recv(std::string &, int, int){
      assert(!"recv called without mpi");
    }

    void sum(double *, double *, int) {
      assert(!"sum called without mpi");
    }

    void sum(ulong *, ulong *, int) {
      assert(!"sum called without mpi");
    }

    void max(ulong *, ulong *, int) {
      assert(!"max called without mpi");
    }

    void bcast(std::vector<std::pair<std::string, ulong> > &){}
      
    void
    KeyValue::add(const char *, int, const char *, int){
      assert(!"KeyValue::add called without mpi");
    }

    MapReduce::MapReduce(void)
      : _impl(0){}

    MapReduce::~MapReduce(void){}

    KeyValue *
    MapReduce::kv(void){
      assert(!"MapReduce::kv called without mpi");
      return 0;
    }

    int
    MapReduce::map(int, LoadFunction, void *){
      assert(!"MapReduce::map called without mpi");
      return 0;
    }

    int
    MapReduce::map(KeyValue *, MapUpdate, void *){
      assert(!"MapReduce::map called without mpi");
      return 0;   
    }

    int
    MapReduce::collate(void){
      assert(!"MapReduce::collate called without mpi");
      return 0;   
    }

    int
    MapReduce::reduce(ReduceFunction, void *){
      assert(!"MapReduce::reduce called without mpi");
      return 0;   
    }

    int
    MapReduce::gather(int){
      assert(!"MapReduce::gather called without mpi");
      return 0;
    }
  }
}
