// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <string>
#include <cstring>
#include <sstream>

#include <boost/serialization/utility.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/mpi.hpp>

#include "mpi.h"
#include "keyvalue.h"
#include "mapreduce.h"

#include "base.h"

#include "cluster.h"

using namespace boost;
namespace MR = MAPREDUCE_NS;

namespace NLP {
  namespace Cluster {

    ulong rank = 0;
    ulong size = 1;
    std::string rank_str;
    std::string size_str;
    std::string processor;
    bool USE_MPI;

    mpi::environment env;
    mpi::communicator world;

    void init(int &argc, char **&argv, bool use_mpi){
      USE_MPI = use_mpi;

      rank = world.rank();
      size = world.size();

      processor = env.processor_name();

      std::ostringstream out;

      out.str("");
      out << Cluster::rank;
      rank_str = out.str();

      out.str("");
      out << Cluster::size;
      size_str = out.str();
    }

    void finalize(void){
      // MPI::Finalize();
      world.barrier();
///      world.finalize();
    }

    void barrier(void){
      // MPI::COMM_WORLD.Barrier();
      world.barrier();
    }

    void send(const std::vector<double> &vals, int dest, int tag){
      world.send(dest, tag, vals);
		}

    void send(const std::vector<ulong> &vals, int dest, int tag){
      world.send(dest, tag, vals);
		}

    void send(const std::vector<long> &vals, int dest, int tag){
      world.send(dest, tag, vals);
		}

    void send(const std::vector<int> &vals, int dest, int tag){
      world.send(dest, tag, vals);
		}

    void send(const std::string &s, int dest, int tag){
      world.send(dest, tag, s);
		}

		void recv(std::vector<double> &vals, int src, int tag){
			world.recv(src, tag, vals);
		}

		void recv(std::vector<ulong> &vals, int src, int tag){
			world.recv(src, tag, vals);
		}

		void recv(std::vector<long> &vals, int src, int tag){
			world.recv(src, tag, vals);
		}

		void recv(std::vector<int> &vals, int src, int tag){
			world.recv(src, tag, vals);
		}

		void recv(std::string &s, int src, int tag){
			world.recv(src, tag, s);
		}

    void sum(double *local_vals, double *global_vals, int len_vals){
      memset(global_vals, 0, len_vals*sizeof(double));
      // MPI::COMM_WORLD.Allreduce(local_vals, global_vals, len_vals, MPI::DOUBLE, MPI::SUM);
      mpi::all_reduce(world, local_vals, len_vals, global_vals, std::plus<double>());
    }

    void sum(ulong *local_vals, ulong *global_vals, int len_vals){
      memset(global_vals, 0, len_vals*sizeof(ulong));
      // MPI::COMM_WORLD.Allreduce(local_vals, global_vals, len_vals, MPI::DOUBLE, MPI::SUM);
      mpi::all_reduce(world, local_vals, len_vals, global_vals, std::plus<ulong>());
    }

    void max(ulong *local_vals, ulong *global_vals, int len_vals) {
      memset(global_vals, 0, len_vals*sizeof(ulong));
      //      MPI::COMM_WORLD.Allreduce(local_vals, global_vals, len_vals, MPI::DOUBLE, MPI::MAX);
      mpi::all_reduce(world, local_vals, len_vals, global_vals, mpi::maximum<ulong>());
    }

    void bcast(std::vector<std::pair<std::string, ulong> > &counts){
      mpi::broadcast(world, counts, 0);
    }

    void
    KeyValue::add(const char *key, int keybytes, const char *value, int valuebytes){
      MR::KeyValue *_this = reinterpret_cast<MR::KeyValue *>(this);
      _this->add(const_cast<char *>(key), keybytes,
		 const_cast<char *>(value), valuebytes);
    }

    MapReduce::MapReduce(void)
      : _impl(new MR::MapReduce(MPI_COMM_WORLD)){
      Cluster::barrier();
    }

    MapReduce::~MapReduce(void){
      Cluster::barrier();
      delete reinterpret_cast<MR::MapReduce *>(_impl);
    }

    typedef void (*_LoadFunction)(int, MR::KeyValue *, void *);

    KeyValue *
    MapReduce::kv(void){
      MR::KeyValue *_kv = reinterpret_cast<MR::MapReduce *>(_impl)->kv;
      return reinterpret_cast<KeyValue *>(_kv);
    }

    int
    MapReduce::map(int nmap, LoadFunction func, void *ptr){
      _LoadFunction _func = reinterpret_cast<_LoadFunction>(func);
      return reinterpret_cast<MR::MapReduce *>(_impl)->map(nmap, _func, ptr);
    }

    typedef void (*_MapUpdate)(int, char *, int, char *, int, MR::KeyValue *, void *);
    
    int
    MapReduce::map(KeyValue *kv, MapUpdate func, void *ptr){
      MR::KeyValue *_kv = reinterpret_cast<MR::MapReduce *>(_impl)->kv;
      _MapUpdate _func = reinterpret_cast<_MapUpdate>(func);
      return reinterpret_cast<MR::MapReduce *>(_impl)->map(_kv, _func, ptr);
    }

    int
    MapReduce::collate(void){
      return reinterpret_cast<MR::MapReduce *>(_impl)->collate(0);
    }

    typedef void (*_ReduceFunction)(char *, int, char *, int, int *,
        MR::KeyValue *, void *);

    int
    MapReduce::reduce(ReduceFunction func, void *ptr){
      _ReduceFunction _func = reinterpret_cast<_ReduceFunction>(func);
      return reinterpret_cast<MR::MapReduce *>(_impl)->reduce(_func, ptr);
    }

    int
    MapReduce::gather(int nprocs){
      return reinterpret_cast<MR::MapReduce *>(_impl)->gather(nprocs);
    }
  }
}
