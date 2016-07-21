// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <mpi.h>

#include <cassert>
#include <cmath>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>
#include <limits>
#include <valarray>

#include <sys/types.h>
#include <unistd.h>

#include <boost/filesystem.hpp>

#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/filter/bzip2.hpp>
#include <boost/iostreams/device/file.hpp>

#include "base.h"

#include "cluster.h"

#include "config/config.h"
#include "io/format.h"
#include "config/format.h"

#include "prob.h"

#include "model/model.h"
#include "model/types.h"

#include "io/reader.h"
#include "io/reader_factory.h"
#include "io/writer.h"

#include "tagger/tagdict.h"
#include "tagger/tagger.h"

#include "maxent/feature.h"
#include "maxent/attribute.h"
#include "maxent/context.h"
#include "maxent/gis.h"
#include "maxent/bfgs.h"
#include "maxent/perceptron.h"

#include "timer.h"

#include "tagger/tagsetdict.h"
#include "tagger/super.h"
#include "extract/super.h"

const char *PROGRAM_NAME = "mpi_train_super";

int rank = 1;
int size = 1;

namespace fs = boost::filesystem;
namespace io = boost::iostreams;

using namespace std;
using namespace NLP;

void
write(ulong cur_node, const string &line, ofstream &out){
  if(cur_node != 0)
    Cluster::send(line, cur_node, 0);
  else
    out << line;
}

int
main(int argc, char* argv[]){
  NLP::Cluster::init(argc, argv, true);

  std::ostringstream PREFACE;
  PREFACE << start_preface(argc, argv);

  Config::Main cfg(PROGRAM_NAME);

  Config::OpPath temp_dir("temp_dir", "where to store temporary files", "/tmp");

  Config::OpPath src(cfg, SPACE, "src", "source file to distribute");
  Config::OpPath dest(cfg, "dest", "destination filename (default relative to temp_dir", "", &temp_dir);

  cfg.reg(temp_dir, SPACE);

  Config::Op<bool> header(cfg, SPACE, "header", "source file has a header", false);

  Config::Op<bool> gzip(cfg, SPACE, "gzip", "source file is compressed with gzip", false);
  Config::Op<bool> bzip2(cfg, "bzip2", "source file is compressed with bzip2", false);

  Config::Op<ulong> dist_block_size(cfg, SPACE,  "dist_block_size", "the number of sentences to distribute in each step", 1);
  Config::Op<ulong> max_lines(cfg, "max_lines", "how many lines from the input file to use", 0);
  Config::Op<bool> ignore_blank(cfg, "ignore_blank", "ignore blank lines in the input file", true);

  cfg.parse(argc, argv);
  cfg.check();

  Timer timer("total");

  try {
    if(dist_block_size() < 1)
      throw NLP::Exception("dist_block_size must be at least 1");
    if(max_lines() < 0)
      throw NLP::Exception("max_lines must be 0 or more (0 means use the whole file)");
    fs::path src_path = src();
    fs::path dest_path;
    if(dest.value != "")
      dest_path = dest();
    else{
      dest_path = temp_dir();
      if(src_path.extension() == ".gz" || src_path.extension() == ".bz2")
        dest_path /= src_path.stem();
      else
        dest_path /= src_path.filename();
    }
    string dest_filename = dest_path.string() + '.' + Cluster::rank_str;

    ofstream out(dest_filename.c_str());
    if(!out)
      throw NLP::IOException("failed to open local out file for writing", dest_filename);

    cout << Cluster::rank_str + " writing " + dest_filename + '\n' << flush;

    string line;
    if(Cluster::rank == 0){
      io::filtering_istream in;

      if(gzip() || src_path.extension() == ".gz")
        in.push(io::gzip_decompressor());
      else if(bzip2() || src_path.extension() == ".bz2")
        in.push(io::bzip2_decompressor());

      in.push(io::file_source(src_path.string()));

      ulong cur_node = 0;
      bool in_header = header();
      ulong block_size = 0;
      ulong nline = 0;
      while(getline(in, line)){
        line += '\n';
        if(in_header){
          if(line[0] == '\n')
            in_header = false;
          for(ulong i = 0; i != Cluster::size; ++i)
            write(i, line, out);
          continue;
        }

        if(ignore_blank() && line[0] == '\n')
          continue;

        write(cur_node, line, out);
        block_size = (block_size + 1) % dist_block_size();
        if(block_size == 0)
          cur_node = (cur_node + 1) % Cluster::size;
        nline++;
        if(nline == max_lines())
          break;
      }
      for(cur_node = 1; cur_node != Cluster::size; ++cur_node)
        Cluster::send("", cur_node, 0);
    }else{
      while(true){
        Cluster::recv(line, 0, 0);
        if(line.size() == 0)
          break;
        out << line;
      }
    }
  }catch(NLP::IOException e){
    if(NLP::Cluster::rank == 0){
      cerr << "maxent.ioexception[" << NLP::Cluster::rank << "]:" << e.msg << endl;
      cerr << "  in location " << e.uri << ':' << e.line << endl;
    }
  }catch(NLP::Exception e){
    if(NLP::Cluster::rank == 0)
      cerr << "maxent.exception[" << NLP::Cluster::rank << "]: " << e.msg << endl;
  }

  NLP::Cluster::finalize();

  return 0;
}
