
const char *PROGRAM_NAME = "lexicon";

#include "base.h"
#include "huge.h"
#include "readdir.h"

#include "main.h"

using namespace NLP;
using namespace std;

Huge lexicon("lexicon");
ulong ntokens = 0;
ulong nfiles = 0;

bool
readfile(const char *const filename){
  cerr << "extracting " << filename << endl;
  ifstream in(filename);
  if(!in){
    cerr << "could not open file " << filename << " for reading\n";
    return true;
  }

  std::string token;
  for( ; in >> token; ++ntokens)
    lexicon.add(token, 1);

  nfiles++;

  return true;
}

int
run(int argc, char **argv){
  if(argc < 2){
    cerr << "lexicon: not enough arguments\n";
    cerr << "usage: lexicon <files>\n";
    exit(1);
  }

  for(int arg = 1; arg < argc; arg++){
    readfile(argv[arg]);
    cerr << "finished " << argv[arg] << " at " << ntokens << " tokens (cumulative)\n";
  }

  cerr << "number of files " << nfiles << endl;
  cerr << "number of tokens " << ntokens << endl;

  lexicon.save(cout, "# preface");

  return 0;
}
