
#include "base.h"
#include "huge.h"
#include "readdir.h"

using namespace NLP;
using namespace std;

Huge lexicon("lexicon");
unsigned long long int nbytes = 0;
ulong ngrs = 0;
ulong nsentences = 0;
ulong nfiles = 0;

bool
readfile(const char *const filename){
  cerr << "extracting " << filename << endl;
  ifstream in(filename);
  if(!in){
    cerr << "could not open file " << filename << " for reading\n";
    return true;
  }

  std::string line;
  std::string gr;
  while(getline(in, line)){
    nbytes += in.gcount();
    if(line[0] == '('){
      gr = "";
      for(const char *s = line.c_str(); *s; ++s){
	if(*s == '_'){
	  if(s[1] == ' ' || s[1] == ')')
	    gr += *s++;
	  else while(*s && *s != ' ' && *s != ')')
	    ++s;
	}
	gr += *s;
      }
      lexicon.add(gr, 1);
      ++ngrs;
    }else if(line[0] == '<'){
      ++nsentences;
    }
  }
  nfiles++;

  return true;
}

char filenamebuf[1024];

DirectoryReader dir;

int
main(int argc, char **argv){
  if(argc < 2){
    cerr << "lexicon: not enough arguments\n";
    cerr << "usage: lexicon <files>\n";
    exit(1);
  }

  for(int arg = 1; arg < argc; arg++){
    strcpy(filenamebuf, argv[arg]);

    char *filename = filenamebuf;
    int len = strlen(filename);

    if(filename[len - 1] == '/')
      filename[len - 1] = '\0';

    cerr << "reading " << argv[arg] << endl;
    dir.read(filename, &readfile);
    cerr << "finished " << argv[arg] << " at " << ngrs << " GRs (cumulative)\n";
  }

  cerr << "number of bytes read " << nbytes << endl;
  cerr << "number of GRs " << ngrs << endl;
  cerr << "number of sentences " << nsentences << endl;
  cerr << "number of files " << nfiles << endl;

  lexicon.save(cout, "# preface");

  return 0;
}
