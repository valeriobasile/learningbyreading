
#include "base.h"
#include "pool.h"
#include "bootstrap/bootstrap.h"

#include "libgen.h"

using namespace std;
using namespace NLP;

const static ulong NITERATIONS = 200;
const static ulong NBEST_TEMPS = 5;
const static ulong NBEST_TERMS = 5;
const static ulong DEPTH_TEMPS = 10;
const static ulong DEPTH_TERMS = 10;

int
main(int argc, char **argv){
  Bootstrap::Bootstrap bootstrap(argv[1]);

  const ulong NCATS = argc - 2;

  Bootstrap::Categories categories;
  categories.reserve(NCATS);

  cerr << "data loaded" << endl;

  for(int arg = 2; arg < argc; ++arg){
    std::string filename = argv[arg];
    std::string name = basename(argv[arg]);
    Bootstrap::Category *cat = new Bootstrap::Category(name, arg - 2, bootstrap);
    bootstrap.load_terms(filename, cat->previous, cat->cat);
    categories.push_back(cat);
  }

  for(ulong iteration = 0; iteration < NITERATIONS; ++iteration){
    for(Bootstrap::Categories::iterator cat = categories.begin(); cat != categories.end(); ++cat)
      (*cat)->get_templates(iteration);
    for(Bootstrap::Categories::iterator cat = categories.begin(); cat != categories.end(); ++cat)
      (*cat)->get_terms(iteration, NBEST_TEMPS, DEPTH_TEMPS);
    for(Bootstrap::Categories::iterator cat = categories.begin(); cat != categories.end(); ++cat)
      (*cat)->add_terms(iteration, NBEST_TERMS, DEPTH_TERMS);
    for(Bootstrap::Categories::iterator cat = categories.begin(); cat != categories.end(); ++cat)
      (*cat)->cleanup(iteration);
    cerr << "--------------------------------------------------------" << endl;
  }

  for(Bootstrap::Categories::iterator i = categories.begin(); i != categories.end(); ++i){
    Bootstrap::Category *cat = *i;

    std::string outfile = "out/" + cat->name;
    ofstream cat_file(outfile.c_str());
    int rank = 0;
    for(Bootstrap::Terms::const_iterator j = cat->previous.begin(); j != cat->previous.end(); ++j, ++rank){
      const Bootstrap::Term *term = *j;
      cat_file << rank << '\t' << term->word.str() << '\t' << term->added << endl;
    }
  }

  return 0;
}
