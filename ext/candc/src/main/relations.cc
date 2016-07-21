// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

extern const char *const PROGNAME = "relations";

#include <cassert>
#include <cmath>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <limits>
#include <iomanip>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

using namespace std;

#include "utils.h"
#include "except.h"
#include "pool.h"
#include "hash.h"
#include "hashtable/entry.h"
#include "hashtable/list.h"
#include "hashtable/count.h"
#include "relations/relations.h"
#include "relations/morpha.h"
#include "readdir.h"

#include "relations/reader.h"
#include "relations/word.h"

void addrelation_grefenstette(Word *wordtag1, Word *wordtag2, const char *rel);

#include "relations/phrase.h"
#include "relations/sentence.h"

char buffer[2048];
char morph_buffer1[2048];
char morph_buffer2[2048];
char b1[2048];
char b2[2048];
const static int MAX_WORDS = 6;

ulong nfiles = 0;
ulong nrelations = 0;
ulong nsentences = 0;
ulong nignored = 0;
ulong nlines = 0;
const char *current = NULL;

ulong nw1toolong = 0;
ulong nw2toolong = 0;
ulong ntoolong = 0;
ulong nampersand = 0;

bool morph1 = false;
bool morph2 = false;
bool remove_tag1 = false;
bool remove_tag2 = false;
bool has_tag1 = false;
bool has_tag2 = false;
bool has_relations = true;
bool remove_relations = false;
bool ignore_relations = false;
bool sort_relations = true;
bool write_relations = true;

ulong max_sentences = ULONG_MAX;
ulong max_relations = ULONG_MAX;
ulong max_unique = ULONG_MAX;

FILE *logfile = NULL;

Relations relations;
ListLexicon<Lexeme,LEX_SMALL,POOL_SMALL> ignore("ignore");
StringPool pool(POOL_SMALL);

void
load_ignorelist(char *filename){
  ignore_relations = true;

  ReadBuffer buf(filename, 128);

  while(buf.read()){
    strchomp(buf.data());
    ignore.addlexeme(buf.data());
  }
}

void
add_ignore(char *word){
  ignore_relations = true;
  ignore.addlexeme(word);
}

void
print_options(FILE *file){
  fprintf(file, "%s invoked with the following options:\n", PROGNAME);
  fprintf(file, "  %s:morph1 = %s\n", PROGNAME, bool2str(morph1));
  fprintf(file, "  %s:morph2 = %s\n", PROGNAME, bool2str(morph2));
  fprintf(file, "  %s:has_tag1 = %s\n", PROGNAME, bool2str(has_tag1));
  fprintf(file, "  %s:has_tag2 = %s\n", PROGNAME, bool2str(has_tag2));
  fprintf(file, "  %s:remove_tag1 = %s\n", PROGNAME, bool2str(remove_tag1));
  fprintf(file, "  %s:remove_tag2 = %s\n", PROGNAME, bool2str(remove_tag2));
  fprintf(file, "  %s:has_relations = %s\n", PROGNAME, bool2str(has_relations));
  fprintf(file, "  %s:remove_relations = %s\n", PROGNAME, bool2str(remove_relations));
  fprintf(file, "  %s:sort_relations = %s\n", PROGNAME, bool2str(sort_relations));
  fprintf(file, "  %s:write_relations = %s\n", PROGNAME, bool2str(write_relations));
  fprintf(file, "  %s ignored the following relations: \n", PROGNAME);
  for(ulong i = 0; i < ignore.nlexemes; i++)
    fprintf(file, "  %s\n", ignore.lexemes[i]->str());
  fprintf(file, "  %s:max_sentences = %lu\n", PROGNAME, max_sentences);
  fprintf(file, "  %s:max_relations = %lu\n", PROGNAME, max_relations);
  fprintf(file, "  %s:max_unique = %lu\n", PROGNAME, max_unique);
}

void
print_elapsed(FILE *file, char *msg, time_t start, time_t end){
  char timebuf[32];
  time_t elapsed = end - start;
  strftime(timebuf, sizeof(timebuf), "%M:%S", gmtime(&elapsed));
  fprintf(file, "%s %s\n", msg, timebuf);
}

char *
next(char *s){
  for( ; *s; s++)
    if(*s == ' ' || *s == '\n'){
      *s++ = '\0';
      return s;
    }
  return s;
}

char *
last(char *s){
  for( ; *s; s++)
    if(*s == '\n'){
      *s++ = '\0';
      return s;
    }
  return NULL;
}

void
chomp_tag(char *s){
  for( ; *s; s++)
    if(*s == '_'){
      *s++ = '\0';
      return;
    }
}

char *
tagend(char *s){
  for( ; *s; s++)
    if(*s == '\t'){
      *s++ = '\0';
      return s;
    }
  return s;
}

char *
wordend(char *s){
  int nwords = 1;
  for( ; *s; s++){
    if((*s == '_' || *s == '-' || *s == '/') && nwords++ == MAX_WORDS)
      return NULL;
    else if(*s == '\t'){
      *s++ = '\0';
      return s;
    }
  }

  return s;
}

char *
findend(char *s){
  for( ; *s; s++)
    if(*s == '\t' || *s == ' ' || *s == '\n')
      return s;
  return s;
}

bool
stop(bool test, const char *fmt, ...){
  if(test){
    va_list va;

    va_start(va, fmt);
    vfprintf(logfile, fmt, va);
    va_end(va);
    fputc('\n', logfile);

    va_start(va, fmt);
    vfprintf(stdout, fmt, va);
    va_end(va);
    fputc('\n', stdout);
  }

  return test;
}

void
addrelation(const char *w1, const char *w2, char rel, ulong freq){
  if(ignore_relations && ignore.find(rel) != NULL)
    nignored++;
  else if(remove_relations)
    relations.add(w1, w2, '\0', freq);
  else
    relations.add(w1, w2, rel, freq);
}

void
addrelation(const char *w1, const char *w2, const char *rel, ulong freq){
  if(ignore_relations && ignore.find(rel) != NULL)
    nignored++;
  else if(remove_relations)
    relations.add(w1, w2, (const char *)NULL, freq);
  else
    relations.add(w1, w2, rel, freq);
}

void
addinverse(const char *w1, const char *w2, const char *rel, ulong freq){
  if(ignore_relations && ignore.find(rel) != NULL)
    nignored++;
  else if(remove_relations)
    relations.addinv(w1, w2, (const char *)NULL, freq);
  else
    relations.addinv(w1, w2, rel, freq);
}

void
addrelation_minipar(char *w1, char *t1, char *w2, char *t2, char *rel){
  if(t2[0] == '(' || w2[0] == '~')
    return;
  if(w1[0] == 'b' && w1[1] == 'e' && w1[2] == '\0')
    return;
  if(w1[0] == 'i' && w1[1] == 'n' && w1[2] == 'f' && w1[3] == '\0')
    return;

  if(w2[0] == '&' || w1[0] == '#' || w2[0] == '#'){
    nampersand++;
    return;
  }

  if(t1[0] == 'N' && t1[1] == '\0')
    addrelation(w1, w2, rel, 1);
  if(t2[0] == 'N' && t2[1] == '\0')
    addinverse(w1, w2, rel, 1);
}

void
addrelation_grefenstette(Word *wordtag1, Word *wordtag2, const char *rel){
  char *w1 = b1;
  char *w2 = b2;

  wordtag1->str(b1, sizeof(b1));
  wordtag2->str(b2, sizeof(b2));

  if(morph1){
    if(morph_analyse(morph_buffer1, w1, has_tag1))
      w1 = morph_buffer1;
    else{
      file_msg(current, "%lu:morphing failed on '%s'", nlines, w1);
      fprintf(logfile, "%lu:morphing failed on '%s'", nlines, w1);
    }
  }

  if(morph2){
    if(morph_analyse(morph_buffer2, w2, has_tag2))
      w2 = morph_buffer2;
    else{
      file_msg(current, "%lu:morphing failed on '%s'", nlines, w2);
      fprintf(logfile, "%lu:morphing failed on '%s'", nlines, w2);
    }
  }

  if(remove_tag1)
    chomp_tag(w1);
  if(remove_tag2)
    chomp_tag(w2);

  nrelations++;
  addrelation(w1, w2, rel, 1);

//  if(wordtag2->is_noun()){
//    nrelations++;
//    addinverse(w1, w2, rel, 1);
//  }
}

bool
readfile_grefenstette(const char *const filename){
  fprintf(logfile, "extracting %s: ", filename);
  printf("extracting %s: ", filename);

  if(stop(nsentences >= max_sentences, "max_sentences reached"))
    return true;
  if(stop(nrelations >= max_relations, "max_relations reached"))
    return true;
  if(stop(relations.nlexemes >= max_unique, "max_unique reached"))
    return true;

  current = filename;
  ReadBuffer buf(filename, 1024);
  Reader reader('\t', '\n', 32);
  Sentence s;
  nlines = 0;

  while(buf.read()){
    if(buf.data()[0] == '<' && strcmp(buf.data(), "<S>\n") == 0){
      nsentences++;
      if(stop(nsentences >= max_sentences, "%d lines -- max_sentences reached", nlines))
        return true;

      s.parse();
      s.pass1();
      s.pass2();
      s.pass3();
      s.pass4();
      s.clear();
      continue;
    }

    if(stop(nrelations >= max_relations, "%d lines -- max_relations reached", nlines))
      return true;
    if(stop(relations.nlexemes >= max_unique, "%d lines -- max_unique reached\n", nlines))
      return true;

    reader.split(buf.data());
    if(reader.nfields != 3)
      fatal("incorrect number of fields");

    s.add(reader.fields[0], reader.fields[1], reader.fields[2]);
    nlines++;
  }

  nfiles++;
  fprintf(logfile, "%lu lines\n", nlines);
  printf("%lu lines\n", nlines);

  return true;
}

bool
readfile_minipar(const char *const filename){
  // minipar output format (modified) is w1\tt1\trel\t\w2\tt2
  // multiword terms have spaces replaced with underscores
  fprintf(logfile, "extracting %s: ", filename);
  printf("extracting %s: ", filename);

  if(stop(nsentences >= max_sentences, "max_sentences reached"))
    return true;
  if(stop(nrelations >= max_relations, "max_relations reached"))
    return true;
  if(stop(relations.nlexemes >= max_unique, "max_unique reached"))
    return true;

  FILE *file = fopen(filename, "r");
  int nlines = 0;

  if(file == NULL){
    file_perror(filename);
    return false;
  }

  for( ; fgets(buffer, sizeof(buffer), file) != NULL; nlines++){
    char *w1 = buffer;	// first word
    char *w2 = NULL;	// second word
    char *rel = NULL;	// grammatical relation
    char *t1 = NULL;	// first POS tag
    char *t2 = NULL;	// second POS tag
    char *end = NULL;	// used to check if there are any other tabs

    if(buffer[0] == '&'){
      nampersand++;
      continue;
    }

    if(buffer[0] == '\n'){
      nsentences++;
      if(nsentences >= max_sentences){
        fprintf(logfile, "%d lines -- max_sentences reached\n", nlines);
        printf("%d lines -- max_sentences reached\n", nlines);
        fclose(file);
        return true;
      }
      continue;
    }
    nrelations++;
    if(nrelations >= max_relations){
      fprintf(logfile, "%d lines -- max_relations reached\n", nlines);
      printf("%d lines -- max_relations reached\n", nlines);
      fclose(file);
      return true;
    }

    w1 = buffer;
    t1 = wordend(w1);
    if(t1 == NULL){		// word 1 too long (too many _/-)
      nw1toolong++;
      continue;
    }
    if(*t1 == '\0')
      file_error(filename, nlines, "missing tab between word1 and tag1");

    rel = tagend(t1);
    if(*rel == '\0')
      file_error(filename, nlines, "missing tab between tag1 and relation");

    w2 = tagend(rel);
    if(*w2 == '\0')
      file_error(filename, nlines, "missing tab between relation and word2");

    t2 = wordend(w2);
    if(t2 == NULL){		// word 2 too long (too many _/-)
      nw2toolong++;
      continue;
    }
    if(*t2 == '\0')
      file_error(filename, nlines, "missing tab between word2 and tag2");

    end = findend(t2);
    if(*end != '\n')
      file_error(filename, nlines, "extra tabs found in input line");
    *end = '\0';

    if(end > 80 + buffer){	// whole input line is too long
      ntoolong++;
      continue;
    }

    if(morph1){
      if(morph_analyse(morph_buffer1, w1, has_tag1))
        w1 = morph_buffer1;
      else
        file_error(filename, nlines, "morphological analysis failed on '%s'", w1);
    }

    if(morph2){
      if(morph_analyse(morph_buffer2, w2, has_tag2))
        w2 = morph_buffer2;
      else
        file_error(filename, nlines, "morphological analysis failed on '%s'", w1);
    }

    if(remove_tag1)
      chomp_tag(w1);
    if(remove_tag2)
      chomp_tag(w2);

    addrelation_minipar(w1, t1, w2, t2, rel);

    if(relations.nlexemes >= max_unique){
      fprintf(logfile, "%d lines -- max_unique reached\n", nlines);
      printf("%d lines -- max_unique reached\n", nlines);
      fclose(file);
      return true;
    }
  }

  nfiles++;
  fprintf(logfile, "%d lines\n", nlines);
  printf("%d lines\n", nlines);

  fclose(file);
  return true;
}

bool
readfile_default(const char *const filename){
  // window output format is freq w1 w2 rel
  // multiword terms have spaces replaced with underscores
  fprintf(logfile, "extracting %s: ", filename);
  printf("extracting %s: ", filename);

  if(stop(nsentences >= max_sentences, "max_sentences reached"))
    return true;
  if(stop(nrelations >= max_relations, "max_relations reached"))
    return true;
  if(stop(relations.nlexemes >= max_unique, "max_unique reached"))
    return true;

  FILE *file = fopen(filename, "r");
  int nlines = 0;

  if(file == NULL){
    file_perror(filename);
    return false;
  }

  for( ; fgets(buffer, sizeof(buffer), file) != NULL; nlines++){
    char *w1 = buffer;	// first word
    char *w2 = NULL;	// second word
    char *rel = NULL;	// grammatical relation

    if(buffer[0] == '\n'){
      nsentences++;
      if(nsentences >= max_sentences){
        fprintf(logfile, "%d lines -- max_sentences reached\n", nlines);
        printf("%d lines -- max_sentences reached\n", nlines);
        fclose(file);
        return true;
      }
      continue;
    }
    nrelations++;
    if(nrelations >= max_relations){
      fprintf(logfile, "%d lines -- max_relations reached\n", nlines);
      printf("%d lines -- max_relations reached\n", nlines);
      fclose(file);
      return true;
    }

    w2 = next(w1);
    if(*w2 == '\0')
      file_error(filename, nlines, "missing space between word1 and word2");

    rel = next(w2);
    if(!last(rel) && has_relations)
      file_error(filename, nlines, "missing space between word2 and relation '%s'", w2);

    if(morph1){
      if(morph_analyse(morph_buffer1, w1, has_tag1))
        w1 = morph_buffer1;
      else
        file_error(filename, nlines, "morphological analysis failed on '%s'", w1);
    }

    if(morph2){
      if(morph_analyse(morph_buffer2, w2, has_tag2))
        w2 = morph_buffer2;
      else
        file_error(filename, nlines, "morphological analysis failed on '%s'", w1);
    }

    if(remove_tag1)
      chomp_tag(w1);
    if(remove_tag2)
      chomp_tag(w2);

    addrelation(w1, w2, rel, 1);
    if(relations.nlexemes >= max_unique){
      fprintf(logfile, "%d lines -- max_unique reached\n", nlines);
      printf("%d lines -- max_uniqey reached\n", nlines);
      fclose(file);
      return true;
    }
  }

  nfiles++;
  fprintf(logfile, "%d lines\n", nlines);
  printf("%d lines\n", nlines);

  fclose(file);
  return true;
}

void usage(char *fmt, ...) __attribute__ ((format (printf, 1, 2)));
void
usage(char *fmt, ...){
  if(fmt){
    va_list va;
    va_start(va, fmt);

    fprintf(stderr, "%s: ", PROGNAME);
    vfprintf(stderr, fmt, va);
    fputc('\n', stderr);

    va_end(va);
  }
  fprintf(stderr, "usage: relations [options] <outprefix> <files ...>\n"
                  "where options are one or more of:\n"
                  "  -t the objects have tags attached in the form WORD_TAG (for morphing)\n"
                  "  -u the attributes have tags attached in the form WORD_TAG (for morphing)\n"
                  "  -r remove the tags from the objects\n"
                  "  -s remove the tags from the attributes\n"
                  "  -m morphologically analyse the objects\n"
                  "  -n morphologically analyse the attributes\n"
                  "  -a remove all relations (i.e. only use attribute values)\n"
                  "  -b do not sort the relations\n"
                  "  -w do not write the relations (just the statistics)\n"
                  "  -z does not have relations\n"
                  "  -i <relation> ignore the given relation\n"
                  "  -j <file> ignore all the relations in the file\n"
                  "  -e <max_sentences> maximum number of sentences to read\n"
                  "  -f <max_relations> maximum number of relations to read\n"
                  "  -g <max_unique> maximum number of unique relations to read\n"
                  "  -x use minipar relations format\n"
                  "  -y use grefenstette parser and relations extractor\n"
                  "  -h display this usage information\n");
  exit(fmt != NULL);
}

int
main(int argc, char **argv){
  int c = 0;
  opterr = 0;

  bool (* readfile)(const char *const) = readfile_default;

  while ((c = getopt(argc, argv, "tursmnabwzhi:j:e:f:g:xy")) != EOF)
    switch (c) {
      case 't': if(has_tag1)
                  usage("only one has_tag1 argument (-t) accepted");
                has_tag1 = true;
                break;
      case 'u': if(has_tag2)
                  usage("only one has_tag2 argument (-u) accepted");
                has_tag2 = true;
                break;
      case 'r': if(remove_tag1)
                  usage("only one remove_tag1 argument (-r) accepted");
                remove_tag1 = true;
                break;
      case 's': if(remove_tag2)
                  usage("only one remove_tag2 argument (-s) accepted");
                remove_tag2 = true;
                break;
      case 'm': if(morph1)
                  usage("only one morph1 argument (-m) accepted");
                morph1 = true;
                break;
      case 'n': if(morph2)
                  usage("only one morph2 argument (-n) accepted");
                morph2 = true;
                break;
      case 'a': if(remove_relations)
                  usage("only one ignore_all_relations argument (-a) accepted");
                remove_relations = true;
                break;
      case 'b': if(!sort_relations)
                  usage("only one not sort_relations argument (-b) accepted");
                sort_relations = false;
                break;
      case 'w': if(!write_relations)
                  usage("only one not write_relations argument (-w) accepted");
                write_relations = false;
                break;
      case 'z': if(!has_relations)
                  usage("only one not has_relations argument (-z) accepted");
                has_relations = false;
                break;
      case 'i': add_ignore(optarg);
                break;
      case 'j': load_ignorelist(optarg);
                break;
      case 'e': if(max_sentences != ULONG_MAX)
                  usage("only one max_sentences argument (-e) accepted");
                if(!str2ulong(optarg, max_sentences) || max_sentences == 0)
                  usage("max_sentences argument must be a positive integer");
                break;
      case 'f': if(max_relations != ULONG_MAX)
                  usage("only one max_relations argument (-f) accepted");
                if(!str2ulong(optarg, max_relations) || max_relations == 0)
                  usage("max_relations argument must be a positive integer");
                break;
      case 'g': if(max_unique != ULONG_MAX)
                  usage("only one max_unique argument (-g) accepted");
                if(!str2ulong(optarg, max_unique) || max_unique == 0)
                  usage("max_unique argument must be a positive integer");
                break;
      case 'x': readfile = &readfile_minipar;
                break;
      case 'y': readfile = &readfile_grefenstette;
                break;
      case 'h': usage(NULL); break;
      case '?': if(optopt == 'i' || optopt == 'j' || optopt == 'e' ||
                   optopt == 'f' || optopt == 'g')
                  usage("missing filename argument for '-%c' option", optopt);
                else
                  usage("unrecognised option '-%c'", optopt);
    }

  if(argc - optind < 2)
    usage("missing prefix name or files");

  char filenamebuf[1024];

  strcpy(filenamebuf, argv[optind]);
  strcat(filenamebuf, ".log");

  char *logfilename = strdup(filenamebuf);
  logfile = fopen(logfilename, "w");
  if(logfile == NULL)
    file_error(logfilename, 0, "could not open log file");

  strcpy(filenamebuf, argv[optind]);
  strcat(filenamebuf, ".relations");

  char *outfilename = strdup(filenamebuf);
  FILE *out = fopen(outfilename, "w");
  if(out == NULL)
    file_error(outfilename, 0, "could not open to write");

  optind++;

  print_options(stdout);
  print_options(logfile);

  fprintf(stderr, "initialising morphological analyser...\n");

  morph_initialise("/home/james/system/relations/verbstem.list");

  time_t start_time = time(NULL);

  fprintf(stderr, "reading...\n");
  fflush(stderr);

  DirectoryReader dir;
  while(optind != argc)
    dir.read(argv[optind++], readfile);

  time_t extract_time = time(NULL);

  fprintf(logfile, "number of files %lu\n", nfiles);
  fprintf(logfile, "number of sentences %lu\n", nsentences);
  fprintf(logfile, "number of read relations %lu\n", nrelations);
  fprintf(logfile, "number of ignored relations %lu\n", nignored);
  fprintf(logfile, "number of word1 too long relations %lu\n", nw1toolong);
  fprintf(logfile, "number of word2 too long relations %lu\n", nw2toolong);
  fprintf(logfile, "number of too long relations %lu\n", ntoolong);
  fprintf(logfile, "number of ignored ampersand relations %lu\n", nampersand);
  fprintf(logfile, "collected relations %lu\n", relations.nlexemes);
  fflush(logfile);

  relations.printstats(logfile);
  relations.pool()->stats(logfile);
  relations.lpool()->stats(logfile);
  fflush(logfile);

  if(sort_relations){
    fprintf(stderr, "sorting...\n");
    fflush(stderr);
    relations.sort();
  }else{
    fprintf(stderr, "skipping sorting...\n");
    fflush(stderr);
  }
  time_t sort_time = time(NULL);

  if(write_relations){
    fprintf(stderr, "saving...\n");
    fflush(stderr);
    relations.savestats(out);
  }else{
    fprintf(stderr, "skipping saving...\n");
    fflush(stderr);
  }
  time_t save_time = time(NULL);

  print_elapsed(logfile, "extraction", start_time, extract_time);
  print_elapsed(logfile, "sorting   ", extract_time, sort_time);
  print_elapsed(logfile, "saving    ", sort_time, save_time);
  print_elapsed(logfile, "total     ", start_time, save_time);

  fclose(out);
  fclose(logfile);

  return 0;
}
