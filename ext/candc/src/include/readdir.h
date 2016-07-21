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


#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

class DirectoryReader {
  char _path[1024];

  bool (* _readfile)(const char *const filename);

  bool _readdir(char *filename, char *end){
    DIR *dir = NULL;
    struct dirent *entry = NULL;

    if(filename < end && end[-1] != '/'){
      *end++ = '/';
      *end = '\0';
    }

    if((dir = opendir(filename)) == 0)
      throw NLP::IOException("could not read directory", filename);

    while((entry = readdir(dir)) != NULL){
      char *e = end;
      if(entry->d_name[0] == '.')
        continue;
      for(char *s = entry->d_name; *s; )
        *e++ = *s++;
      *e = '\0';
      if(!_readpath(filename, e))
        return false;
    }

    closedir(dir);
    return true;
  }

  int _readpath(char *filename, char *end){
    struct stat buf;

    if(stat(filename, &buf) < 0)
      throw NLP::IOException("could not stat name", filename);

    if(S_ISDIR(buf.st_mode))
      return _readdir(filename, end);
    else
      return _readfile(filename);

    return false;
  }
public:
  DirectoryReader(void) {};
  ~DirectoryReader(void) {};

  static bool print(const char *const filename){
    printf("'%s'\n", filename);
    return true;
  }

  bool read(const char *dirname, bool (* reader)(const char *const)) {
    _readfile = reader;
    char *end = _path;
    for(const char *d = dirname; *d; )
      *end++ = *d++;
    if(end > _path && end[-1] == '/')
      *--end = '\0';
    else
      *end = '\0';
    return _readpath(_path, end);
  }
};

class DirectoryBuilder {
  static const mode_t DIR_PERMS = S_IRUSR | S_IXUSR | S_IWUSR | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH;

  char _inpath[1024];
  char _outpath[1024];

  bool (* _readfile)(const char *const infile, const char *const outfile);

  bool _readdir(char *infile, char *inend, char *outfile, char *outend){
    DIR *dir = NULL;
    DIR *temp = NULL;
    struct dirent *entry = NULL;

    if(infile < inend && inend[-1] != '/'){
      *inend++ = '/';
      *inend = '\0';
    }

    if(outfile < outend && outend[-1] != '/'){
      *outend++ = '/';
      *outend = '\0';
    }

    if((dir = opendir(infile)) == 0)
      throw NLP::IOException("could not read directory", infile);

    if(outfile[0] != '.' || outfile[1] != '\0'){
      if((temp = opendir(outfile)) != 0){
        closedir(temp);
        throw NLP::IOException("directory already exists", outfile);
      }else{
        if(mkdir(outfile, DIR_PERMS) < 0)
          throw NLP::IOException("could not create directory", outfile);
      }
    }

    while((entry = readdir(dir)) != NULL){
      char *ie = inend;
      char *oe = outend;
      if(entry->d_name[0] == '.')
        continue;
      for(char *s = entry->d_name; *s; )
        *oe++ = *ie++ = *s++;
      *oe = *ie = '\0';

      if(!_readpath(infile, ie, outfile, oe))
        return false;
    }

    closedir(dir);
    return true;
  }

  int _readpath(char *infile, char *inend, char *outfile, char *outend){
    struct stat buf;

    if(stat(infile, &buf) < 0)
      throw NLP::IOException("could not stat name", infile);

    if(S_ISDIR(buf.st_mode))
      return _readdir(infile, inend, outfile, outend);
    else
      return _readfile(infile, outfile);

    return false;
  }
public:
  DirectoryBuilder(void) {};
  ~DirectoryBuilder(void) {};

  static bool print(const char *const infile, const char *const outfile){
    std::cout << "'" << infile << "' -> '" << outfile << "'\n";
    return true;
  }

  bool read(const char *indir, const char *outdir, bool (* builder)(const char *const, const char *const)) {
    _readfile = builder;

    char *inend = _inpath;
    for(const char *d = indir; *d; )
      *inend++ = *d++;
    if(inend > _inpath && inend[-1] == '/')
      *--inend = '\0';
    else
      *inend = '\0';

    char *outend = _outpath;
    for(const char *d = outdir; *d; )
      *outend++ = *d++;
    if(outend > _outpath && outend[-1] == '/')
      *--outend = '\0';
    else
      *outend = '\0';

    return _readpath(_inpath, inend, _outpath, outend);
  }
};
