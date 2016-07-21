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

#include	<cstring>

inline ulong
strchomp(char *string) {
  char *s = string;
  while(*s && *s != '\n')
    s++;
  *s = '\0';
  return s - string;
}

inline char *
strlower(char *string){
  for(char *s = string; *s; s++)
    *s = std::tolower(*s);
  return string;
}

inline char *
chomp(char *string){
  char *s = string;
  while(*s && *s != '\n')
    s++;
  *s = '\0';
  return string;
}

// short, fast inlinable version of strcmp.
inline bool
strequal(const char *s1, const char *s2) {
  for( ; *s1; s1++, s2++)
    if(*s1 != *s2)
      return false;

  return *s2 == '\0';
}

// short, fast inlinable version of strcmp when
// we have the end of the first string rather than
// having a null character
inline bool
strequal(const char *start, const char *end, const char *s2){
  for( ; start != end; start++, s2++)
    if(*start != *s2)
      return false;

  return *s2 == '\0';
}

inline bool
str2ulong(const char *str, ulong &value){
  char *error = 0;
  if(!str)
    return false;
  long temp = strtol(str, &error, 10);
  if(error == str || temp < 0){
    value = 0;
    return false;
  }
  value = temp;
  return true;
}

inline bool
str2long(const char *str, long &value){
  char *error = 0;
  if(!str)
    return false;
  long temp = strtol(str, &error, 10);
  if(error == str){
    value = 0;
    return false;
  }
  value = temp;
  return true;
}

inline bool
str2double(const char *str, double &value){
  char *error = 0;
  if(!str)
    return false;
  double temp = strtod(str, &error);
  if(error == str){
    value = 0.0;
    return false;
  }
  value = temp;
  return true;
}

inline char *
pathcat(char *path, char *file){
  uint pathlen = strlen(path);
  uint filelen = strlen(file);
  char *buffer = 0;

  if(path[pathlen-1] == '/'){
    buffer = new char[pathlen + filelen + 1];
    strcpy(buffer, path);
    strcpy(buffer + pathlen, file);
  }else{
    buffer = new char[pathlen + filelen + 2];
    strcpy(buffer, path);
    buffer[pathlen] = '/';
    strcpy(buffer + pathlen + 1, file);
  }
  return buffer;
}

inline const char *
bool2str(bool flag){
  return flag ? "true" : "false";
}
