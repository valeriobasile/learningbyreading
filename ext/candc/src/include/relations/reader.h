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


class Reader {
private:
  ulong _sfields;
  void _resize(void){
    char **old_fields = fields;

    fields = new char *[_sfields * 2];
    if(fields == NULL)
      fatal("could not reallocate memory for Reader fields array");

    memmove(fields, old_fields, _sfields*sizeof(char *));
    _sfields *= 2;

    delete [] old_fields;
  }
public:
  ulong nfields;
  char **fields;

  const char FIELD_SEP;
  const char DATA_TERM;

  Reader(const char FIELD_SEP = '\t', const char DATA_TERM = '\t', const ulong size = 128):
      _sfields(size),  nfields(0), fields(new char *[_sfields]),
      FIELD_SEP(FIELD_SEP), DATA_TERM(DATA_TERM) {
    assert(size > 0);
    if(fields == NULL)
      fatal("could not allocate memory for Reader fields or raw array");
  };

  virtual ~Reader(void) {
    delete [] fields;
  };

  void split(char *buffer){
    char *field = buffer;

    nfields = 0;
    if(*buffer == '\n')
      return;

    for(char *b = buffer; *b; b++){
      if(*b == DATA_TERM){
        *b = '\0';
        break;
      }
      if(*b == FIELD_SEP){
        *b = '\0';
        if(nfields == _sfields)
          _resize();
        fields[nfields++] = field;
        field = b + 1;
        continue;
      }
    }
    if(nfields == _sfields)
      _resize();
    fields[nfields++] = field;
  };
};
