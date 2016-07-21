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


inline
void dump_uchar(std::ostream &out, uchar value){
  out.put(value);
}

inline
bool load_uchar(std::istream &in, uchar &value){
  return in.get((char &)value);
}

inline
void dump_ushort(std::ostream &out, ushort value){
  dump_uchar(out, 0xFF & value);
  dump_uchar(out, 0xFF & (value >> 8));
}

inline
bool load_ushort(std::istream &in, ushort &value){
  uchar tmp;
  if(!load_uchar(in, tmp))
    return false;
  value = (ushort)tmp;

  if(!load_uchar(in, tmp))
    return false;
  value |= (ushort)tmp << 8;

  return true;
}

inline
void dump_ulong(std::ostream &out, ulong value){
  dump_uchar(out, 0xFF & value);
  dump_uchar(out, 0xFF & (value >> 8));
  dump_uchar(out, 0xFF & (value >> 16));
  dump_uchar(out, 0xFF & (value >> 24));
}

inline
bool load_ulong(std::istream &in, ulong &value){
  uchar tmp;
  if(!load_uchar(in, tmp))
    return false;
  value = (ulong)tmp;

  if(!load_uchar(in, tmp))
    return false;
  value |= (ulong)tmp << 8;

  if(!load_uchar(in, tmp))
    return false;
  value |= (ulong)tmp << 16;

  if(!load_uchar(in, tmp))
    return false;
  value |= (ulong)tmp << 24;

  return true;
}

inline
void dump_float(std::ostream &out, float value){
  union {
    ulong bits;
    float value;
  } tmp;
  tmp.value = value;

  dump_uchar(out, 0xFF & tmp.bits);
  dump_uchar(out, 0xFF & (tmp.bits >> 8));
  dump_uchar(out, 0xFF & (tmp.bits >> 16));
  dump_uchar(out, 0xFF & (tmp.bits >> 24));
}

inline
bool load_float(std::istream &in, float &value){
  union {
    ulong bits;
    float value;
  } res;

  uchar tmp;
  if(!load_uchar(in, tmp))
    return false;
  res.bits = (ulong)tmp;

  if(!load_uchar(in, tmp))
    return false;
  res.bits |= (ulong)tmp << 8;

  if(!load_uchar(in, tmp))
    return false;
  res.bits |= (ulong)tmp << 16;

  if(!load_uchar(in, tmp))
    return false;
  res.bits |= (ulong)tmp << 24;

  value = res.value;

  return true;
}

inline
void dump_string(std::ostream &out, const char *const str){
  int len = strlen(str);
  assert(len <= USHRT_MAX);
  dump_ushort(out, len);
  out.write(str, len);
}

inline
bool load_string(std::istream &in, char *buffer, ushort nbuffer){
  ushort len;
  if(!load_ushort(in, len))
    return false;
  if(len >= nbuffer)
    throw NLP::IOException("string too long for buffer in load_string");
  if(!in.read(buffer, len))
    return false;
  buffer[len] = '\0';
  return true;
}
