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


namespace NLP {
  namespace Relations {

    class Relations: public HashTable::Count<CountLexeme,LEX_LARGE,POOL_HUGE> {
    public:
      Hash hash_rel(const char *w1, const char *w2, const char *rel){
        Hash hash = HASH_INIT;
        hash += w1;
        hash += ' ';
        hash += w2;
        if(rel){
          hash += ' ';
          hash += rel;
        }
        return hash;
      }

    Hash hash_rel(const char *w1, const char *w2, char rel){
      Hash hash = HASH_INIT;
      strhash(hash, w1);
      charhash(hash, ' ');
      strhash(hash, w2);
      charhash(hash, ' ');
      charhash(hash, rel);
      return hash;
    }

    Hash hash_inv(const char *w1, const char *w2, const char *rel){
      Hash hash = hash_rel(w1, w2, rel);
      if(rel)
        charhash(hash, '*');
      return hash;
    }
    Hash hash_inv(const char *w1, const char *w2, char rel){
      Hash hash = hash_rel(w1, w2, rel);
      charhash(hash, '*');
      return hash;
    }

    Relations(void): CountLexicon<CountLexeme,LEX_LARGE,POOL_HUGE>("relations") {};

    CountLexeme *find(const char *w1, const char *w2, char *rel){
      Hash hash = hash_rel(w1, w2, rel);
      CountLexeme *bucket = _buckets[hash % NBUCKETS];

      if(bucket)
        return bucket->find(hash);
      return NULL;
    }

    void add(const char *w1, const char *w2, const char *rel, ulong freq = 1);
    void add(const char *w1, const char *w2, const char rel, ulong freq = 1);
    void add(const char *w1, const char *t1, const char *w2, const char *t2, const char *rel, ulong freq = 1);
    void add(const char *w1, const char *t1, const char *w2, const char *t2, const char rel, ulong freq = 1);
    void addinv(const char *w2, const char *w1, const char *rel, ulong freq = 1);
    void addinv(const char *w2, const char *w1, const char rel, ulong freq = 1);
  };

void
Relations::add(const char *w1, const char *w2, const char *rel, ulong freq){
  static char buf[1024];

  Hash hash = hash_rel(w1, w2, rel);
  CountLexeme *lexeme = _buckets[hash % NBUCKETS];
  if(lexeme && (lexeme = lexeme->find(hash)) != NULL)
    return lexeme->inc(freq);

  if(nlexemes == _slexemes)
    _resize();

  char *b = buf;
  for(const char *s = w1; *s; s++)
    *b++ = *s;
  *b++ = ' ';
  for(const char *s = w2; *s; s++)
    *b++ = *s;
  if(rel){
    *b++ = ' ';
    for(const char *s = rel; *s; s++)
      *b++ = *s;
  }
  *b++ = '\0';

  char *lstr = _pool->strdup(buf, b - buf);
  lexeme = new (_lpool) CountLexeme(lstr, nlexemes, hash, freq, _buckets[hash % NBUCKETS]);
  lexemes[nlexemes++] = _buckets[hash % NBUCKETS] = lexeme;
}

void
Relations::add(const char *w1, const char *w2, const char rel, ulong freq){
  static char buf[1024];

  Hash hash = hash_rel(w1, w2, rel);
  CountLexeme *lexeme = _buckets[hash % NBUCKETS];
  if(lexeme && (lexeme = lexeme->find(hash)) != NULL)
    return lexeme->inc(freq);

  if(nlexemes == _slexemes)
    _resize();

  char *b = buf;
  for(const char *s = w1; *s; s++)
    *b++ = *s;
  *b++ = ' ';
  for(const char *s = w2; *s; s++)
    *b++ = *s;
  if(rel){
    *b++ = ' ';
    *b++ = rel;
  }
  *b++ = '\0';

  char *lstr = _pool->strdup(buf, b - buf);
  lexeme = new (_lpool) CountLexeme(lstr, nlexemes, hash, freq, _buckets[hash % NBUCKETS]);
  lexemes[nlexemes++] = _buckets[hash % NBUCKETS] = lexeme;
}

void
Relations::addinv(const char *w2, const char *w1, const char *rel, ulong freq){
  static char buf[1024];

  Hash hash = hash_inv(w1, w2, rel);
  CountLexeme *lexeme = _buckets[hash % NBUCKETS];
  if(lexeme && (lexeme = lexeme->find(hash)) != NULL)
    return lexeme->inc(freq);

  if(nlexemes == _slexemes)
    _resize();

  char *b = buf;
  for(const char *s = w1; *s; s++)
    *b++ = *s;
  *b++ = ' ';
  for(const char *s = w2; *s; s++)
    *b++ = *s;
  *b++ = ' ';
  if(rel){
    for(const char *s = rel; *s; s++)
      *b++ = *s;
    *b++ = '*';
  }
  *b++ = '\0';

  char *lstr = _pool->strdup(buf, b - buf);
  lexeme = new (_lpool) CountLexeme(lstr, nlexemes, hash, freq, _buckets[hash % NBUCKETS]);
  lexemes[nlexemes++] = _buckets[hash % NBUCKETS] = lexeme;
}

void
Relations::addinv(const char *w2, const char *w1, const char rel, ulong freq){
  static char buf[1024];

  Hash hash = hash_inv(w1, w2, rel);
  CountLexeme *lexeme = _buckets[hash % NBUCKETS];
  if(lexeme && (lexeme = lexeme->find(hash)) != NULL)
    return lexeme->inc(freq);

  if(nlexemes == _slexemes)
    _resize();

  char *b = buf;
  for(const char *s = w1; *s; s++)
    *b++ = *s;
  *b++ = ' ';
  for(const char *s = w2; *s; s++)
    *b++ = *s;
  if(rel){
    *b++ = ' ';
    *b++ = rel;
    *b++ = '*';
  }
  *b++ = '\0';

  char *lstr = _pool->strdup(buf, b - buf);
  lexeme = new (_lpool) CountLexeme(lstr, nlexemes, hash, freq, _buckets[hash % NBUCKETS]);
  lexemes[nlexemes++] = _buckets[hash % NBUCKETS] = lexeme;
}

} }
