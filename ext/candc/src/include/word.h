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

  class Word {
  private:
    typedef ulong ID;

    ID id_;

    NLP::Hash hash_(void) const;
    ulong freq_(void) const;
    ulong index_(void) const;
    const char *str_(void) const;
  public:
    Word(void): id_(0){}
    Word(None): id_(0){}
    Word(Sentinel): id_(1){}

    explicit Word(ulong id)
      : id_(id){}

    Word(const Word &other)
      : id_(other.id_){}
    Word &operator =(const Word other){
      id_ = other.id_;
      return *this;
    }

    operator ulong(void) const{
      return id_;
    }

    NLP::Hash hash(void) const{
      return (id_ < 2) ? Hash(id_) : hash_();
    }

    ulong freq(void) const{
      return (id_ < 2) ? 0 : freq_();
    }

    const char *str(void) const {
      switch(id_){
      case 0: return None::str;
      case 1: return Sentinel::str;
      default: return str_();
      }
    }

    ulong index(void) const {
      switch(id_){
      case 0: return None::val;
      case 1: return Sentinel::val;
      default: return index_() + 2;
      }
    }

    friend bool operator ==(Word w1, Word w2);
    friend bool operator ==(Word w, None);
    friend bool operator ==(Word w, Sentinel);

    friend bool operator !=(Word w1, Word w2);
    friend bool operator !=(Word w, None);
    friend bool operator !=(Word w, Sentinel);
  };

  inline bool operator ==(Word w1, Word w2){ return w1.id_ == w2.id_; }
  inline bool operator ==(Word w, None){ return (int)w.id_ == 0; }
  inline bool operator ==(Word w, Sentinel){ return (int)w.id_ == 1; }

  inline bool operator !=(Word w1, Word w2){ return w1.id_ != w2.id_; }
  inline bool operator !=(Word w, None){ return (int)w.id_ != 0; }
  inline bool operator !=(Word w, Sentinel){ return (int)w.id_ != 1; }
}
