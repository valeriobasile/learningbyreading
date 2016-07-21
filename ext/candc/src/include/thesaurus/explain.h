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
  namespace Thesaurus {

template <class V>
struct __stats {
  const V &val;

  __stats(const V &val): val(val) {};
};

template <class V>
__stats<V> stats(const V &value){ return __stats<V>(value); };

template <class V>
inline std::ostream &
operator <<(std::ostream &out, const __stats<V> &s){
  s.val.printstats(out);
  return out;
}

inline void
print_equal(std::ostream &out, Relations::const_iterator i, Relations::const_iterator j){
  out << "  " << i->str() << ' ';
  out << setprecision(4) << i->score() << ' ' << j->score();
  out << "    " << stats(*i) << endl;
  out << "    " << stats(*j) << endl;
}

inline void
print_unequal(std::ostream &out, const Object *obj, Relations::const_iterator i){
  out << "  " << i->str() << ' ' << setprecision(4) << i->score();
  out << " (only " << obj->str() << ")\n";
}

inline void
print_common(std::ostream &out, ulong n, float num, float denom){
  out << n << " terms in common giving ";
  out << setprecision(4) << num << '/' << denom << " = " << div0(num, denom) << endl;
}

inline void
print_unique(std::ostream &out, const Object *obj, ulong n, float score){
  out << n << " terms unique to " << obj->str();
  out << " giving " << setprecision(4) << score << endl;
}

inline void
print_score(std::ostream &out, const char *name, float num, float denom){
  out << name << ' ' << setprecision(4) << num << '/' << denom;
  out << " = " << div0(num, denom) << endl;
}

inline void
print_failed(std::ostream &out, const Object *obj1, const Object *obj2){
  out << obj1->str() << " and " << obj2->str();
  out << " share no terms in common\n";
}

  }
}
