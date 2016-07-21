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


namespace NLP { namespace Config {

template <typename T>
std::istream &
operator>>(std::istream &is, std::vector<T> &vec){
  T temp;
  char delimiter;
  bool commadelimited=0;
   
  vec.resize(0);

  if(!(is >> temp)){
    if(is.eof() && is.fail())
      is.clear(std::ios::eofbit);
    return is; //first token must be of type T
  }
  vec.push_back(temp);
      
  if(!(is >> delimiter)){
    is.clear(std::ios::eofbit);
    return is;
  }

  if(delimiter == ','){
    commadelimited = 1;
  } else {
    is.unget();
  }

  if(!(is>>temp))
    return is;

  vec.push_back(temp);

  do{
    if(commadelimited){
      if(!(is >> delimiter)){
	is.clear(std::ios::eofbit);
	return is;
      }
      if(delimiter!=','){
	return is;
      }
    }
    
    if(!(is >> temp)){
      if(!commadelimited && is.eof())
	is.clear(std::ios::eofbit);
      return is;
    }
    vec.push_back(temp);
  } while(!is.eof());
  
  return is;
}

template <typename T>
std::ostream &
operator<<(std::ostream &os, const std::vector<T> &vec){
  if(vec.size() != 0){
    os << vec[0];

    for(size_t i = 1; i < vec.size(); ++i)
      os << ' ' << vec[i];
  }

  return os;
}

} }
