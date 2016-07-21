// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include <limits>
#include <vector>
#include "utils/offset_vector.h"

#include <iostream>

int
main(void){
  offset_vector<int, 2, 3> vec;

  vec.push_back(-1);
  vec.push_back(-1);

  std::cout << vec.size() << std::endl;

  vec.push_back(0);
  vec.push_back(1);
  vec.push_back(2);
  vec.push_back(3);
  vec.push_back(4);

  std::cout << vec.size() << std::endl;

  vec.push_back(-2);
  vec.push_back(-2);
  vec.push_back(-2);

  std::cout << vec.size() << std::endl;

  std::cout << vec[-1] << std::endl;
  std::cout << vec[0] << std::endl;
  std::cout << vec[4] << std::endl;
  std::cout << vec[5] << std::endl;

  return 0;
}
