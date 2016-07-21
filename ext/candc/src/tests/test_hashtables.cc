// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "pool.h"
#include "hashtable/size.h"
#include "hashtable/entry.h"
#include "hashtable/base.h"
#include "hashtable/ordered.h"

using namespace std;

using namespace NLP::HashTable;

Base<Entry<ulong>,SMALL,LARGE> base("blah");
Ordered<Entry<ulong>,SMALL,LARGE> base2("blah2");

int main(void){

  cout << sizeof(Entry<ulong>) << endl;
  cout << sizeof(Entry<double>) << endl;

  std::string line;
  while(getline(cin, line)){
    base.add(line);
    base2.add(line);
  }
  base2.save(cout);
  return 0;
}
