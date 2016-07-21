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
  namespace CCG {

    using namespace NLP::Tree;

    class DistFeatureType: public FeatureType {
    public:
      DistFeatureType(void){}
      virtual ~DistFeatureType(void){ /* do nothing */ }

      ushort calc_dist(Type type, Position l, Position r, const Words &tags) const {
	ushort nverbs = 0;
	ushort npunct = 0;

	ushort start = l - 1;
	ushort end = r - 1;
  
	if(type == DIST_ADJ_HEAD || type == DIST_ADJ_POS){
	  ushort words = end - start - 1;
	  if(words > 2)
	    words = 2;
	  return words;
	}

	else if(type == DIST_VERBS_HEAD  || type == DIST_VERBS_POS){
	  for(ulong i = start + 1; i < end; ++i){
	    const char *t = tags[i].str();
	    if(t[0] == 'V' && nverbs < 1)
	      nverbs++;
	  }
	  return nverbs;
	}

	else if(type == DIST_PUNCT_HEAD || type == DIST_PUNCT_POS){
	  for(ulong i = start + 1; i < end; ++i){
	    const char *t = tags[i].str();
	    if((t[0] == ',' || t[0] == ':' || t[0] == '.' || t[0] == ';') 
	       && npunct < 2)
	      npunct++;
	  }
	  return npunct;
	} 
	else
	  throw NLP::IOException("unexpected type in _calc_dist()");
      }

    };
  }
}
