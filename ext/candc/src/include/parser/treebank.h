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

    class TBNode {
    public:
      static const ushort INTERNAL = 0;
      static const ushort LEAF = 1;
    public:
      // sc: added this so can get markedup version (would be better
      // to just write a function that removes outer brackets)
      std::string catNoBrack;
      std::string cat;
      std::string pos;
      std::string word;
      ushort type;
      ushort nchildren;

      TBNode(void) {};
      TBNode(const std::string &line);
      TBNode(const TBNode &other):
        catNoBrack(other.catNoBrack), cat(other.cat), pos(other.pos), 
	word(other.word), type(other.type), nchildren(other.nchildren) {};
      ~TBNode(void){};

      bool is_internal(void) const { return type == INTERNAL; };
      bool is_leaf(void) const { return type == LEAF; };
    };

    inline std::ostream &operator <<(std::ostream &stream, const TBNode &node){
      stream << node.type << ' ' << node.cat << ' ' << node.pos;
      if(node.type == TBNode::INTERNAL)
        return stream << ' ' << node.nchildren;
      else
        return stream << ' ' << node.word;
    }

    typedef std::vector<TBNode> TBSentence;
  }
}
