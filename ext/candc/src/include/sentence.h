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
    class Cat;
  }

  struct Sentence {
  public:
    typedef std::string FieldNames;

    const static ulong NMISC = 10;

    RawWords words;
    RawWords lemmas;

    RawTags pos;
    RawTags chunks;
    RawTags entities;
    RawTags super;

    MultiRaws mpos;
    MultiRaws mchunks;
    MultiRaws mentities;
    MultiRaws msuper;

    std::vector<const CCG::Cat *> cats;
		CCG::Constraints constraints;

    RawWords misc[NMISC];

    template <class V>
    static void reset_vector(V &v){
      if(!v.empty())
				v.clear();
    }

    void reset(void){
      reset_vector(words);
      reset_vector(lemmas);
      reset_vector(pos);
      reset_vector(chunks);
      reset_vector(entities);
      reset_vector(super);

      reset_vector(mpos);
      reset_vector(mchunks);
      reset_vector(mentities);
      reset_vector(msuper);

      reset_vector(cats);

      for(ulong i = 0; i != NMISC; ++i)
				reset_vector(misc[i]);
    }

    std::string last(void) const {
      if(words.size() > 0)
				return words.back();
      else
				return "";
    }

    static const int TYPE_INVALID = 0;
    static const int TYPE_IGNORE = 1;
    static const int TYPE_OPTIONAL = 2;
    static const int TYPE_SINGLE = 3;
    static const int TYPE_MULTI = 4;

    static int type(char name){
      switch(name){
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9': return TYPE_OPTIONAL;
      case 'w':
      case 'l':
      case 'p':
      case 'c':
      case 'n':
      case 's': return TYPE_SINGLE;
      case 'P':
      case 'C':
      case 'N':
      case 'S': return TYPE_MULTI;
      case '?': return TYPE_IGNORE;
      default: return TYPE_INVALID;
      }
    }

    NLP::Raws &get_single(char name){
      switch(name){
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9': return misc[name - '0'];
      case 'w': return words;
      case 'l': return lemmas;
      case 'p': return pos;
      case 'c': return chunks;
      case 'n': return entities;
      case 's': return super;
      default:
				throw NLP::Exception(std::string("sentence does not have a single-valued '") + name + "' field");
      }
    }

    NLP::MultiRaws &get_multi(char name){
      switch(name){
      case 'P': return mpos;
      case 'C': return mchunks;
      case 'N': return mentities;
      case 'S': return msuper;
      default:
				throw NLP::Exception(std::string("sentence does not have a multi-valued '") + name + "' field");
      }
    }

    void copy_multi(char from, char to){
      const NLP::Raws &single = get_single(from);
      NLP::MultiRaws &multi = get_multi(to);

      multi.resize(single.size());

      for(ulong i = 0; i != single.size(); ++i){
				MultiRaw &mraw = multi[i];
				mraw.clear();
				mraw.push_back(ScoredRaw(single[i], 1.0));
      }
    }

    ulong get_size(char name){
      switch(name){
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9': return misc[name - '0'].size();
      case 'w': return words.size();
      case 'l': return lemmas.size();
      case 'p': return pos.size();
      case 'c': return chunks.size();
      case 'n': return entities.size();
      case 's': return super.size();
      case 'P': return mpos.size();
      case 'C': return mchunks.size();
      case 'N': return mentities.size();
      case 'S': return msuper.size();
      default:
				throw NLP::Exception(std::string("sentence does not have a '") + name + "' field");
      }
    }

    void check_sizes(const FieldNames &fieldnames){
      const ulong LEN = get_size(fieldnames[0]);
      for(FieldNames::const_iterator f = fieldnames.begin(); f != fieldnames.end(); ++f)
				if(LEN != get_size(*f))
					throw NLP::Exception(std::string("number of tokens in field '") 
															 + *f + "' does not match field '" + fieldnames[0] + "'");  
    }

  };

}
