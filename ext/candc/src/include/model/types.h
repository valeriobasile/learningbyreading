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

// NLP::Model::Types
// feature types configuration object used to query
// the set of features that are used for the taggers
// used in both the extraction code and the tagging
// code

// also stores the Type values used to disambiguate
// different feature types in the AffixAttributes and
// UniAttributes hash tables used for looking up features
// in the taggers

namespace NLP {
  namespace Model {

    using namespace NLP::Config;

    typedef ulong TypeIndex;

    struct Type {
      const char *desc;
      const char *id;
      TypeIndex index;
      ulong flags;
    };

    class Types: public Cfg {
    public:
      // null type (used in the Extract::Classifier)
      const static Type null;

      // word types
      const static Type w;
      const static Type pw;
      const static Type ppw;
      const static Type pppw;
      const static Type nw;
      const static Type nnw;
      const static Type nnnw;

      // word bigram types
      const static Type pppw_ppw_b;
      const static Type ppw_pw_b;
      const static Type pw_w_b;
      const static Type pw_nw_b;
      const static Type w_nw_b;
      const static Type nw_nnw_b;
      const static Type nnw_nnnw_b;

      // word trigram types
      const static Type pppw_ppw_pw_c;
      const static Type ppw_pw_w_c;
      const static Type pw_w_nw_c;
      const static Type w_nw_nnw_c;
      const static Type nw_nnw_nnnw_c;
      
      // tag types
      const static Type t;
      const static Type pt;
      const static Type ppt;
      const static Type pppt;
      const static Type nt;
      const static Type nnt;
      const static Type nnnt;

      // tag bigram types
      const static Type pppt_ppt_b;
      const static Type ppt_pt_b;
      const static Type pt_t_b;
      const static Type pt_nt_b;
      const static Type t_nt_b;
      const static Type nt_nnt_b;
      const static Type nnt_nnnt_b;
      const static Type nnnt_nnnnt_b;

      // tag trigram types
      const static Type pppt_ppt_pt_c;
      const static Type ppt_pt_t_c;
      const static Type pt_t_nt_c;
      const static Type t_nt_nnt_c;
      const static Type nt_nnt_nnnt_c;
      const static Type nnt_nnnt_nnnnt_c;

      // chunk types
      const static Type c;
      const static Type pc;
      const static Type ppc;
      const static Type nc;
      const static Type nnc;

      // NE types
      const static Type ne;
      const static Type pne;
      const static Type ppne;
      const static Type nne;
      const static Type nnne;

      // supertag types
      const static Type st;
      const static Type pst;
      const static Type ppst;
      const static Type nst;
      const static Type nnst;

      // history types
      const static Type pk;
      const static Type ppk;
      const static Type ppkpk;

      // prefix and suffix types
      const static Type suff;
      const static Type pref;

      // orthographic types
      const static Type has_digit;
      const static Type has_hyphen;
      const static Type has_period;
      const static Type has_punct;
      const static Type has_uppercase;
      const static Type kase;
      const static Type digits;
      const static Type number;
      const static Type alphanum;
      const static Type length;
      const static Type roman;
      const static Type initial;
      const static Type acronym;

      // gazetteer types
      const static Type gaz_common;
      const static Type gaz_first;
      const static Type gaz_last;
      const static Type gaz_loc;
      const static Type pgaz_common;
      const static Type pgaz_first;
      const static Type pgaz_last;
      const static Type pgaz_loc;
      const static Type ngaz_common;
      const static Type ngaz_first;
      const static Type ngaz_last;
      const static Type ngaz_loc;

      // gazetteer and sentence position types
      const static Type xcommon_bs;
      const static Type xcommon_ms;

      // wordtype types
      const static Type wt;
      const static Type pwt;
      const static Type ppwt;
      const static Type nwt;
      const static Type nnwt;

      // bigram wordtype types
      const static Type ppbt;
      const static Type pbt;
      const static Type bt;
      const static Type nbt;
      const static Type nnbt;

      // trigram wordtype types
      const static Type ptt;
      const static Type ntt;

      // miscellaneous special types
      const static Type last;

      const static Type nu;
      const static Type nnu;

      // be disambiguated on the attribute values itself
      const static Type m;
    public:
      // Ratnaparkhi POS tagger features (plus postags and chunktags)
      Op<bool> use_words;
      Op<bool> use_history;
      Op<bool> use_pos;
      Op<bool> use_chunks;
      Op<bool> use_prefix;
      Op<bool> use_suffix;

      Op<bool> use_has_uppercase;
      Op<bool> use_has_hyphen;
      Op<bool> use_has_digit;
      Op<bool> use_has_punct;
      Op<bool> use_has_period;

      // Borthwick's features (plus a couple of other digit features)
      Op<bool> use_one_digit;
      Op<bool> use_two_digits;
      Op<bool> use_three_digits;
      Op<bool> use_four_digits;
      Op<bool> use_digits;

      Op<bool> use_number;
      Op<bool> use_case;
      Op<bool> use_alphanum;

      Op<bool> use_length;
      Op<bool> use_roman;
      Op<bool> use_initial;
      Op<bool> use_acronym;

      Op<bool> use_last;

      Op<bool> use_gazetteers;
      Op<bool> use_gaz_common;
      Op<bool> use_gaz_first;
      Op<bool> use_gaz_last;
      Op<bool> use_gaz_loc;

      Op<bool> use_prev_gazetteers;
      Op<bool> use_next_gazetteers;

      Op<bool> use_nu;
      Op<bool> use_nnu;
      Op<bool> use_wordtypes;
      Op<bool> use_bitypes;
      Op<bool> use_tritypes;

      Op<bool> use_composites;

      Types(const std::string &parent);
      virtual ~Types(void);
    };
  }
}
