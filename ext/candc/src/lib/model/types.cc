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

#include "base.h"

#include "config/config.h"

#include "model/types.h"

using namespace std;

namespace NLP { namespace Model {

const Type Types::null = { "null type", "-", 0, 0 };

// word types
// these types share UniAttributes
const Type Types::pw = { "prev word", "pw", 0, 0 };
const Type Types::ppw = { "prev2 word", "ppw", 1, 0 };
const Type Types::pppw = { "prev3 word", "pppw", 2, 0 };
const Type Types::w = { "current word", "w", 3, 0 };
const Type Types::nw = { "next word", "nw", 4, 0 };
const Type Types::nnw = { "next2 word", "nnw", 5, 0 };
const Type Types::nnnw = { "next3 word", "nnnw", 6, 0 };

// word bigram types
// these types use individual BiWordAttributes
const Type Types::pppw_ppw_b = { "prev3 & prev2 words", "pppwppw", 0, 0 };
const Type Types::ppw_pw_b = { "prev2 & prev words", "ppwpw", 1, 0 };
const Type Types::pw_w_b = { "prev & current words", "pww", 2, 0 };
const Type Types::pw_nw_b = { "prev & next words", "pwnw", 3, 0 };
const Type Types::w_nw_b = { "current & next words", "wnw", 4, 0 };
const Type Types::nw_nnw_b = { "next & next2 words", "nwnnw", 5, 0 };
const Type Types::nnw_nnnw_b = { "next2 & next3 words", "nnwnnnw", 6, 0 };

// word trigram types
// these types use individual TriWordAttributes
const Type Types::pppw_ppw_pw_c = { "prev3, prev2 & prev word", "pppwppwpw", 0, 0 };
const Type Types::ppw_pw_w_c = { "prev2, prev1 & current word", "ppwpww", 1, 0 };
const Type Types::pw_w_nw_c = { "prev1, current & next word", "pwwnw", 2, 0 };
const Type Types::w_nw_nnw_c = { "current, next & next2 word", "wnwnnw", 3, 0 };
const Type Types::nw_nnw_nnnw_c = { "next, next2 & next3 word", "nwnnwnnnw", 4, 0 };

// tag types
// these types use individual TagAttributes
const Type Types::pt = { "prev tag", "pt", 0, 0 };
const Type Types::ppt = { "prev2 tag", "ppt", 0, 0 };
const Type Types::pppt = { "prev3 tag", "pppt", 0, 0 };
const Type Types::t = { "current tag", "t", 0, 0 };
const Type Types::nt = { "next tag", "nt", 0, 0 };
const Type Types::nnt = { "next2 tag", "nnt", 0, 0 };
const Type Types::nnnt = { "next3 tag", "nnnt", 0, 0 };

// tag bigram types
// these types use individual BiTagAttributes
const Type Types::pppt_ppt_b = { "prev3 & prev2 tag", "ppptppt", 0, 0 };
const Type Types::ppt_pt_b = { "prev2 & prev tag", "pptpt", 0, 0 };
const Type Types::pt_t_b = { "prev & current tag", "ptt", 0, 0 };
const Type Types::pt_nt_b = { "prev & next tag", "ptnt", 0, 0 };
const Type Types::t_nt_b = { "current & next tag", "tnt", 0, 0 };
const Type Types::nt_nnt_b = { "next & next2 tag", "ntnnt", 0, 0 };
const Type Types::nnt_nnnt_b = { "next2 & next3 tag", "nntnnnt", 0, 0 };
const Type Types::nnnt_nnnnt_b = { "next3 & next4 tag", "nnntnnnnt", 0, 0};

// tag trigram types
// these types use individual TriTagAttributes
const Type Types::pppt_ppt_pt_c = { "prev3, prev2 & prev tag", "ppptpptpt", 0, 0 };
const Type Types::ppt_pt_t_c = { "prev2, prev1 & current tag", "pptptt", 0, 0 };
const Type Types::pt_t_nt_c = { "prev1, current & next tag", "pttnt", 0, 0 };
const Type Types::t_nt_nnt_c = { "current, next & next2 tag", "tntnnt", 0, 0 };
const Type Types::nt_nnt_nnnt_c = { "next, next2 & next3 tag", "ntnntnnnt", 0, 0 };
const Type Types::nnt_nnnt_nnnnt_c = { "next2, next3 & next4 tag", "nntnnntnnnnt", 0, 0 };

// chunk types
// these types use individual TagAttributes
const Type Types::pc = { "prev chunk", "pc", 0, 0 };
const Type Types::ppc = { "prev2 chunk", "ppc", 0, 0 };
const Type Types::c = { "current chunk", "c", 0, 0 };
const Type Types::nc = { "next chunk", "nc", 0, 0 };
const Type Types::nnc = { "next2 chunk", "nnc", 0, 0 };

// NE types
// these types use individual TagAttributes
const Type Types::pne = { "prev ne", "pne", 0, 0 };
const Type Types::ppne = { "prev2 ne", "ppne", 0, 0 };
const Type Types::ne = { "current ne", "ne", 0, 0 };
const Type Types::nne = { "next ne", "nne", 0, 0 };
const Type Types::nnne = { "next2 ne", "nnne", 0, 0 };

// supertag types
// these types use individual TagAttributes
const Type Types::pst = { "prev supertag", "pst", 0, 0 };
const Type Types::ppst = { "prev2 supertag", "ppst", 0, 0 };
const Type Types::st = { "current supertag", "st", 0, 0 };
const Type Types::nst = { "next supertag", "nst", 0, 0 };
const Type Types::nnst = { "next2 supertag", "nnst", 0, 0 };

// history types
// should all eventually be replaces with pk, ppk, and ppkpk
// but for backward compatibility with current models will
// use the values defined above

// these types use individual TagAttributes
const Type Types::pk = { "prev class", "pk", 0, 0 };
const Type Types::ppk = { "prev2 class", "ppk", 0, 0 };

// these types use individual BiTagAttributes
const Type Types::ppkpk = { "prev2 class & prev class", "ppkpk", 0, 0 };

// prefix and suffix types
// these types share AffixAttributes
const Type Types::suff = { "suffix (up to length 4)", "suff", 0, 1 };
const Type Types::pref = { "prefix (up to length 4)", "pref", 1, 1 };

// orthographic types
// these multi valued types share AffixAttributes
// with the suffix/prefix features

const Type Types::kase = { "case (title, mixed, upper, lower)", "case", 2, 1 };
const Type Types::digits = { "only digits (ndigits, 1-4 or more)", "digits", 3, 1 };
const Type Types::length = { "word length (0-15 or more)", "len", 4, 1 };

// orthographic types
// these binary valued types use individual BinAttributes

const Type Types::has_digit = { "contains a digit", "dig", 5, 0 };
const Type Types::has_hyphen = { "contains a hyphen", "hyph", 6, 0 };
const Type Types::has_period = { "contains a period", "per", 7, 0 };
const Type Types::has_punct = { "contains a ispunct", "pun", 8, 0 };
const Type Types::has_uppercase = { "contains an uppercase", "uc", 9, 0 };

const Type Types::number = { "is a number", "num", 10, 0 };
const Type Types::alphanum = { "contains only alphanumeric", "an", 11, 0 };

const Type Types::roman = { "contains roman numerals only", "rom", 12, 0 };
const Type Types::initial = { "is an initial", "i", 13, 0 };
const Type Types::acronym = { "is an acronym", "acr", 14, 0 };

// gaz types
// these types use individual BinAttributes
const Type Types::gaz_common = { "in common gaz", "gc", 0, 0 };
const Type Types::gaz_first = { "in firstname gaz", "gf", 1, 0 };
const Type Types::gaz_last = { "in lastname gaz", "gs", 2, 0 };
const Type Types::gaz_loc = { "in location gaz", "gl", 3, 0 };

const Type Types::pgaz_common = { "prev in common gaz", "gpc", 4, 0 };
const Type Types::pgaz_first = { "prev in firstname gaz", "gpf", 5, 0 };
const Type Types::pgaz_last = { "prev in lastname gaz", "gps", 6, 0 };
const Type Types::pgaz_loc = { "prev in location gaz", "gpl", 7, 0 };

const Type Types::ngaz_common = { "next in common gaz", "gnc", 8, 0 };
const Type Types::ngaz_first = { "next in firstname gaz", "gnf", 9, 0 };
const Type Types::ngaz_last = { "next in lastname gaz", "gns", 10, 0 };
const Type Types::ngaz_loc = { "next in location gaz", "gnl", 11, 0 };

// gaz and sentence position types
const Type Types::xcommon_bs = { "in common gaz & begins sentence", "xbs", 12, 0 };
const Type Types::xcommon_ms = { "in common gaz & not begins sentence", "xms", 13, 0 };

// wordtype features (a la Collins)
// these types share a separate UniAttributes

const Type Types::wt = { "wordtype", "wt", 0, 1 };
const Type Types::pwt = { "prev wordtype", "pwt", 1, 1 };
const Type Types::ppwt = { "prev2 wordtype", "ppwt", 2, 1 };
const Type Types::nwt = { "next wordtype", "nwt", 3, 1 };
const Type Types::nnwt = { "next2 wordtype", "nnwt", 4, 1 };

const Type Types::pbt = { "prev & current wordtype", "pbt", 5, 1 };
const Type Types::ppbt = { "prev2 & prev wordtype", "ppbt", 6, 1 };
const Type Types::bt = { "prev & next wordtype", "bt", 7, 1 };
const Type Types::nbt = { "current & next wordtype", "nbt", 8, 1 };
const Type Types::nnbt = { "next & next2 wordtype", "nnbt", 9, 1 };

const Type Types::ptt = { "prev3 & prev2 & prev wordtype", "ptt", 10, 1 };
const Type Types::ntt = { "next & next2 & next3 wordtype", "ntt", 11, 1 };

// miscellaneous special types

const Type Types::last = { "last ne tag assigned to word", "last", 0, 0 };

const Type Types::nu = { "(most frequent) unigram tag of next", "nu", 0, 0 };
const Type Types::nnu = { "(most frequent) unigram tag of next2", "nnu", 0, 0 };

// type for experimental feature
// note there is only one type so multiple columns will need to
// be disambiguated on the attribute values itself
const Type Types::m = { "experimental feature", "m", 12, 0 };

Types::Types(const std::string &parent)
  : Cfg("types", parent + " feature types", SPACE),

    use_words(*this, "use_words", "use_words features", true),
    use_history(*this, "use_history", "use_history features", true),
    use_pos(*this, "use_pos", "use_pos features", true),
    use_chunks(*this, "use_chunks", "use_chunks features", false),
    use_prefix(*this, "use_prefix", "use_prefix features", true),
    use_suffix(*this, "use_suffix", "use_suffix features", true),

    use_has_uppercase(*this, SPACE, "use_has_uppercase", "use_has_uppercase features", true),
    use_has_hyphen(*this, "use_has_hyphen", "use_has_hyphen features", true),
    use_has_digit(*this, "use_has_digit", "use_has_digit features", true),
    use_has_punct(*this, "use_has_punct", "use_has_punct features", true),
    use_has_period(*this, "use_has_period", "use_has_period features", true),

    use_one_digit(*this, SPACE, "use_one_digit", "use_one_digit features", true),
    use_two_digits(*this, "use_two_digits", "use_two_digits features", true),
    use_three_digits(*this, "use_three_digits", "use_three_digits features", true),
    use_four_digits(*this, "use_four_digits", "use_four_digits features", true),
    use_digits(*this, "use_digits", "use_digits features", true),
    use_number(*this, "use_number", "use_number features", true),
    use_case(*this, "use_case", "use_case features", true),
    use_alphanum(*this, "use_alphanum", "use_alphanum features", true),

    use_length(*this, SPACE, "use_length", "use_length features", true),
    use_roman(*this, "use_roman", "use_roman features", true),
    use_initial(*this, "use_initial", "use_initial features", true),
    use_acronym(*this, "use_acronym", "use_acronym features", true),
    use_last(*this, "use_last", "use_last features", true),

    use_gazetteers(*this, SPACE, "use_gazetteers", "use_gazetteers features", true),
    use_gaz_common(*this, "use_gaz_common", "use_gaz_common features", true),
    use_gaz_first(*this, "use_gaz_first", "use_gaz_first features", true),
    use_gaz_last(*this, "use_gaz_last", "use_gaz_last features", true),
    use_gaz_loc(*this, "use_gaz_loc", "use_gaz_loc features", true),
    use_prev_gazetteers(*this, "use_prev_gazetteers", "use_prev_gazetteers features", true),
    use_next_gazetteers(*this, "use_next_gazetteers", "use_next_gazetteers features", true),

    use_nu(*this, SPACE, "use_nu", "use_nu features", true),
    use_nnu(*this, "use_nnu", "use_nnu features", false),
    use_wordtypes(*this, "use_wordtypes", "use_wordtypes features", true),
    use_bitypes(*this, "use_bitypes", "use_bitypes features", true),
    use_tritypes(*this, "use_tritypes", "use_tritypes features", true),

    use_composites(*this, "use_composites", "use_composites features", true){}

Types::~Types(void){}

} }

