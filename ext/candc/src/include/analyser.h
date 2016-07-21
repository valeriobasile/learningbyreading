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

// NLP::Analyser
// service class for identifying orthographic features of a string
// e.g. is alphabetic, is uppercase, is numeric
// identifies several features in a single pass over the string
// using counts of each character class

namespace NLP {

  class Analyser {
  protected:
    // runs over the string counting character class membership
    void _count(const std::string &word){
      // reset counts
      nupper = 0;
      nlower = 0;
      ndigits = 0;
      nother = 0;

      npunct = 0;
      nhyphens = 0;
      ncommas = 0;
      nperiods = 0;
      nuroman = 0;
      nlroman = 0;

      // set initial character flags
      fupper = isupper(word[0]);
      fhyphen = word[0] == '-';

      // loop over the string and add to the character class counters
      for(uint i = 0; i < len; i++){
        // use isxxx functions rather than a case statement because
        // they are more portable.
        // a case statement would be faster though
        if(islower(word[i])){
          nlower++;
          // lowercase Roman numeral characters
          switch(word[i]){
            case 'c':
            case 'd':
            case 'i':
            case 'l':
            case 'm':
            case 'v':
            case 'x': nlroman++;
          }
        }else if(isupper(word[i])){
          nupper++;
	  // uppercase Roman numeral characters
          switch(word[i]){
            case 'C':
            case 'D':
            case 'I':
            case 'L':
            case 'M':
            case 'V':
            case 'X': nuroman++;
          }
        }else if(isdigit(word[i]))
          ndigits++;
        else if(ispunct(word[i])){
          npunct++;
          // specific punctuation counters
          switch(word[i]){
            case '-': nhyphens++; break;
            case ',': ncommas++; break;
            case '.': nperiods++; break;
          }
        }else
          nother++;
      }
      nalpha = nlower + nupper;
    };

  public:
    ulong len;		// length of the current string

    ulong nupper;	// number of uppercase characters (using isupper)
    ulong nlower;	// number of lowercase characters (using islower)
    ulong nalpha;	// nupper + nlower

    ulong ndigits;	// number of digit characters (using isdigit)
    ulong npunct;	// number of punctuation characters (using ispunct)
    ulong nother;	// anything not in the previous character classes

    ulong nhyphens;	// number of hyphens
    ulong ncommas;	// number of commas
    ulong nperiods;	// number of periods
    ulong nuroman;	// number of uppercase Roman letters (CDILMVX)
    ulong nlroman;	// number of lowercase Roman letters (cdilmvx)

    bool fupper;	// is the first character uppercase
    bool fhyphen;	// is the first character a hyphen

    // analyse a string
    Analyser(const std::string &word): len(word.size()) { _count(word); };

    // analyse a specific length prefix of a string
    Analyser(const std::string &word, ulong len): len(len) { _count(word); };

    // word is all digits
    bool is_digits(void) const { return ndigits == len; };
    // word is all alphabetic
    bool is_alpha(void) const { return nalpha == len; };
    // word is all alphanumeric
    bool is_alphanum(void) const { return nalpha + ndigits == len; };
    // word is probably a number (rough match)
    bool is_number(void) const { return (fhyphen + ndigits + nperiods + ncommas) == len; };
    // word is an initial of form X.
    bool is_initial(void) const { return fupper + nperiods == 2; };
    // word consists of only roman numerals
    bool is_roman(void) const { return nuroman == len || nlroman == len; };
    // word is probably an acronym (rough match)
    bool is_acronym(void) const { return fupper && (nperiods + nupper) == len; };

    // word is uppercase
    bool uc(void) const { return nupper == len; };
    // word is titlecase, i.e. of form Xxxxx
    bool tc(void) const { return fupper && nlower == len - 1; };
    // word is mixed case (subsumes titlecase)
    bool mc(void) const { return (nupper + nlower) == len; };
    // word is lowercase
    bool lc(void) const { return nlower == len; };
  };

}
