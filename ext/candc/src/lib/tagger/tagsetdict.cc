// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Tagger::TagSetDict
// for a given word stores the tags the tagger is permitted to
// hypothesise (i.e. assign) for that word

// the set of permissible tags are represented as a vector
// of Tag objects

#include "base.h"

#include "tagger/tagsetdict.h"

#include "share.h"

using namespace std;

namespace NLP { namespace Taggers {

class TagSetDict::_Impl: public Shared {
public:
  std::string name;
  TagSet tagset;
  TagSet lexicon;
  std::string PREFACE;

  std::vector<Tags> table;

  _Impl(const string &name, const string &filename,
        const TagSet &tagset, const TagSet &lexicon):
      name(name), tagset(tagset), lexicon(lexicon), table(lexicon.size()) {
    load(filename);
  };

  _Impl(const string &name, const string &filename,
	ulong min, const TagSet &tagset, const TagSet &lexicon):
      name(name), tagset(tagset), lexicon(lexicon), table(lexicon.size()) {
    load(filename, min);
  };
  virtual ~_Impl(void) {};

  void load(const std::string filename, ulong min = 0){
    ulong nlines = 0;
    ifstream stream(filename.c_str());
    if(!stream)
      throw IOException("could not open wordtags file", filename);

    PREFACE += read_preface(filename, stream, nlines);

    std::string word_str, tag_str;
    ulong freq;
    while(stream >> word_str >> tag_str >> freq){
      ++nlines;
      if(stream.get() != '\n')
	throw IOException("expected newline after frequency in tagsetdict file", filename, nlines);

      if(freq < min)
	continue;

      // TODO: should we complain here if the word/tag are unknown?
      Tag word = lexicon[word_str];
      if(!word)
	continue;

      Tag tag = tagset[tag_str];
      if(!tag)
	continue;

      table[word.value()].push_back(tag);
    }

    if(!stream.eof())
      throw IOException("could not parse class", filename, nlines);
  };
};

TagSetDict::TagSetDict(const std::string &name, const std::string &filename,
		       const TagSet &tagset, const TagSet &lexicon):
    _impl(new _Impl(name, filename, tagset, lexicon)){
}

TagSetDict::TagSetDict(const std::string &name, const std::string &filename,
		       ulong min, const TagSet &tagset, const TagSet &lexicon):
    _impl(new _Impl(name, filename, min, tagset, lexicon)){
}

TagSetDict::TagSetDict(const TagSetDict &other): _impl(share(other._impl)){}

TagSetDict::~TagSetDict(void){
  release(_impl);
}

TagSetDict &
TagSetDict::operator=(TagSetDict &other){
  if(_impl != other._impl){
    release(_impl);
    _impl = share(other._impl);
  }

  return *this;
}

const std::string
TagSetDict::name(void) const {
  return _impl->name;
}

size_t
TagSetDict::size(void) const {
  return _impl->table.size();
}

Tags &
TagSetDict::tags(Tag word) const {
  return _impl->table[word.value()];
}

bool
TagSetDict::exists(Tag word, Tag tag) const {
  const Tags &tags = _impl->table[word.value()];
  return std::find(tags.begin(), tags.end(), tag) != tags.end();
}

} }
