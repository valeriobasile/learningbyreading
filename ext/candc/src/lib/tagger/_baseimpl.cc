// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Tagger::_BaseImpl
// provides some common services for the tagger classes
// including the NLP::Tagger::State class which holds the
// Beam search lattice representation, the tagger buffer
// and some frequently used temporary probability distribution
// arrays 

#include "tagger/_baseimpl.h"

using namespace NLP::Model;

namespace NLP { namespace Taggers {

void
Tagger::Impl::reg_attributes(void){
  registry.reg(Types::pppw, w_attribs);
  registry.reg(Types::ppw, w_attribs);
  registry.reg(Types::pw, w_attribs);
  registry.reg(Types::w, w_attribs);
  registry.reg(Types::nw, w_attribs);
  registry.reg(Types::nnw, w_attribs);
  registry.reg(Types::nnnw, w_attribs);

  registry.reg(Types::pppw_ppw_b, ww_attribs);
  registry.reg(Types::ppw_pw_b, ww_attribs);
  registry.reg(Types::pw_w_b, ww_attribs);
  registry.reg(Types::pw_nw_b, ww_attribs);
  registry.reg(Types::w_nw_b, ww_attribs);
  registry.reg(Types::nw_nnw_b, ww_attribs);
  registry.reg(Types::nnw_nnnw_b, ww_attribs);

  registry.reg(Types::pppw_ppw_pw_c, www_attribs);
  registry.reg(Types::ppw_pw_w_c, www_attribs);
  registry.reg(Types::pw_w_nw_c, www_attribs);
  registry.reg(Types::w_nw_nnw_c, www_attribs);
  registry.reg(Types::nw_nnw_nnnw_c, www_attribs);
}

// model/features is read into the features vector
// and we create the mapping between attribute identifiers
// and feature pointers (Attrib2Feats)
void
Tagger::Impl::_read_features(const Model::Model &cfg, const Model::Info &info,
                             Attrib2Feats &attrib2feats){
  features.reserve(info.nfeatures());
  attrib2feats.reserve(info.nattributes() + 1);

  ifstream stream(cfg.weights().c_str());
  if(!stream)
    throw NLP::IOException("could not open weights file", cfg.weights());

  ulong nlines = 0;
  read_preface(cfg.weights(), stream, nlines);

  ulong previous = static_cast<ulong>(-1);
  ulong klass, attrib;
  double lambda;
  while(stream >> klass >> attrib >> lambda){
    ++nlines;
    features.push_back(Feature(klass, static_cast<float>(exp(lambda))));

    if(attrib != previous){
      attrib2feats.push_back(&features.back());
      previous = attrib;
    }
  }

  if(!stream.eof())
    throw NLP::IOException("could not parse feature tuple", cfg.weights(), nlines);

  if(features.size() != info.nfeatures())
    throw NLP::IOException("number of features read != configuration value", cfg.weights(), nlines);

  attrib2feats.push_back(&features.back() + 1);
  if(attrib2feats.size() != info.nattributes() + 1)
    throw NLP::IOException("number of attributes read != configuration value", cfg.weights(), nlines);
}

void
Tagger::Impl::_read_attributes(const Model::Model &cfg, const Model::Info &info,
                               vector<Feature *> &attrib2feats){
  ulong nlines = 0;
  std::string filename = cfg.attributes();
  try {
    ulong id = 0;
    // now the contextual elements
    ifstream stream(filename.c_str());
    if(!stream)
      throw NLP::IOException("could not open attributes file", filename);

    read_preface(filename, stream, nlines);

    std::string type;
    ulong freq = 0;
    while(stream >> type){
      ++nlines;

      if(id >= info.nattributes())
        throw NLP::IOException("inconsistent attribute index (>= nattributes)", filename, nlines);

      Attribute &attrib = registry.load(type, stream);

      if(!(stream >> freq)) {
      	stream >> type;
      	printf("failed to read freq value - got: %s\n", type.c_str());
        break;
			}
      if(stream.get() != '\n')
        throw IOException("expected a newline after the attribute", filename, nlines);

      attrib.begin = attrib2feats[id];
      attrib.end = attrib2feats[id + 1];

      ++id;
    }

    if(!stream.eof())
      throw NLP::IOException("could not parse attribute", filename, nlines);

    if(id != info.nattributes())
      throw NLP::IOException("number of attributes read != info file value", filename, nlines);

  }catch(NLP::Exception e){
    throw NLP::IOException(e.msg, filename, nlines);
  }
}

Tagger::Impl::Impl(const std::string &name, Tagger::Config &cfg)
  : name(name),
    klasses("klasses", cfg.model.klasses()),
    lexicon("lexicon", cfg.model.lexicon()),
    tagdict("tagdict", cfg.tagdict(), cfg.tagdict_ratio(), cfg.tagdict_min(), klasses, lexicon),
    registry("registry"),
    pk_attribs(klasses), ppkpk_attribs(klasses),
    w_attribs(lexicon),
    ww_attribs(lexicon),
    www_attribs(lexicon),
    rare_cutoff(cfg.rare_cutoff()),
    beam_width(cfg.beam_width()), beam_ratio(log(cfg.beam_ratio())),
    forward_beam_ratio(cfg.forward_beam_ratio()),
    maxwords(cfg.maxwords()){}

Tagger::Impl::~Impl(void){}

} }
