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

// NLP::Taggers::_BaseImpl
// provides some common services for the tagger classes
// including the NLP::Tag::State class which holds the
// Beam search lattice representation, the tagger buffer
// and some frequently used temporary probability distribution
// arrays 

#include "base.h"

#include "prob.h"

#include "io/reader.h"
#include "io/writer.h"

#include "affix.h"

#include "config/config.h"

#include "model/model.h"
#include "model/types.h"
#include "model/feature.h"
#include "model/attribute.h"
#include "model/attributes.h"
#include "model/registry.h"

#include "pool.h"
#include "tagger/tagdict.h"
#include "tagger/taghist.h"

#include "tagger/nodepool.h"
#include "tagger/lattice.h"
#include "tagger/flattice.h"
#include "tagger/tagger.h"

#include "tagger/state.h"

#include "timer.h"
#include "share.h"

using namespace std;
using namespace NLP::Model;

namespace NLP { namespace Taggers {

// provides methods common to all of the taggers
// reading in the feature file, code for common
// feature types (i.e. the words and the
// previously assigned tags)

// the reduce function which applies the beam after
// all of the hypothesised tags have been scored

class Tagger::Impl: public Shared {
protected:
  typedef std::vector<Feature *> Attrib2Feats;
  void _read_features(const Model::Model &cfg, const Model::Info &info, Attrib2Feats &attrib2feats);
  void _read_attributes(const Model::Model &cfg, const Model::Info &info, Attrib2Feats &attrib2feats);
  void _read_model(const Tagger::Config &cfg){
    Model::Info info(cfg.model.info());
    std::vector<Feature *> attrib2feats;

    _read_features(cfg.model, info, attrib2feats);
    _read_attributes(cfg.model, info, attrib2feats);
  }

  bool _add_attribute(Attribute attrib, PDF &dist) const;
  void _normalise(PDF &dist) const;

  void _add_history(const Node *prev, PDF &dist) const;
  void _add_history(const FNode *prev, PDF &dist) const;
  void _reduce(State &state) const;

  void _unpack_tags(State &state, Raws &raws, bool RECORD) const;
  void _unpack_mtags(State &state, MultiRaws &mraws, double BETA, bool RECORD) const;

  void _calc_viterbi_word(State &state, ulong i, ulong DICT_CUTOFF) const;
  void _calc_viterbi_probs(State &state, ulong DICT_CUTOFF) const;

  void _build_lattice(State &state, ulong DICT_CUTOFF) const;
  void _calc_weights(State &state) const;
  void _apply_forward_beam(const FNodes &column, double BETA) const;
  void _forward(Flattice &flattice, double BETA) const;
  void _backward(Flattice &flattice) const;

  void _calc_fb_probs(State &state, ulong DICT_CUTOFF, double BETA) const;
  void _calc_noseq_probs(State &state, ulong DICT_CUTOFF, double BETA) const;
  void _calc_greedy_probs(State &state, ulong DICT_CUTOFF, double BETA) const;

  void _tag(Sentence &sent, State &state, Algorithm alg, ulong DICT_CUTOFF) const;
  void _mtag(Sentence &sent, State &state, Algorithm alg, ulong DICT_CUTOFF, double BETA) const;

  static bool is_number(const char c){
    switch(c){
    case '.':
    case ',':
    case ':':
    case '/':
    case '\\':
    case '-':
    case '+': return true;
    default: return isdigit(c);
    }
  }

  static bool is_number(const std::string &s){
    // check first and last are digit characters
    if(isdigit(*s.begin()) && isdigit(*(s.end() - 1))){
      for(std::string::const_iterator i = s.begin(); i != s.end(); ++i)
        if(!is_number(*i))
          return false;

      return true;
    }

    return false;
  }

  void add_surrounding_words(const OffsetWords &words, ulong i, PDF &dist) const;
public:
  const std::string name;

  NLP::TagSet klasses;
  NLP::Lexicon lexicon;
  TagDict tagdict;

  Features features;

  Model::Registry registry;

  TagAttributes pk_attribs;
  BiTagAttributes ppkpk_attribs;
  UniAttributes w_attribs;
  BigramAttributes ww_attribs;
  TrigramAttributes www_attribs;

  const ulong rare_cutoff;

  const ulong beam_width;
  const float beam_ratio;
  const double forward_beam_ratio;

  const ulong maxwords;

  NLP::Tags unknown_klasses;        // tags which can be assigned to previously unseen words

  virtual void reg_attributes(void);
  virtual void create_unknowns(const Tagger::Config &cfg) = 0;
  virtual void can_sentence(const Sentence &sent, State &state) const = 0;
  virtual const Tags &get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const = 0;
  virtual void add_features(const State &state, ulong i, PDF &pdf) const = 0;
  virtual void unpack_tags(State &state, Sentence &sent) const = 0;
  virtual void unpack_mtags(State &state, Sentence &sent, double BETA) const = 0;

  virtual void begin_sentence(State &state) const;
  virtual void begin_document(State *state) const;

  virtual State *create_state(void) const { return new State(klasses.size(), maxwords); }

  void create_model(const Tagger::Config &cfg){
    reg_attributes();
    create_unknowns(cfg);
    _read_model(cfg);
  }

  void tag(Sentence &sent, Algorithm alg, ulong DICT_CUTOFF, State *state = 0) const;
  void tag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
           Algorithm alg, ulong DICT_CUTOFF) const;

  void mtag(Sentence &sent, Algorithm alg, const ulong DICT_CUTOFF, double BETA, State *state = 0) const;
  void mtag(NLP::IO::Reader &reader, NLP::IO::Writer &writer, Algorithm alg,
            const ulong DICT_CUTOFF, double BETA) const;

  Impl(const std::string &name, Tagger::Config &config);
  virtual ~Impl(void);
};

// wrapper function for adding the weights of an attribute
// (i.e. the weights of all of the features with a particular
// attribute) to the current distribution.
inline bool
Tagger::Impl::_add_attribute(Attribute attrib, PDF &dist) const {
  for(const Feature *f = attrib.begin; f != attrib.end; ++f){
    assert(f->klass < dist.size());
    dist[f->klass] *= f->lambda;
  }

  return attrib.begin != 0;
}

// normalise a distribution (i.e. make it into a probability
// distribution by dividing by the total mass).
// skipping the first two elements (for Tag::NONE and Tag::SENTINEL)
inline void
Tagger::Impl::_normalise(PDF &dist) const {
  double Z = 0.0;
  for(PDF::iterator i = dist.begin() + 2; i != dist.end(); ++i)
    Z += *i;

  double invZ = 1.0/Z;
  for(PDF::iterator i = dist.begin() + 2; i != dist.end(); ++i)
    *i *= invZ;
}

// add the surrounding word features
inline void
Tagger::Impl::add_surrounding_words(const OffsetWords &words, ulong i, PDF &dist) const {
  _add_attribute(w_attribs(Types::pw, words[i - 1]), dist);
  _add_attribute(w_attribs(Types::ppw, words[i - 2]), dist);
  _add_attribute(w_attribs(Types::nw, words[i + 1]), dist);
  _add_attribute(w_attribs(Types::nnw, words[i + 2]), dist);

  // TODO: Move these into their own functions
  _add_attribute(ww_attribs(Types::pppw_ppw_b, words[i - 3], words[i - 2]), dist);
  _add_attribute(ww_attribs(Types::ppw_pw_b, words[i - 2], words[i - 1]), dist);
  _add_attribute(ww_attribs(Types::pw_w_b, words[i - 1], words[i]), dist);
  _add_attribute(ww_attribs(Types::pw_nw_b, words[i - 1], words[i + 1]), dist);
  _add_attribute(ww_attribs(Types::w_nw_b, words[i], words[i + 1]), dist);
  _add_attribute(ww_attribs(Types::nw_nnw_b, words[i + 1], words[i + 2]), dist);
  _add_attribute(ww_attribs(Types::nnw_nnnw_b, words[i + 2], words[i + 3]), dist);

  _add_attribute(www_attribs(Types::pppw_ppw_pw_c, words[i - 3], words[i - 2], words[i - 1]), dist);
  _add_attribute(www_attribs(Types::ppw_pw_w_c, words[i - 2], words[i - 1], words[i]), dist);
  _add_attribute(www_attribs(Types::pw_w_nw_c, words[i - 1], words[i], words[i - 1]), dist);
  _add_attribute(www_attribs(Types::w_nw_nnw_c, words[i], words[i + 1], words[i + 2]), dist);
  _add_attribute(www_attribs(Types::nw_nnw_nnnw_c, words[i + 1], words[i + 2], words[i + 3]), dist);
}

// add the previously assigned tag (in the current path through the lattice)
// features (the previous tag and the bigram of the previous two tags)
// this is intimately connected with the size of the Markov window 
inline void
Tagger::Impl::_add_history(const Node *const prev, PDF &dist) const {
  _add_attribute(pk_attribs(prev->id), dist);
  _add_attribute(ppkpk_attribs(prev->pid, prev->id), dist);
}

//added by SC to deal with nodes in the full lattice used for
//multi-tagging
inline void
Tagger::Impl::_add_history(const FNode *const prev, PDF &dist) const {
  _add_attribute(pk_attribs(prev->id), dist);
  _add_attribute(ppkpk_attribs(prev->pid, prev->id), dist);
}

// this gets called on every position in the sentence
// (incrementing i each time it is called)
// it does all of the work for creating the new entries
// in the lattice.  State::reduce then goes through
// and eliminates the weakest entries using the beam
inline void
Tagger::Impl::_calc_viterbi_word(State &state, ulong i, const ulong DICT_CUTOFF) const {
  Lattice &current = state.current;
  NodeMatrix &next = state.next;

  const Tags *permit = &get_permitted(state, i, DICT_CUTOFF);
  assert(permit->size() != 0);

  add_features(state, i, state.dist_word);

  for(Lattice::iterator node = current.begin(); node != current.end(); ++node){
    state.dist = state.dist_word;
    const Node *hist = *node;

    _add_history(hist, state.dist);
    _normalise(state.dist);

    for(Tags::const_iterator t = permit->begin(); t != permit->end(); ++t)
      next.add(hist, hist->id, *t, static_cast<float>(log(state.dist[t->value()])) + hist->sum);
  }
}

inline void
Tagger::Impl::_unpack_tags(State &state, Raws &seq, bool RECORD) const {
  const Node *path = *min_element(state.current.begin(), state.current.end(), NodeGTCmp());

  const int LEN = state.raws.size();
  seq.resize(LEN);
  for(int i = LEN - 1; i >= 0; --i, path = path->prev){
    seq[i] = klasses[path->id];
    if(RECORD)
      state.last_klass.add(state.raws[i], path->id);
  }
}

inline void
Tagger::Impl::_calc_viterbi_probs(State &state, const ulong DICT_CUTOFF) const {
  Lattice &current = state.current;
  NodeMatrix &next = state.next;

  current.push_back(next.start());
  const ulong len = state.raws.size();
  for(ulong i = 0; i < len; ++i){
    // tag the current word
    _calc_viterbi_word(state, i, DICT_CUTOFF);
    // and apply the beam
    state.reduce(beam_width, beam_ratio); 
  }
}

inline void
Tagger::Impl::_build_lattice(State &state, const ulong DICT_CUTOFF) const {
  Flattice &lattice = state.flattice;

  lattice.reset();

  const ulong LEN = state.raws.size();
  for(ulong i = 0; i < LEN; ++i){
    const Tags *permit = &get_permitted(state, i, DICT_CUTOFF);
    lattice.permitted.push_back(permit->size());

    ulong prev_prev_permit = lattice.permitted[i - 2];  // permitted tags 2 words back
    for(Tags::const_iterator j = permit->begin(); j != permit->end(); ++j){
      for(ulong k = 0; k < lattice[i - 1].size(); k += prev_prev_permit){  // previous column
        FNode *node = lattice.create(lattice[i - 1][k], *j, lattice[i - 1][k]->id, 0.0, 0.0);
        lattice[i].push_back(node); // adding node to current column
      }
    }
  }

  for(ulong i = 0; i < lattice[LEN - 1].size(); ++i)
    lattice[LEN - 1][i]->backward = 1.0;
}

inline void
Tagger::Impl::_calc_weights(State &state) const {
  const ulong LEN = state.raws.size();
  for(ulong i = 0; i < LEN; ++i){
    state.flattice.PDFs.push_back(PDF(klasses.size(), 1.0));
    add_features(state, i, state.flattice.PDFs[i]);
  }
}

inline void
Tagger::Impl::_apply_forward_beam(const FNodes &column, const double BETA) const {
  // beam culls low-score histories
  double beam_cutoff = 0.0;
  for(ulong j = 0; j < column.size(); ++j)
    beam_cutoff = max(beam_cutoff, column[j]->forward);
  beam_cutoff *= forward_beam_ratio*BETA;

  // loop over history nodes and equivalents
  for(ulong j = 0; j < column.size(); ++j)
    if(column[j]->forward <= beam_cutoff)
      column[j]->forward = 0.0;
}

inline void
Tagger::Impl::_forward(Flattice &lattice, const double BETA) const {
  PDF pdf(klasses.size());

  ulong LEN = lattice.PDFs.size();
  for(ulong i = 0; i < LEN; ++i){
    PDF &curr_pdf = lattice.PDFs[i];

    // permitted tags for the 2 history columns
    const ulong prev_prev_permit = lattice.permitted[i - 2];
    const ulong prev_permit = lattice.permitted[i - 1];

    const FNodes &prev_col = lattice[i - 1];
    const FNodes &curr_col = lattice[i];

    _apply_forward_beam(prev_col, BETA);

    // loop over history nodes and equivalents
    for(ulong j = 0; j < prev_col.size(); ++j){
      if(!prev_col[j]->forward)
        continue;

      copy(curr_pdf.begin(), curr_pdf.end(), pdf.begin());
      FNode *hist = prev_col[j];

      _add_history(hist, pdf);
      _normalise(pdf);

      // pass the forward scores to current column
      // note the integer division
      for(ulong l = j / prev_prev_permit; l < curr_col.size(); l += prev_permit){ 
        FNode *current = curr_col[l];
        current->forward += pdf[current->id.value()] * hist->forward;
      }
    }
  }

  // apply the beam to the last column
  _apply_forward_beam(lattice[LEN - 1], BETA);
}

inline void
Tagger::Impl::_backward(Flattice &lattice) const {
  PDF pdf(klasses.size());

  const ulong LEN = lattice.PDFs.size();
  for(ulong i = LEN - 1; i > 0; --i){
    PDF &curr_pdf = lattice.PDFs[i];

    // permitted tags for the 2 history columns
    const ulong prev_prev_permit = lattice.permitted[i - 2];
    const ulong prev_permit = lattice.permitted[i - 1];

    const FNodes &prev_col = lattice[i - 1];
    const FNodes &curr_col = lattice[i];

    // loop over history nodes and equivalents
    for(ulong j = 0; j < prev_col.size(); ++j){
      if(!prev_col[j]->forward){
        prev_col[j]->backward = 0.0;
        continue;
      }

      copy(curr_pdf.begin(), curr_pdf.end(), pdf.begin());
      FNode *hist = prev_col[j];
      _add_history(hist, pdf);
      _normalise(pdf);
      
      // get the backward scores from current column
      for(ulong l = j / prev_prev_permit; l < curr_col.size(); l += prev_permit){  // integer division
        FNode *current = curr_col[l];
        hist->backward += pdf[current->id.value()] * current->backward;
      }
    }
  }

  // put the full forward-backward probability distribution back into the PDFs
  for(ulong i = 0; i < lattice.PDFs.size(); ++i){
    const FNodes &curr_col = lattice[i];
    PDF &dist = lattice.PDFs[i];

    zero(dist);

    for(ulong j = 0; j < curr_col.size(); ++j){
      FNode *current = curr_col[j];
      dist[current->id.value()] += current->forward * current->backward;
    }
  }
}

inline void
Tagger::Impl::_calc_fb_probs(State &state, const ulong DICT_CUTOFF, const double BETA) const {
  _build_lattice(state, DICT_CUTOFF);
  _calc_weights(state);
  _forward(state.flattice, BETA);
  _backward(state.flattice);
}

inline void
Tagger::Impl::_calc_noseq_probs(State &state, const ulong DICT_CUTOFF, const double) const {
  _build_lattice(state, DICT_CUTOFF);

  for(ulong i = 0; i < state.raws.size(); ++i){
    state.flattice.PDFs.push_back(PDF(klasses.size(), 1.0));
    add_features(state, i, state.flattice.PDFs[i]);
    _normalise(state.flattice.PDFs[i]);
  }
}

inline void
Tagger::Impl::_calc_greedy_probs(State &state, const ulong DICT_CUTOFF, const double) const {
  Node hist(0, NLP::SENTINEL, NLP::SENTINEL, 1.0);

  _build_lattice(state, DICT_CUTOFF);

  for(ulong i = 0; i < state.raws.size(); ++i){
    state.flattice.PDFs.push_back(PDF(klasses.size(), 1.0));
    PDF &dist = state.flattice.PDFs[i];

    add_features(state, i, dist);
    _add_history(&hist, dist);
    _normalise(state.flattice.PDFs[i]);

    ushort max_tag = 0;
    double max_tag_prob = 0;
    for(ushort j = 2; j < dist.size(); ++j)
      if(max_tag_prob < dist[j]){
        max_tag = j;
        max_tag_prob = dist[j];
      }

    hist.pid = hist.id;
    hist.id = max_tag;
    hist.sum = max_tag_prob;
  }
}

inline void
Tagger::Impl::_unpack_mtags(State &state, MultiRaws &mraws, const double BETA,
                            bool RECORD) const {
  mraws.resize(state.raws.size());

  for(ulong i = 0; i < state.raws.size(); ++i){
    PDF &dist = state.flattice.PDFs[i];

    double total_mass = 0.0;
    double prob_cutoff = 0.0;
    ushort max_klass = 2;
    for(ulong j = 2; j < dist.size(); ++j){
      total_mass += dist[j];
      if(prob_cutoff < dist[j]){
        prob_cutoff = dist[j];
        max_klass = j;
      }
    }
    prob_cutoff *= BETA;
    if(RECORD)
      state.last_klass.add(state.raws[i], max_klass);

    MultiRaw &mraw = mraws[i];
    mraw.resize(0);

    for(ushort j = 2; j < dist.size(); ++j)
      if(dist[j] >= prob_cutoff)
        mraw.push_back(ScoredRaw(klasses[j], dist[j]/total_mass));

    std::sort(mraw.begin(), mraw.end());
  }
}

inline void
Tagger::Impl::_tag(Sentence &sent, State &state, Algorithm alg, ulong DICT_CUTOFF) const {
  begin_sentence(state);
  can_sentence(sent, state);
  switch(alg){
  case VITERBI:
    _calc_viterbi_probs(state, DICT_CUTOFF);
    break;
  default:
    throw NLP::Exception("unrecognised algorithm identifier passed to Tagger::tag method");
  }
  unpack_tags(state, sent);
}

inline void
Tagger::Impl::_mtag(Sentence &sent, State &state, Algorithm alg, ulong DICT_CUTOFF, double BETA) const {
  begin_sentence(state);
  can_sentence(sent, state);
  switch(alg){
  case NOSEQ:
    _calc_noseq_probs(state, DICT_CUTOFF, BETA);
    break;
  case GREEDY:
    _calc_greedy_probs(state, DICT_CUTOFF, BETA);
    break;
  case FWDBWD:
    _calc_fb_probs(state, DICT_CUTOFF, BETA);
    break;
  default:
    throw NLP::Exception("unrecognised algorithm identifier passed to Tagger::tag method");
  }
  unpack_mtags(state, sent, BETA);
}

inline void
Tagger::Impl::tag(NLP::Sentence &sent, const Algorithm alg,
                  const ulong DICT_CUTOFF, State *state) const {
  if(state)
    _tag(sent, *state, alg, DICT_CUTOFF);
  else{
    State tmp(klasses.size(), maxwords);
    _tag(sent, tmp, alg, DICT_CUTOFF);
  }
}

inline void
Tagger::Impl::tag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
                  const Algorithm alg, const ulong DICT_CUTOFF) const {
  NLP::Sentence sent;
  State state(klasses.size(), maxwords);

  while(reader.next(sent)){
    if(sent.words.size() == 0)
      state.begin_document();
    _tag(sent, state, alg, DICT_CUTOFF);
    writer.next(sent);
  }
}

inline void
Tagger::Impl::mtag(Sentence &sent, const Algorithm alg,
                   const ulong DICT_CUTOFF, const double BETA,
                   State *state) const {
  if(state)
    _mtag(sent, *state, alg, DICT_CUTOFF, BETA);
  else{
    State tmp(klasses.size(), maxwords);
    _mtag(sent, tmp, alg, DICT_CUTOFF, BETA);
  }
}

inline void
Tagger::Impl::mtag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
                   const Algorithm alg, const ulong DICT_CUTOFF,
                   const double BETA) const {
  NLP::Sentence sent;
  State state(klasses.size(), maxwords);

  while(reader.next(sent)){
    if(sent.words.size() == 0)
      state.begin_document();
    _mtag(sent, state, alg, DICT_CUTOFF, BETA);
    writer.next(sent);
  }
}

inline void
Tagger::Impl::begin_sentence(State &state) const {
  state.begin_sentence();
}

inline void
Tagger::Impl::begin_document(State *state) const {
  if(state)
    state->begin_document();
}

} }
