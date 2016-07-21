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
  namespace Relations {

    void
    print_relation(std::ostream &out, Word *w1, Word *w2, const char *rel){
      addrelation_grefenstette(w1, w2, rel);
    }

    class Phrase {
    public:
      const static ulong ACTIVE = 1;
      const static ulong PASSIVE = 2;
      const static ulong ATTRIBUTIVE = 4;

      Word *begin;
      Word *end;
      const char *tag;
      Words *head;
      ulong voice;

      Phrase(Words::iterator s): begin(s), end(s + 1), tag(s->chunk), head(0), voice(0) {};
      ~Phrase(void) {};

      bool in_modifier_set(Word *w){
        return w->is_noun() || w->is_det() || w->is_adj() || w->is_pverb() || w->is_prep() || w->is_pp();
      }
      bool in_begin_set(Word *w){ return w->is_cnoun() || w->is_adj(); }
      bool in_receive_set(Word *w){ return w->is_noun(); }

      bool is_np(void){ return tag[2] == 'N'; }
      bool is_vp(void){ return tag[2] == 'V'; }
      bool is_pp(void){ return tag[2] == 'P' && tag[3] == 'P'; }

      void pass1(void){
        if(!is_np())
          return;
        for(Word *i = begin; i != end; i++){
          if(in_modifier_set(i))
            for(Word *j = i + 1; j != end; j++){
              if(j->is_prep() || j->is_pp() || j->is_cc())
                break;
              if(in_receive_set(j)){
                i->attached = j;
                if(i->is_pp())
                  j->prep = i;
                else switch(i->tag[0]){
                  case 'I': 
                  case 'T':
                    j->prep = i;
                    break;
                  case 'J':
                    print_relation(stdout, j, i, "J");
                    break;
                  case 'V':
                    print_relation(stdout, j, i, "G");
                    break;
                  case 'N':
                    print_relation(stdout, j, i, "N");
                    break;
                }
              }
            }
        }
      }

      void pass2(void){
        if(!is_np())
          return;
        for(Word *i = end - 1; i > begin; i--){
          if(i->attached == NULL){
            Word *current = i->prep != NULL ? i->prep : i;
            for(Word *j = current - 1; j >= begin; j--){
              if(j->is_cnoun() && j->attached != i){
                if(current->is_prep() || current->is_pp())
                  print_relation(stdout, j, i, "P");
                else if(current->is_noun())
                  print_relation(stdout, j, i, "N");
                else
                  continue;
                i->attached = j;
                break;
              }
            }
          }
        }
      }

      void findhead_np(void){
        for(head = end - 1; head >= begin; head--)
          if(head->is_noun() && !head->attached)
            return;
        head = NULL;
      }

      void findhead_vp(void){
        for(head = end - 1; head >= begin; head--)
          if(head->is_verb() && !head->attached)
            return;
        head = NULL;
      }

      void voice_vp(void){
        if(head->is_to_be())
          voice = ATTRIBUTIVE;
        else{
          voice = ACTIVE;
          for(Word *i = begin; i != end; i++){
            if(!i->is_verb())
              continue;
            if(i->is_to_be())
              voice = PASSIVE;
            else if(i->is_pverb() && !i->is_being())
              voice = ACTIVE;
          }
        }
      }

      void findhead(void){
        if(is_np())
          findhead_np();
        else if(is_vp()){
          findhead_vp();
          if(head)
            voice_vp();
        }
      }

      void println(FILE *file){
        fprintf(file, "%-6s [", tag);
        if(head)
          head->print(file);
        fputs("]\n", file);

        for(Word *i = begin; i != end; i++){
          fputs("         ", file);
          i->println(file);
        }
      }
    };

  }
}
