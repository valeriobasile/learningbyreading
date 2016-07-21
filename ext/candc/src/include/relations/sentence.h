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

    class Sentence {
    public:
      vector<Word> words;
      vector<Phrase> phrases;

      Sentence(void): words(), phrases() {
        words.reserve(128);
        phrases.reserve(128);
      };

      void add(NLP::Word w, const char *t, const char *c){
        words.push_back(Word(w,t,c));
      }

      void parse(void){
        Phrase *current = 0;
        bool extend = false;

        for(vector<Word>::iterator w = words.begin(); w != words.end(); w++){
          if((w->is_pp() || w->is_cc() || w->is_pos()) && current && current->is_np())
            extend = true;
          if(w->is_advp() && current && current->is_vp())
            extend = true;

          if(w->is_begin() && !extend){
            Phrase phrase(&*w);
            phrases.push_back(phrase);
            current = &*phrases.end() - 1;
          }else
            current->end++;
          if(w->is_np() || w->is_vp())
            extend = 0;
        }
      }

      void pass1(void){
        for(vector<Phrase>::iterator i = phrases.begin(); i != phrases.end(); i++)
          i->pass1();
      }

      void pass2(void){
        for(vector<Phrase>::iterator i = phrases.begin(); i != phrases.end(); i++)
          i->pass2();
      }

      void pass12(void){
        for(vector<Phrase>::iterator i = phrases.begin(); i != phrases.end(); i++){
          i->pass1();
          i->pass2();
        }
      }

      void pass3(void){
        for(vector<Phrase>::iterator i = phrases.begin(); i != phrases.end(); i++)
          i->findhead();
        Phrase *np = NULL;
        Phrase *vp = NULL;
        for(vector<Phrase>::reverse_iterator i = phrases.rbegin(); i != phrases.rend(); i++){
          if(i->is_vp() && i->head)
            vp = &*i;
          else if(i->is_np() && i->head){
            np = &*i;
            if(vp){
              switch(vp->voice){
                case Phrase::ACTIVE:
                  print_relation(stdout, np->head, vp->head, "S");
                  np->head->attached = vp->head;
                  break;
                case Phrase::PASSIVE:
                  print_relation(stdout, np->head, vp->head, "D");
                  np->head->attached = vp->head;
                  break;
                default: ; /* ignore */
              }
            }
          }else{
            np = NULL;
            vp = NULL;
          }
        }
      }

      void pass4(void){
        Phrase *np = NULL;
        Phrase *vp = NULL;
        bool seen_pp = false;
        for(vector<Phrase>::iterator i = phrases.begin(); i != phrases.end(); i++){
          if(i->is_vp() && i->head){
            vp = &*i;
            seen_pp = false;
          }else if(i->is_np() && i->head){
            np = &*i;
            if(vp){
              if(seen_pp){
                print_relation(stdout, np->head, vp->head, "I");
                np->head->attached = vp->head;
              }else switch(vp->voice){
                case Phrase::ACTIVE:
                  print_relation(stdout, np->head, vp->head, "D");
                  np->head->attached = vp->head;
                  break;
                case Phrase::PASSIVE:
                  print_relation(stdout, np->head, vp->head, "S");
                  np->head->attached = vp->head;
                  break;
                default: ; /* ignore */
              }
            }
          }else if(i->is_pp())
            seen_pp = true;
          else{
            vp = NULL;
            seen_pp = false;
          }
        }
      }

      void clear(void){
        words.resize(0);
        phrases.resize(0);
      };

      void print(FILE *file){
        for(vector<Phrase>::iterator p = phrases.begin(); p != phrases.end(); p++)
          p->println(file);
      }
    };

  }
}
