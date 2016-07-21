
:- module(morpha,[morpha/2]).
:- use_module(library(lists),[append/3,select/3]).

/* -------------------------------------------------------------------------
   Pre-Processing of CCG derivation to ensure correct format
------------------------------------------------------------------------- */

morpha(Tags1,[pos:Pos,lemma:New|Tags3]):-
   select(pos:Pos,Tags1,Tags2),
   select(lemma:Lem,Tags2,Tags3), !,
   morpha(Pos,Lem,New).

morpha(Tags,Tags).

/* -------------------------------------------------------------------------
   Morphological Rules (+POS,+Old,-New)
------------------------------------------------------------------------- */

morpha(_,'n\'t',not):- !.
morpha(_, '\'t',not):- !.

morpha('NN','%',percent):- !.

morpha('$','$',dollar):- !.
morpha('$','£',pound):- !.
morpha('$','#',pound):- !.
morpha('$','€',euro):- !.

morpha('VBP','\'s', be):- !.
morpha('VBZ','\'s', be):- !.
morpha('VBP','\'m', be):- !.
morpha('VBZ','\'m', be):- !.
morpha('VBP','\'re',be):- !.
morpha('VBZ','\'re',be):- !.

morpha('JJR',better,    good):- !.
morpha('JJS',best,      good):- !.
morpha('JJR',worse,      bad):- !.
morpha('JJS',worst,      bad):- !.
morpha('JJR',farther,    far):- !.
morpha('JJR',further,    far):- !.
morpha('JJS',farthest,   far):- !.
morpha('JJS',furthest,   far):- !.
morpha('JJR',larger,   large):- !.
morpha('JJS',largest,  large):- !.
morpha('JJR',simpler, simple):- !.
morpha('JJS',simplest,simple):- !.
morpha('JJR',closer,   close):- !.
morpha('JJS',closest,  close):- !.
morpha('JJR',freer,     free):- !.
morpha('JJS',freest,    free):- !.
morpha('JJR',safer,     safe):- !.
morpha('JJS',safest,    safe):- !.
morpha('JJR',looser,   loose):- !.
morpha('JJS',loosest,  loose):- !.
morpha('JJR',wider,     wide):- !.
morpha('JJS',widest,    wide):- !.
morpha('JJR',wiser,     wise):- !.
morpha('JJS',wisest,    wise):- !.

morpha('JJR',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"ier", Codes1),  !, append(Codes3,"y",Codes2), name(Lemma2,Codes2).
morpha('JJS',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"iest", Codes1), !, append(Codes3,"y",Codes2), name(Lemma2,Codes2).
morpha('JJR',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"tter",Codes1),  !, append(Codes3,"t",Codes2), name(Lemma2,Codes2).
morpha('JJS',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"ttest",Codes1), !, append(Codes3,"t",Codes2), name(Lemma2,Codes2).
morpha('JJR',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"gger",Codes1),  !, append(Codes3,"g",Codes2), name(Lemma2,Codes2).
morpha('JJS',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"ggest",Codes1), !, append(Codes3,"g",Codes2), name(Lemma2,Codes2).
morpha('JJR',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"dder",Codes1),  !, append(Codes3,"d",Codes2), name(Lemma2,Codes2).
morpha('JJS',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"ddest",Codes1), !, append(Codes3,"d",Codes2), name(Lemma2,Codes2).
morpha('JJR',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"nner",Codes1),  !, append(Codes3,"n",Codes2), name(Lemma2,Codes2).
morpha('JJS',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"nnest",Codes1), !, append(Codes3,"n",Codes2), name(Lemma2,Codes2).
morpha('JJR',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes2,"er",  Codes1),  !,                            name(Lemma2,Codes2).
morpha('JJS',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes2,"est", Codes1),  !,                            name(Lemma2,Codes2).

morpha('RB',only,only):- !.
morpha('RB',really,really):- !.
morpha('RB',hardly,hardly):- !.
morpha('RB',especially,especially):- !.
morpha('RB',accordingly,accordingly):- !.
morpha('RB',lovely,lovely):- !.
morpha('RB',Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes3,"ily",Codes1), !, append(Codes3,"y",Codes2), name(Lemma2,Codes2).
morpha('RB', Lemma1,Lemma2):- name(Lemma1,Codes1), append(Codes2,"ly",Codes1), !, name(Lemma2,Codes2).

morpha(_,X,X).


