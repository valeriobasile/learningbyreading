
:- module(tense,
          [ tense/4,   % +Mood, +Index, +Att--Att, -Sem
            aspect/5   % +ArgMood, +Mood, +Index, +Att--Att, -Sem
          ]).

:- use_module(semlib(options),[option/2]).
:- use_module(library(lists),[member/2]).
:- use_module(boxer(categories),[att/3]).


/* =========================================================================
   Tense
========================================================================= */

tense(Mood,Index,Att-[sem:Tag|Att],Sem):-
   option('--tense',true), 
   member(Mood,[dcl,inv,wq,q]),
   att(Att,pos,PoS), 
   pos2tense(PoS,Index,Sem,Tag), !.

tense(com,_,Att-[sem:'COM'|Att],Sem):- !,
   Sem = lam(S,lam(M,app(S,M))).

tense(adj,_,Att-[sem:'IST'|Att],Sem):- !,
   Sem = lam(S,lam(M,app(S,M))).

tense(_,_,Att-[sem:'EVE'|Att],Sem):-
   Sem = lam(S,lam(M,app(S,M))).


/* -------------------------------------------------------------------------
   Past Tense
------------------------------------------------------------------------- */

pos2tense('VBD',Index,Sem,'PAS'):- 
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B1:[]:N,B2:Index:T],
                                              [B1:[]:pred(N,now,a,1),
                                               B2:[]:rel(E,T,temp_included,1),
                                               B2:[]:rel(T,N,temp_before,1)]),
                                       app(F,E)))))).


/* -------------------------------------------------------------------------
   Present Tense
------------------------------------------------------------------------- */

pos2tense(Cat,Index,Sem,'PRE'):- 
   member(Cat,['VBP','VBZ']), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B1:[]:N,B2:Index:T],
                                              [B1:[]:pred(N,now,a,1),
                                               B2:[]:rel(E,T,temp_included,1),
                                               B2:[]:eq(T,N)]),
                                       app(F,E)))))).


/* -------------------------------------------------------------------------
   Future Tense
------------------------------------------------------------------------- */

pos2tense('MD',Index,Sem,'FUT'):- 
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B1:[]:N,B2:Index:T],
                                              [B1:[]:pred(N,now,a,1),
                                               B2:[]:rel(E,T,temp_included,1),
                                               B2:[]:rel(N,T,temp_before,1)]),
                                       app(F,E)))))).


/* =========================================================================
   Aspect
========================================================================= */

/* -------------------------------------------------------------------------
   Present Perfect
------------------------------------------------------------------------- */
 
aspect(pt,_,Index,Att-[semtag:'PRE'|Att],Sem):-
   option('--tense',true),
   att(Att,pos,PoS),
   member(PoS,['VBZ','VBP']), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B1:[]:N,B2:Index:T,B2:[]:St],
                                              [B1:[]:pred(N,now,a,1),
                                               B2:[]:eq(T,N),
                                               B2:[]:rel(St,T,temp_includes,1),
                                               B2:[]:rel(E,St,temp_abut,1)]),
%                                      app(F,St)))))).
                                       app(F,E)))))).


/* -------------------------------------------------------------------------
   Past Perfect
------------------------------------------------------------------------- */

aspect(pt,_,Index,Att-[semtag:'PAS'|Att],Sem):-
   option('--tense',true),
   att(Att,pos,'VBD'),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B1:[]:N,B2:Index:T,B2:[]:St],
                                              [B1:[]:pred(N,now,a,1),
                                               B2:[]:rel(T,N,temp_before,1),
                                               B2:[]:rel(St,T,temp_includes,1),
                                               B2:[]:rel(E,St,temp_abut,1)]),
%                                      app(F,St)))))).
                                       app(F,E)))))).


/* -------------------------------------------------------------------------
   Perfect Passive
------------------------------------------------------------------------- */

aspect(pss,pt,Index,Att-[semtag:'UNK'|Att],Sem):-
   option('--tense',true), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B2:Index:T,B2:[]:St],
                                              [B2:[]:rel(St,T,temp_includes,1),
                                               B2:[]:rel(E,St,temp_overlap,1)]),
%                                      app(F,St)))))).
                                       app(F,E)))))).


/* -------------------------------------------------------------------------
   Perfect Progressive 
------------------------------------------------------------------------- */

aspect(ng,pt,Index,Att-[semtag:'UNK'|Att],Sem):-
   option('--tense',true), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B2:Index:T,B2:[]:St],
                                              [B2:[]:rel(St,T,temp_includes,1),
                                               B2:[]:rel(E,St,temp_overlap,1)]),
%                                      app(F,St)))))).
                                       app(F,E)))))).


/* -------------------------------------------------------------------------
   Present Progressive
------------------------------------------------------------------------- */

aspect(ng,_,Index,Att-[semtag:'PRE'|Att],Sem):-
   option('--tense',true), 
   att(Att,pos,PoS),
   member(PoS,['VBZ','VBP']), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B1:[]:N,B2:Index:T,B2:[]:St],
                                              [B1:[]:pred(N,now,a,1),
                                               B2:[]:eq(T,N),
                                               B2:[]:rel(St,T,temp_includes,1),
                                               B2:[]:rel(E,St,temp_overlap,1)]),
%                                      app(F,St)))))).
                                       app(F,E)))))).


/* -------------------------------------------------------------------------
   Past Progressive
------------------------------------------------------------------------- */

aspect(ng,_,Index,Att-[semtag:'PAS'|Att],Sem):-
   att(Att,pos,'VBD'),
   option('--tense',true), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(B2:drs([B1:[]:N,B2:Index:T,B2:[]:St],
                                              [B1:[]:pred(N,now,a,1),
                                               B2:[]:rel(T,N,temp_before,1),
                                               B2:[]:rel(St,T,temp_included,1),
                                               B2:[]:rel(E,St,temp_overlap,1)]),
%                                      app(F,St)))))).
                                       app(F,E)))))).


/* -------------------------------------------------------------------------
   Other cases
------------------------------------------------------------------------- */

aspect(_,Mood,Index,Att,Sem):-
   tense(Mood,Index,Att,Sem).
