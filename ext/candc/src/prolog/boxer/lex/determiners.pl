
:- module(determiners,[semlex_det/4]).
:- use_module(boxer(resolveDRT),[goldAntecedent/2]).
:- use_module(boxer(categories),[rel/3]).
:- use_module(semlib(options),[option/2]).

/* =========================================================================
   Determiners: NP/N

   This file contains the lexical semantic specifications for determiners,
   i.e. tokens with CCG category NP/N.
========================================================================= */


/* -------------------------------------------------------------------------
   Generalized Quantifiers
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Att-[sem:'UNK'|Att],Sem):-
   member(Lemma,[few,most,two,three]), !,
   Sem = lam(N,lam(P,B1:drs([],[B1:[]:duplex(Lemma,merge(B2:drs([B2:Index:X],[]),
                                                         app(N,X)),X,
                                             app(P,X))]))).


/* -------------------------------------------------------------------------
   Universally quantifying
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Att-[sem:'AND'|Att],Sem):-
   member(Lemma,[all,each,either,any,every,most,whichever,whatever]),
   option('--semantics',amr), !,
   Sem = lam(N,lam(P,merge(merge(B:drs([B:[]:X,B:[]:A],[B:Index:pred(A,Lemma,a,1),
                                                        B:[]:rel(X,A,mod,2)]),
                           app(N,X)),app(P,X)))).

semlex_det(Lemma,Index,Att-[sem:'AND'|Att],Sem):-
   member(Lemma,[all,each,either,any,every,most,whichever,whatever]), !,
   Sem = lam(N,lam(P,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:X],[]),
                                          app(N,X)),
                                    app(P,X))]))).


/* -------------------------------------------------------------------------
   Negation
------------------------------------------------------------------------- */

semlex_det(no,Index,Att-[sem:'NOT'|Att],Sem):- !,
   Sem = lam(N,lam(P,B1:drs([],[B1:Index:not(merge(B2:drs([B2:Index:X],[]),
                                             merge(app(N,X),
                                                   app(P,X))))]))).


/* -------------------------------------------------------------------------
   Definites/Demonstratives
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Att-[sem:'PRX'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,[this,these]), !,
   Sem = lam(N,lam(P,alfa(def,merge(B:drs([B:[]:X,B:[]:E],[B:Index:pred(E,this,a,1),B:[]:rel(X,E,mod,1)]),
                                    app(N,X)),
                              app(P,X)))).

semlex_det(Lemma,Index,Att-[sem:'DST'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,[that,those]), !,
   Sem = lam(N,lam(P,alfa(def,merge(B:drs([B:[]:X,B:[]:E],[B:Index:pred(E,that,a,1),B:[]:rel(X,E,mod,1)]),
                                    app(N,X)),
                              app(P,X)))).

semlex_det(Lemma,Index,Att-[sem:'DEF'|Att],Sem):-
   member(Lemma,[the,both]), !,
%  goldAntecedent(Index,Att),
   Sem = lam(N,lam(P,alfa(def,merge(B:drs([B:Index:X],[]),
                                    app(N,X)),
                              app(P,X)))).

semlex_det(Lemma,Index,Att-[sem:'PRX'|Att],Sem):-
   member(Lemma,[this,these]), !,
%  goldAntecedent(Index,Att),
   Sem = lam(N,lam(P,alfa(def,merge(B:drs([B:Index:X],[]),
                                    app(N,X)),
                              app(P,X)))).

semlex_det(Lemma,Index,Att-[sem:'DST'|Att],Sem):-
   member(Lemma,[that,those]), !,
%  goldAntecedent(Index,Att),
   Sem = lam(N,lam(P,alfa(def,merge(B:drs([B:Index:X],[]),
                                    app(N,X)),
                              app(P,X)))).


/* -------------------------------------------------------------------------
   WH
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Lemma,[which,what]), !,
   Sem = lam(N,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                       merge(B2:drs([B2:Index:X],[]),app(N,X)),
                                       X,
                                       app(P,X))]))).


/* -------------------------------------------------------------------------
   "another"
------------------------------------------------------------------------- */

semlex_det(another,Index,Att-[sem:'ALT'|Att],Sem):- 
   \+ option('--semantics',amr), !,
%  goldAntecedent(Index,Att),
   Sem = lam(N,lam(P,alfa(def,merge(B1:drs([B1:[]:Y],[]),
                                    app(N,Y)),
                              merge(merge(B2:drs([B2:Index:X],
                                                 [B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),
                                          app(N,X)),
                                    app(P,X))))).


/* -------------------------------------------------------------------------
   "neither" (see Heim & Kratzer 1998 p. 154)
------------------------------------------------------------------------- */

semlex_det(neither,Index,Att-[sem:'NOT'|Att],Sem):- !,
   Sem = lam(N,lam(P,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:X],[]),app(N,X)),
                                                B3:drs([],[B3:Index:not(app(P,X))]))]))).


/* -------------------------------------------------------------------------
   Possessives
------------------------------------------------------------------------- */

semlex_det(my,Index,Att1-[sem:'HAS'|Att2],Sem):-
   option('--semantics',amr), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,i,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(my,Index,Att1-[sem:'HAS'|Att2],Sem):- !,
%  goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,person,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(Lemma,Index,Att1-[sem:'HAS'|Att2],Sem):- 
   option('--semantics',amr),
   member(Lemma,[your,thy]), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,you,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(Lemma,Index,Att1-[sem:'HAS'|Att2],Sem):- 
   member(Lemma,[your,thy]), !,
%  goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,person,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(his,Index,Att1-[sem:'HAS'|Att2],Sem):-
   option('--semantics',amr), !,
   goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,he,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(his,Index,Att1-[sem:'HAS'|Att2],Sem):- !,
   goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,male,n,2)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(her,Index,Att1-[sem:'HAS'|Att2],Sem):-
   option('--semantics',amr), !,
   goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,she,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(her,Index,Att1-[sem:'HAS'|Att2],Sem):- !,
   goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,female,n,2)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(its,Index,Att1-[sem:'HAS'|Att2],Sem):-
   option('--semantics',amr), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,it,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(its,Index,Att1-[sem:'HAS'|Att2],Sem):- !,
%  goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,thing,n,12)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(our,Index,Att1-[sem:'HAS'|Att2],Sem):- 
   option('--semantics',amr), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,we,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(our,Index,Att1-[sem:'HAS'|Att2],Sem):- !,
%  goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,person,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(their,Index,Att1-[sem:'HAS'|Att2],Sem):-
   option('--semantics',amr), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,they,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(their,Index,Att1-[sem:'HAS'|Att2],Sem):- !,
%  goldAntecedent(Index,Att1),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:Index:pred(Y,thing,n,12)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:[]:rel(X,Y,Relation,1)]),
                                             app(N,X)),
                                       app(P,X))))).


/* -------------------------------------------------------------------------
   Many/Much [as determiner]
------------------------------------------------------------------------- */

semlex_det(many,Index,Att-[sem:'QUA'|Att],Sem):- !,
   Sem = lam(P,lam(Q,merge(B:drs([B:[]:X],[B:Index:pred(X,quantity,n,1)]),
                           merge(app(P,X),app(Q,X))))).

semlex_det(much,Index,Att-[sem:'QUA'|Att],Sem):- !,
   Sem = lam(P,lam(Q,merge(B:drs([B:[]:X],[B:Index:pred(X,amount,n,3)]),
                           merge(app(P,X),app(Q,X))))).


/* -------------------------------------------------------------------------
   Indefinites (and all other cases)
------------------------------------------------------------------------- */

semlex_det(_Lemma,Index,Att-[sem:'DIS'|Att],Sem):-
   Sem = lam(N,lam(P,merge(merge(B:drs([B:Index:X],[]),app(N,X)),app(P,X)))).
