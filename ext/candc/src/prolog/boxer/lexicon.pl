:- module(lexicon,[semlex/5]).
:- multifile semlex/5.

:- use_module(boxer(slashes)).
:- use_module(boxer(string2digit),[string2digit/2,string2score/2]).
:- use_module(boxer(categories),[category/3,att/3,sense/4,roles/4,rel/3,role/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(knowledge(ne),[neClass/2,neClassType/3,neClassType/4]).
:- use_module(knowledge(dates),[month/2,dofm/2,decade/2,year/2]).
:- use_module(knowledge(punctuation),[punctuation/2]).
%:-use_module(knowledge(title),[title/2]).
%:- use_module(knowledge(negprefix),[negprefix/4]).
%:- use_module(knowledge(negsuffix),[negsuffix/4]).
%:-use_module(knowledge(nationality),[nationality/2]).
:- use_module(lex(determiners),[semlex_det/4]).
:- use_module(lex(verbs),[semlex_verb/5]).
:- use_module(boxer(closure),[closing/1,plosing/1]).
:- use_module(library(lists),[member/2]).
:- use_module(boxer(coordination),[coordMacro/2,argCard/2]).
:- use_module(boxer(resolveDRT),[goldAntecedent/2]).

:- [lex/pp].
:- [lex/n].
% :- [lex/s].
% :- [lex/np].


/* =========================================================================
   Category Abstraction
========================================================================= */

cat(X1/Y1,X2|Y2):- !, cat(X1,X2), cat(Y1,Y2).
cat(X1\Y1,X2|Y2):- !, cat(X1,X2), cat(Y1,Y2).
cat(X,X).


/* =========================================================================
   Punctuation
========================================================================= */

semlex(t:_\s:_,_,_,Att-[sem:'NIL'|Att],Sem):- !,
   closing(CC),
   Sem = lam(S,app(S,CC)).

semlex(s:X\s:X,'?',Index,Att-[sem:'QUE'|Att],Sem):- 
   option('--semantics',amr), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([],[B:Index:pred(E,interrogative,r,1)]),app(F,E)))))).

semlex(Cat,_,_,Att-[sem:'NIL'|Att],Sem):- 
   att(Att,pos,POS),
   punctuation(POS,_),
   member(Cat,[C\C, C/C,
               (s:X/s:X)/(s:Y/s:Y), (s:X/s:X)/(s:Y\s:Y),
               (s:X/s:X)\(s:Y/s:Y), (s:X/s:X)\(s:Y\s:Y),
               (s:X\s:X)/(s:Y\s:Y), (s:X\s:X)/(s:Y/s:Y),
               (s:X\s:X)\(s:Y/s:Y), (s:X\s:X)\(s:Y\s:Y)]), !,
   Sem = lam(P,P).

semlex(Cat,_,_,Att-[sem:'NIL'|Att],Sem):- 
   att(Att,pos,POS),
   punctuation(POS,_),
   member(Cat,[(((s:X\np)/(s:X\np))\(s:Y/s:Y)),
               (((s:X\np)\(s:X\np))\(s:Y/s:Y))]), !,
   Sem = lam(SMOD,lam(VP,lam(NP,app(SMOD,app(VP,NP))))).

semlex(Cat,_,Index,Att-[sem:'SUB'|Att],Sem):-
   att(Att,pos,POS),
   punctuation(POS,left),
   member(Cat,[(np\np)/np,(np\np)\np,(np/np)/np,(np/np)\np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(B:drs([],[B:Index:rel(X,Y,rel,0)]),app(P,X))))))))).

semlex(Cat,_,Index,Att-[sem:'SUB'|Att],Sem):-
   att(Att,pos,POS),
   punctuation(POS,_),
   member(Cat,[(s:X\s:X)/np,(s:X\s:X)\np,(s:X/s:X)/np,(s:X/s:X)\np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(Z,app(Q1,lam(Y,merge(B:drs([],[B:Index:rel(Z,Y,rel,0)]),app(P,Z))))))))).

semlex(Cat,_,Index,Att-[sem:'SUB'|Att],Sem):-
   att(Att,pos,POS),
   punctuation(POS,left),
   member(Cat,[(n\n)/n,(n/n)/n]), !,
   Sem = lam(N,lam(P,lam(X,merge(merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,rel,0)]),app(N,Y)),app(P,X))))).

semlex(Cat,_,Index,Att-[sem:'SUB'|Att],Sem):-
   att(Att,pos,POS),
   punctuation(POS,_),
   member(Cat,[((s:X\np)\(s:X\np))/np, 
               ((s:X\np)/(s:X\np))/np, 
               ((s:X\np)\(s:X\np))\np, 
               ((s:X\np)/(s:X\np))\np]), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(B:drs([],[B:Index:rel(E,Y,rel,0)]),app(F,E)))))))))).


/* =========================================================================
   Coordination (disjunction and negation)
========================================================================= */

semlex(conj:n,Sym,Index,Att-[sem:'DIS'|Att],Sem):-
   Sym = or,
   option('--semantics',drg), !,
   Sem = lam(P2,lam(P1,lam(X,B:drs([],[B:Index:pred(X,Sym,s,1),
                                       B:[]:or(app(P1,X),app(P2,X))])))).

semlex(conj:Cat,Lemma,Index,Att-[sem:'DIS'|Att],Sem):-
   member(Lemma,[either,or]), !,
   argCard(Cat,N),
   coordMacro(N,Coord),
   Sem = app(Coord,lam(K2,lam(K1,B:drs([],[B:Index:or(K1,K2)])))).

semlex(conj:Cat,Lemma,Index,Att-[sem:'NOT'|Att],Sem):- 
   member(Lemma,[neither,nor,not,of,than]), !,
   argCard(Cat,N),
   coordMacro(N,Coord),
   Sem = app(Coord,lam(K2,lam(K1,merge(K1,B:drs([],[B:Index:not(K2)]))))).


/* =========================================================================
   Contrast
========================================================================= */

semlex(conj:(s:_\np),but,Index,Att-[sem:'BUT'|Att],Sem):-    
   option('--theory',sdrt),
   option('--semantics',amr), !,
   Sem = lam(V1,lam(V2,lam(X,lam(E,sdrs([lab(K1,app(app(V2,X),E)),
                                         lab(K2,app(app(V1,X),E))],
                                        [Index:rel(K1,K2,'contrast-01')]))))).

semlex(conj:(s:_\np),but,Index,Att-[sem:'BUT'|Att],Sem):-    
   option('--theory',sdrt), !,
   Sem = lam(V1,lam(V2,lam(X,lam(E,sdrs([lab(K1,app(app(V2,X),E)),
                                         lab(K2,app(app(V1,X),E))],
                                        [Index:rel(K1,K2,continuation),
                                         []:rel(K1,K2,contrast)]))))).

semlex(conj:(s:_),but,Index,Att-[sem:'BUT'|Att],Sem):- 
   option('--theory',sdrt),
   option('--semantics',amr), !,
   Sem = lam(S1,lam(S2,lam(E,sdrs([lab(K1,app(S2,E)),
                                   lab(K2,app(S1,E))],
                                  [Index:rel(K1,K2,'contrast-01')])))).

semlex(conj:(s:_),but,Index,Att-[sem:'BUT'|Att],Sem):- 
   option('--theory',sdrt), !,
   Sem = lam(S1,lam(S2,lam(E,sdrs([lab(K1,app(S2,E)),
                                   lab(K2,app(S1,E))],
                                  [Index:rel(K1,K2,continuation),
                                   []:rel(K1,K2,contrast)])))).



/* =========================================================================
   Coordination: nouns
========================================================================= */

semlex(conj:n,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   option('--semantics',amr), !,
   Sem = lam(P2,lam(P1,lam(X,merge(B:drs([B:[]:Y,B:[]:Z],
                                         [B:Index:pred(X,Sym,n,1),
                                          B:[]:rel(X,Y,op1,1),
                                          B:[]:rel(X,Z,op2,1)]),
                                   merge(app(P1,Y),
                                         app(P2,Z)))))).

semlex(conj:n,Sym,Index,Att-[sem:'AND'|Att],Sem):- 
   option('--semantics',drg), !,
   Sem = lam(P2,lam(P1,lam(X,merge(B:drs([B:[]:Y,B:[]:Z],
                                         [B:Index:pred(X,Sym,s,1),
                                          B:[]:rel(Y,X,subset_of,1),
                                          B:[]:rel(Z,X,subset_of,1)]),
                                   merge(app(P1,Y),
                                         app(P2,Z)))))).

semlex(conj:n,_Sym,_,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(P2,lam(P1,lam(X,merge(B:drs([B:[]:Y,B:[]:Z],
                                         [B:[]:rel(Y,X,subset_of,1),
                                          B:[]:rel(Z,X,subset_of,1)]),
                                   merge(app(P1,Y),
                                         app(P2,Z)))))).


/* =========================================================================
   Coordination: adjectives
========================================================================= */

semlex(conj:(n/n),_,_,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(A2,lam(A1,lam(P,lam(X,app(app(A1,lam(Y,app(app(A2,P),Y))),X))))).

% this the old analysis, where the noun is copied
semlex(conj:(n/n),_,_,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(A2,lam(A1,lam(P,lam(X,merge(B:drs([B:[]:Y,B:[]:Z],
                                               [B:[]:rel(Y,X,subset_of,1),
                                                B:[]:rel(Z,X,subset_of,1)]),
                                   merge(app(app(A1,P),Y),
                                         app(app(A2,P),Z))))))).


/* =========================================================================
   Coordination: noun phrases
========================================================================= */

semlex(conj:np,Sym,Index,Att-[sem:'AND'|Att],Sem):- 
   option('--semantics',amr), !,  
   Sem = lam(X2,lam(X1,lam(P,merge(merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,0)]),
                                         merge(app(X1,lam(Y,B1:drs([],[B1:[]:rel(X,Y,op1,1)]))),
                                               app(X2,lam(Z,B2:drs([],[B2:[]:rel(X,Z,op2,1)]))))),
                                   app(P,X))))).

semlex(conj:np,_,Index,Att-[sem:'AND'|Att],Sem):- !,  % collective
   Sem = lam(X2,lam(X1,lam(P,merge(merge(B:drs([B:Index:X],[]),
                                         merge(app(X1,lam(Y,B1:drs([],[B1:[]:rel(Y,X,subset_of,1)]))),
                                               app(X2,lam(Z,B2:drs([],[B2:[]:rel(Z,X,subset_of,1)]))))),
                                   app(P,X))))).

semlex(conj:app,_,Index,Att-[sem:'APP'|Att],Sem):- !,
   Sem = lam(X1,lam(X2,lam(P,app(X2,
                                 lam(Y,merge(app(X1,lam(X,B:drs([],[B:Index:rel(Y,X,rel,2)]))),
                                             app(P,Y))))))).


/* =========================================================================
   Coordination: verb phrases
========================================================================= */

semlex(conj:(s:_\np),Sym,Index,Att-[sem:'COO'|Att],Sem):-   
   option('--semantics',amr),
   option('--theory',sdrt), !,
   Sem = lam(V1,lam(V2,lam(N,lam(E,app(N,lam(X,sdrs([lab(K1,app(app(V2,lam(P,app(P,X))),E)),
                                                     lab(K2,app(app(V1,lam(P,app(P,X))),E))],
                                        [Index:rel(K1,K2,Sym)]))))))).

semlex(conj:(s:_\np),Sym,Index,Att-[sem:'COO'|Att],Sem):-   
   option('--semantics',amr),
   option('--theory',sdrt), !,
   Sem = lam(V1,lam(V2,lam(X,lam(E,sdrs([lab(K1,app(app(V2,X),E)),
                                         lab(K2,app(app(V1,X),E))],
                                        [Index:rel(K1,K2,Sym)]))))).


semlex(conj:(s:_\np),_Sym,Index,Att-[sem:'COO'|Att],Sem):-   
   option('--theory',sdrt), !,
   Sem = lam(V1,lam(V2,lam(N,lam(E,app(N,lam(X,sdrs([lab(K1,app(app(V2,lam(P,app(P,X))),E)),
                                                     lab(K2,app(app(V1,lam(P,app(P,X))),E))],
                                        [Index:rel(K1,K2,continuation),
                                         []:rel(K1,K2,parallel)]))))))).

semlex(conj:(s:_\np),_Sym,Index,Att-[sem:'COO'|Att],Sem):-    % VP coordination
   option('--theory',sdrt), !,
   Sem = lam(V1,lam(V2,lam(X,lam(E,sdrs([lab(K1,app(app(V2,X),E)),
                                         lab(K2,app(app(V1,X),E))],
                                        [Index:rel(K1,K2,continuation),
                                         []:rel(K1,K2,parallel)]))))).


/* =========================================================================
   Coordination: sentences
========================================================================= */

semlex(conj:(s:_),Sym,Index,Att-[sem:'COO'|Att],Sem):-   
   option('--semantics',amr),
   option('--theory',sdrt), !,
   Sem = lam(S1,lam(S2,lam(E,sdrs([lab(K1,app(S2,E)),
                                   lab(K2,app(S1,E))],
                                  [Index:rel(K1,K2,Sym)])))).

semlex(conj:(s:_),_Sym,Index,Att-[sem:'COO'|Att],Sem):-     % S coordination
   option('--theory',sdrt), !,
   Sem = lam(S1,lam(S2,lam(E,sdrs([lab(K1,app(S2,E)),
                                   lab(K2,app(S1,E))],
                                  [Index:rel(K1,K2,continuation),
                                   []:rel(K1,K2,parallel)])))).


/* =========================================================================
   Coordination: all other cases
========================================================================= */

semlex(conj:CCat,_,_,Att-[sem:'AND'|Att],Sem):- !,
   argCard(CCat,N),
   coordMacro(N,Coord),
   Sem = app(Coord,lam(K2,lam(K1,merge(K1,K2)))).


/* =========================================================================
   Compound Coordination
========================================================================= */

%semlex(conj:F/conj:F,instead,Index,Att-[sem:'UNK'|Att],Sem):- !,  % instead of
%   Sem = lam(C,lam(K1,lam(K2,app(app(C,B:drs([],[B:Index:not(K1)])),K2)))).

%semlex(conj:F/conj:F,rather,Index,Att-[sem:'UNK'|Att],Sem):- !,   % rather than
%   Sem = lam(C,lam(K1,lam(K2,app(app(C,B:drs([],[B:Index:not(K1)])),K2)))).

semlex(conj:F/conj:F,_,_,Att-[sem:'NIL'|Att],lam(U,U)):- !.
semlex(conj:F\conj:F,_,_,Att-[sem:'NIL'|Att],lam(U,U)):- !.


/* =========================================================================
   Quotes
========================================================================= */

semlex(q,_,Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(X,B:drs([],[B:Index:pred(X,quotation,n,2)])).

semlex((n/q)/n,_,Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(N,lam(Q,lam(X,merge(B:drs([],[B:Index:pred(X,quotation,n,2)]),
                                 merge(app(N,X),app(Q,X)))))).

semlex((np/q)/np,_,Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(NP,lam(Q,lam(P,app(NP,lam(X,merge(B:drs([],[B:Index:pred(X,quotation,n,2)]),
                                               merge(app(Q,X),app(P,X)))))))).

semlex((s:dcl/q)/s:dcl,_,Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(S,lam(Q,lam(F,app(S,lam(E,merge(B:drs([],[B:Index:pred(E,quotation,n,2)]),
                                             merge(app(Q,E),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Determiners
------------------------------------------------------------------------- */

semlex(np/n,Token,Index,Att,Sem):- !, semlex_det(Token,Index,Att,Sem).


/* -------------------------------------------------------------------------
   Possessives
------------------------------------------------------------------------- */

semlex(np/(n/pp),_,Index,Att-[sem:'HAS'|Att],Sem):- !,
   Sem = lam(RN,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,male,n,2)]),
                               alfa(def,merge(B2:drs([B2:[]:X],[]),
                                              app(app(RN,lam(U,B3:drs([],[B3:Index:rel(U,Y,of,1)]))),X)),
                                        app(P,X))))).


/* -------------------------------------------------------------------------
   Determiners (as many as X)
------------------------------------------------------------------------- */

semlex(((np/n)/pp)/(s:adj\np),_,Index,Att-[sem:'EQA'|Att],Sem):- !,
   closing(CC),
   Sem = lam(AP,lam(PP,lam(N,lam(P,merge(merge(B:drs([B:Index:X],[]),
                                               merge(app(N,X),
                                                     merge(app(PP,X),
                                                           app(app(AP,lam(Q,app(Q,X))),CC)))),
                                         app(P,X)))))).


/* -------------------------------------------------------------------------
   Many/Much [as NP]
------------------------------------------------------------------------- */

semlex(np,many,Index,Att-[sem:'QUA'|Att],Sem):- !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,quantity,n,1)]),app(P,X))).

semlex(np,much,Index,Att-[sem:'QUA'|Att],Sem):- !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,amount,n,3)]),app(P,X))).


/* -------------------------------------------------------------------------
   There insertion
------------------------------------------------------------------------- */

semlex(np,'there',Index,Att-[sem:'UNK'|Att],Sem):-
   option('--semantics',amr), 
   att(Att,pos,'EX'), !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,there,n,1)]),app(P,X))).

semlex(np,'there',Index,Att-[sem:'UNK'|Att],Sem):-
   att(Att,pos,'EX'), !,
   Sem = lam(P,merge(B:drs([B:Index:X],[]),app(P,X))).


/* -------------------------------------------------------------------------
   Pronouns (non-reflexives)
------------------------------------------------------------------------- */

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['I',i,me,mine]), !,
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,i,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,['I',i,me,mine]), !,
%  goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['we','us','\'s','ours']), !,
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,we,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,['we','us','\'s','ours']), !,
%  goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,['we','us','\'s','ours']), !,
%  goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B1:drs([B1:[]:G],[B1:Index:pred(G,group,n,1)]),
                        B2:drs([],[B2:[]:imp(B3:drs([B3:[]:X],[B3:[]:rel(X,G,member_of,0)]),
                                          merge(B4:drs([],[B4:[]:pred(X,person,n,1)]),       
                                                app(P,X)))]))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,[whom,you,thou,yours]), !,
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,you,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,[whom,you,thou,yours]), !,
%  goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['he','his','him']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,he,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,['he','his','him']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,male,n,2)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['she','hers','her']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,she,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,['she','hers','her']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,female,n,2)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,[it,'\'t']), !,
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,it,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,[it,'\'t']), !,
%  goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['they','them','theirs']), !,
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,they,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,['they','them','theirs']), !,
%  goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRO'|Att],Sem):-
   member(Lemma,['they','them','theirs']), !,
%  goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B1:drs([B1:[]:G],[B1:Index:pred(G,group,n,1)]),
                        B2:drs([],[B2:[]:imp(B2:drs([B2:[]:X],[B2:[]:rel(X,G,member_of,0)]),
                                          app(P,X))]))).


/* -------------------------------------------------------------------------
   Reflexive Pronouns 
------------------------------------------------------------------------- */

semlex( np, Lemma,Index,Att-[sem:'REF'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,[myself,yourself,thyself,ourselves]), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,i,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'REF'|Att],Sem):-
   member(Lemma,[myself,yourself,thyself,ourselves]), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'REF'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['himself']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,he,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'REF'|Att],Sem):-
   member(Lemma,['himself']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,male,n,2)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'REF'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['herself']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,she,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'REF'|Att],Sem):-
   member(Lemma,['herself']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,female,n,2)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'REF'|Att],Sem):-
   member(Lemma,['itself']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'REF'|Att],Sem):-
   member(Lemma,['themselves']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,group,n,1)]),app(P,X))).


/* -------------------------------------------------------------------------
   Demonstratives and Quantificational Noun Phrases
------------------------------------------------------------------------- */

semlex( np, Lemma,Index,Att-[sem:'NOT'|Att],Sem):-
   \+ option('--semantics',amr),
   member(Lemma,['none','neither',nothing]), !,
   Sem = lam(P,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),app(P,X)))])).

semlex( np, Lemma,Index,Att-[sem:'DIS'|Att],Sem):-
   \+ option('--semantics',amr),
   member(Lemma,[something,some,'both','most','more','many','less','half','another']), !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRX'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['this','these']), !,
   Sem = lam(P,alfa(def,B:drs([B:[]:X],[B:Index:pred(X,this,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'DST'|Att],Sem):-
   option('--semantics',amr),
   member(Lemma,['that','those']), !,
   Sem = lam(P,alfa(def,B:drs([B:[]:X],[B:Index:pred(X,that,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'PRX'|Att],Sem):-
   member(Lemma,['this','these']), !,
   Sem = lam(P,alfa(def,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'DST'|Att],Sem):-
   member(Lemma,['that','those']), !,
   Sem = lam(P,alfa(def,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-[sem:'AND'|Att],Sem):-
   \+ option('--semantics',amr),
   member(Lemma,['all','any','each','either',everything,anything]), !,
   Sem = lam(P,B1:drs([],[B1:[]:imp(B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),app(P,X))])).

semlex( np, Lemma,Index,Att-[sem:'AND'|Att],Sem):-
   \+ option('--semantics',amr),
   member(Lemma,[everybody,everyone,anybody,anyone]), !,
   Sem = lam(P,B1:drs([],[B1:[]:imp(B2:drs([B2:[]:X],[B2:Index:pred(X,person,n,1)]),app(P,X))])).

semlex( np, Lemma,Index,Att-[sem:'NOT'|Att],Sem):-
   \+ option('--semantics',amr),
   member(Lemma,[nobody,noone,'no-one']), !,
%  Sem = lam(P,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:X],[B2:Index:pred(X,people,n,1)]),app(P,X)))])).
   Sem = lam(P,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:X],[B2:Index:pred(X,person,n,1)]),app(P,X)))])).

semlex( np, Lemma,Index,Att-[sem:'AND'|Att],Sem):-
   \+ option('--semantics',amr),
   member(Lemma,[someone,somebody]), !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).


/* -------------------------------------------------------------------------
   NP (semantically empty)

semlex( np_exp, _Lemma,Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(P,merge(B:drs([B:Index:X],[]),app(P,X))).

semlex( np_thr, _Lemma,Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(P,merge(B:drs([B:Index:X],[]),app(P,X))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   NP Why
------------------------------------------------------------------------- */

semlex( np, Lemma,Index,Att-[sem:'QUE'|Att],Sem):-
   Lemma = 'why', !,
   Sem = lam(P,B1:drs([],[B1:[]:duplex(whq,
                                     B2:drs([B2:[]:X],[B2:Index:pred(X,reason,n,2)]),
                                     X,
                                     app(P,X))])).


/* -------------------------------------------------------------------------
   NP (all others)
------------------------------------------------------------------------- */

semlex(np,Sym,Index,Att-[sem:Tag|Att],Sem):- 
   att(Att,pos,Pos), member(Pos,['NNP','NNPS']), !,
   att(Att,namex,Ne), neClassType(Ne,Class,Type,Tag),
   Sem = lam(P,alfa(nam,B:drs([B:[]:X],[B:Index:named(X,Sym,Class,Type)]),app(P,X))).

semlex(np,Sym,Index,Att-[sem:'CON'|Att],Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense)]),app(P,X))).


/* -------------------------------------------------------------------------
   NP/PP
------------------------------------------------------------------------- */

semlex(np/pp, Sym,Index,Att-[sem:'REL'|Att],Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense)]),
                            merge(app(P,X),app(PP,X))))).



/* -------------------------------------------------------------------------
   Question words: whose
------------------------------------------------------------------------- */

semlex(Cat,whose,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[(s:wq/(s:dcl\np))/n,
               (s:wq/(s:q/np))/n,
               (s:wq\(s:dcl/np))/n]), !, 
   Sem = lam(N,lam(V,app(V,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                                         merge(merge(B2:drs([B2:[]:Y],[]),app(N,Y)),
                                                               B3:drs([B3:[]:X],[B3:Index:pred(X,person,n,1),B3:[]:rel(Y,X,of,1)])),
                                                         X, 
                                                         app(P,Y))]))))).


/* -------------------------------------------------------------------------
   Question words: which/what N
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[(s:wq/(s:dcl\np))/n,
               (s:wq/(s:q/np))/n,
               (s:qem/(s:dcl\np))/n,
               (s:qem/(s:dcl/np))/n,
               (s:wq\(s:dcl/np))/n]), !, 
   Sem = lam(P1,lam(V2,app(V2,lam(P3,B1:drs([],[B1:[]:duplex(whq,
                                             merge(B2:drs([B2:Index:X4],[]),app(P1,X4)),
                                             X4,
                                             app(P3,X4))]))))).

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   Cat = (s:wq/(s:q/pp))/n, !,  % WH-DET N + YNQ
   Sem = lam(N,lam(V,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                              merge(B2:drs([B2:[]:X],[]),app(N,X)),
                                              X,
                                              app(app(V,lam(Y,B3:drs([],[B3:Index:rel(Y,X,rel,0)]))),E))])))).


/* -------------------------------------------------------------------------
   Question words: how much/many 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[(s:wq/(s:q/np))/np,
               (s:wq/(s:dcl\np))/np]), !, 
   Sem = lam(NP,lam(VP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                               merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                     app(NP,lam(U,B3:drs([],[B3:[]:eq(U,X)])))),
                                               Y,
                                               app(app(VP,lam(P,app(P,X))),E))])))).



/* -------------------------------------------------------------------------
   Question words: how much/many N 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[((s:wq/(s:q/np))/n)/(np/n),        
               ((s:wq/(s:dcl\np))/n)/(np/n)]), !, 
   Sem = lam(D,lam(N,lam(VP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                                    merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                    Y,
                                                    app(app(VP,lam(P,app(P,X))),E))]))))).

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[((s:wq/(s:q/pp))/n)/(np/n),        
               ((s:wq/(s:dcl\pp))/n)/(np/n)]), !, 
   Sem = lam(D,lam(N,lam(VP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                                    merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                    Y,
                                                    app(app(VP,lam(Y,B4:drs([],[B4:[]:rel(Y,X,rel,0)]))),E))]))))).

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[(((s:wq/pp)/((s:q/pp)/np))/n)/(np/n)]), !,
   Sem = lam(D,lam(N,lam(TV,lam(PP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                                           merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                                 app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                           Y,
                                                           app(app(app(TV,lam(P,app(P,X))),PP),E))])))))).


semlex(((s:wq/pp)/n)/(np/n),_Sym,Index,Att-[sem:'QUE'|Att],Sem):- !,  % American English dialect (How many feet in a mile?)
   Sem = lam(D,lam(N,lam(PP,lam(_,B1:drs([],[B1:[]:duplex(whq,
                                                    merge(B2:drs([B2:[]:X,B2:[]:Y],[Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                    Y,                                           
                                                    app(PP,X))]))))).
         

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[(((s:wq/(s:pss\np))/((s:q/(s:pss\np))/np))/n)/(np/n)]),
   Sem = lam(D,lam(N,lam(_,lam(VP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                                          merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                                app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                          Y,
                                                          app(app(VP,lam(P,app(P,X))),E))])))))).



/* -------------------------------------------------------------------------
   Question words: how ADJ 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[(s:wq/(s:q/(s:adj\np)))/(s:adj\np),
               ((s:wq/pp)/((s:q/pp)/(s:adj\np)))/(s:adj\np),
               (s:qem/(s:dcl/(s:adj\np)))/(s:adj\np)]), !, % How ADJ
   Sem = lam(A,lam(U,app(U,lam(NP,lam(E,app(NP,lam(X,B1:drs([],[B1:[]:duplex(whq,
                                                                       merge(B2:drs([B2:Index:Y],[]),
                                                                             app(app(A,lam(P,app(P,Y))),E)),
                                                                       Y,
                                                                       B3:drs([],[B3:[]:rel(Y,X,of,1)]))])))))))).


semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   Cat = (s:wq/(s:q/pp))/(s:adj\np), !, % How often does...
   closing(CC),
   Sem = lam(A,lam(VP,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                              merge(B2:drs([B2:Index:X],[]),
                                                    app(app(A,lam(P,app(P,X))),CC)),
                                              X,
                                              app(app(VP,lam(Y,B3:drs([],[B3:[]:rel(Y,X,rel,0)]))),F))])))).


/* -------------------------------------------------------------------------
   Question words: basic question words 
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[s:wq/(s:dcl\np),  
               s:wq/(s:q/np),  
               s:wq\(s:dcl/np)]), 
   ( Sym = what,      Pred = thing,        Sense=12;
     Sym = whatever,  Pred = thing,        Sense=12;
     Sym = which,     Pred = thing,        Sense=12;
     Sym = whichever, Pred = thing,        Sense=12;
     Sym = where,     Pred = location,     Sense=1;
     Sym = why,       Pred = reason,       Sense=2;
     Sym = how,       Pred = manner,       Sense=2; 
     Sym = who,       Pred = person,       Sense=1;      
     Sym = whoever,   Pred = person,       Sense=1;      
     Sym = whom,      Pred = person,       Sense=1;      
     Sym = when,      Pred = unit_of_time, Sense=1 
   ), !,
   Sem = lam(VP,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                        B2:drs([B2:[]:X],[B2:Index:pred(X,Pred,n,Sense)]),
                                        X,
                                        app(app(VP,lam(P,app(P,X))),F))]))).


semlex(Cat,Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   Cat = s:wq/(s:q/pp), 
   ( Sym=where, Pred=location,     Rel=loc_rel,  Sense=1;
     Sym=why,   Pred=reason,       Rel=rel,      Sense=2;
     Sym=how,   Pred=manner,       Rel=rel,      Sense=2;
     Sym=when,  Pred=unit_of_time, Rel=temp_rel, Sense=1
   ), !, 
   Sem = lam(VP,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                        B2:drs([B2:[]:X],[B2:Index:pred(X,Pred,n,Sense)]),
                                        X,
                                        app(app(VP,lam(E,B3:drs([],[B3:[]:rel(E,X,Rel,0)]))),F))]))).

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[np/(s:dcl\np),
               np/(s:dcl/np)]), !,
   closing(CC),
   Sem = lam(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                        B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),
                                        X,
                                        merge(app(app(VP,lam(R,app(R,X))),CC),app(P,X)))]))). 

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[np/((s:to\np)/np)]), !,
   closing(CC),
   Sem = lam(TV,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                        B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),
                                        X,
                                        merge(app(app(app(TV,lam(R,app(R,X))),lam(Q,merge(B3:drs([B3:[]:Z],[B3:[]:pred(Z,thing,n,12)]),
                                                                                          app(Q,Z)))),CC),app(P,X)))]))). 

semlex(Cat,_Sym,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[(np/(s:dcl\np))/n,
               (np/(s:dcl/np))/n]), !,
   closing(CC),
   Sem = lam(N,lam(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                              merge(B2:drs([B2:Index:X],[]),app(N,X)),
                                              X,
                                              merge(app(app(VP,lam(R,app(R,X))),CC),app(P,X)))])))). 


semlex(s:wq/s:q,Sym,Index,Att-[sem:'QUE'|Att],Sem):- 
   ( Sym=how,   Pred=manner,       Sense=2 ;
     Sym=where, Pred=location,     Sense=1 ;
     Sym=when,  Pred=unit_of_time, Sense=1 ;
     Sym=why,   Pred=reason,       Sense=2 ;
     Sym=what,  Pred=thing,        Sense=12 
   ), !,
   Sem = lam(YNQ,lam(E,app(YNQ,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                                       B2:drs([B2:[]:X],[B2:Index:pred(X,Pred,n,Sense)]),    
                                                       X,
                                                       merge(B3:drs([],[B3:[]:rel(F,X,rel,0)]),app(E,F)))]))))). 

semlex(s:qem/(s:to\np),_,Index,Att-[sem:'QUE'|Att],Sem):- !,
   Sem = lam(VP,lam(E,app(app(VP,lam(P,merge(B1:drs([B1:[]:X],[]),app(P,X)))),lam(F,merge(B2:drs([],[B2:Index:pred(F,manner,n,2)]),app(E,F)))))). % how to

% whose
semlex(Cat,whose,Index,Att-[sem:'QUE'|Att],Sem):- 
   member(Cat,[s:wq/(s:q/np),
               s:wq\(s:dcl/np),
               s:wq/(s:dcl\np)]), !,
   Sem = lam(VP,app(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                                     B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:pred(X,thing,n,12),
                                                                               B2:[]:pred(Y,person,n,1),
                                                                               B2:[]:rel(X,Y,of,1)]),
                                                     X,
                                                     app(P,X))])))).

semlex(Cat,_,Index,Att-[sem:'QUE'|Att],Sem):- 
   member(Cat,[s:qem/(s:dcl\np),
               s:_/(s:dcl\np),
               s:qem/(s:dcl/np)]), !,   
   Sem = lam(VP,app(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                               B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),
                                               X,
                                               app(P,X))])))).

% how
semlex((s:qem/s:dcl)/(s:adj\np),_,Index,Att-[sem:'QUE'|Att],Sem):- !,
   Sem = lam(VP,lam(S,app(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                                     merge(B2:drs([B2:[]:X],[B2:Index:pred(X,manner,n,2)]),app(P,X)),
                                                     X,
                                                     app(S,lam(E,B3:drs([],[B3:[]:rel(E,X,rel,0)]))))]))))).

% how much energy was lost (??)
% how many years has GP been campaigning
semlex(Cat,_,Index,Att-[sem:'QUE'|Att],Sem):-
   member(Cat,[((s:qem/(s:dcl\np))/n)/(s:adj\np),
               ((s:qem/(s:dcl/np))/n)/(s:adj\np)]), !,
   closing(CC),
   Sem = lam(VPADJ,lam(N,lam(VPDCL,lam(E,app(app(VPADJ,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                                                                merge(B2:drs([B2:Index:X],[]),
                                                                                      merge(app(N,X),
                                                                                            app(P,X))),
                                                                     X,
                                                                     app(app(VPDCL,lam(P,app(P,X))),E))]))),CC))))).

% why does he always wait
semlex((s:X\s:X)/s:q,_,Index,Att-[sem:'QUE'|Att],Sem):- !,
   Sem = lam(W,lam(S,lam(F,app(S,lam(E,merge(B1:drs([B1:[]:Y],[B1:[]:prop(Y,B2:drs([],[B2:[]:duplex(whq,
                                                                                           B3:drs([B3:[]:Z],[B3:Index:pred(Z,reason,n,2)]),
                                                                                           Z,
                                                                                           app(W,lam(E,B4:drs([],[B4:[]:rel(E,Z,rel,0)]))))])),
                                                            B1:[]:rel(E,Y,rel,0)]),
                                             app(F,E))))))).


/* =========================================================================
   Relative pronouns, pied-piping ("N under which S", "NP under which S")
========================================================================= */

semlex(((np\np)/s:dcl)\((s:F\s:F)/np),_Sym,_Index,Att-[sem:'UNK'|Att],Sem):- !,
   closing(CC),
   Sem = lam(Prep,lam(S,lam(NP,lam(Q,app(app(app(Prep,lam(P,app(NP,lam(X,merge(app(P,X),app(Q,X)))))),S),CC))))). 

semlex(((np\np)/s:dcl)\((np\np)/np),_Sym,_Index,Att-[sem:'UNK'|Att],Sem):- !,
   closing(CC),
   Sem = lam(Prep,lam(S,lam(NP,lam(Q,app(app(app(Prep,lam(P,app(NP,lam(X,merge(app(P,X),app(Q,X)))))),S),CC))))). 

semlex(((n\n)/s:dcl)\((n\n)/np),_Sym,_Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(Prep,lam(S,lam(N,app(app(Prep,S),N)))).

semlex(((np\np)/s:dcl)\((n\n)/np),_Sym,_Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(Prep,lam(S,lam(NP,lam(P,app(NP,app(app(Prep,S),P)))))).



/* =========================================================================
   Verbs
========================================================================= */

semlex(Cat,Sym,Index,Att,Sem):- semlex_verb(Cat,Sym,Index,Att,Sem), !.



/* =========================================================================
   Other Modifiers
========================================================================= */

/* -------------------------------------------------------------------------
   Superlative: at least/most/best (no idea how to specify semantics)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'TOP'|Att],Sem):-
   member(Cat,[(s:_/s:_)/(s:asup\np),(np/np)/(s:asup\np)]), !,
   Sem = lam(VP,lam(NP,lam(P,app(app(VP,NP),lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,0)]),app(P,X))))))).

semlex(Cat,_Sym,_Index,Att-[sem:'NIL'|Att],Sem):-
   member(Cat,[(C/C)/(s:asup\np), 
               (C\C)/(s:asup\np), 
               (C\C)\(s:asup\np),
               (C/C)\(s:asup\np)]), !,
   Sem = lam(_,lam(N,N)).


/* -------------------------------------------------------------------------
   Comparatives
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'MOR'|Att],Sem):-
   member(Cat,[(np/np)\(s:adj\np),
               (np/np)/(s:adj\np)]), !,
   att(Att,sense,Sense),
   Sem = lam(VP,lam(NP,lam(P,app(app(VP,NP),lam(X,merge(B:drs([],[B:Index:pred(X,Sym,r,Sense)]),app(P,X))))))).

semlex(Cat,Sym,Index,Att-[sem:'MOR'|Att],Sem):-
   member(Cat,[((np/np)/(np/np))\(s:adj\np),
               ((np\np)\(np\np))/(s:dcl\np)]), !, 
   att(Att,sense,Sense),
   Sem = lam(VP,lam(NPNP,lam(NP,lam(P,app(app(VP,app(NPNP,NP)),lam(X,merge(B:drs([],[B:Index:pred(X,Sym,r,Sense)]),app(P,X)))))))).

semlex(Cat,Sym,Index,Att-[sem:'MOR'|Att],Sem):-
   Cat = ((((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))))\(s:adj\np), !,
   att(Att,sense,Sense),
   Sem = lam(_AP,lam(MM,lam(M,lam(VP,app(app(MM,M),lam(Q,lam(F,app(app(VP,Q),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E))))))))))).



/* -------------------------------------------------------------------------
   Complementizers (Wh)
------------------------------------------------------------------------- */

semlex(s:qem/s:dcl,Sym,Index,Att-[sem:'QUE'|Att],Sem):- 
   ( Sym=how,   Pred=manner,       Sense=2, Rel=manner_rel;
     Sym=where, Pred=location,     Sense=1, Rel=loc_rel;
     Sym=when,  Pred=unit_of_time, Sense=1, Rel=time_rel;
     Sym=why,   Pred=reason,       Sense=2, Rel=reason_rel), !,
   Sem = lam(S,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                       B2:drs([B2:[]:X],[B2:Index:pred(X,Pred,n,Sense)]),
                                       X,
                                       app(S,lam(E,merge(B3:drs([],[B3:[]:rel(E,X,Rel,0)]),
                                                         app(F,E)))))]))).


/* -------------------------------------------------------------------------
   Complementizers 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   member(Cat,[s:em/s:dcl,s:bem/s:b,s:em/s:b,s:qem/s:dcl]), !,
   plosing(CC),
   Sem = lam(S,lam(E,merge(B:drs([B:Index:K],
                                 [B:[]:prop(K,app(S,CC))]),
                           app(E,K)))).

% it is illegal for children to smoke
%
semlex(Cat,_,_Index,Att-[sem:'SUB'|Att],Sem):- 
   Cat = (s:for/(s:to\np))/np, !,
   Sem = lam(NP,lam(VP,app(VP,NP))).

%semlex(Cat,_,Index,Att-[sem:'UNK'|Att],Sem):- 
%   Cat = (s:for/(s:to\np))/np, !,
%   plosing(CC),
%   Sem = lam(NP,lam(VP,lam(E,merge(B:drs([B:Index:K],
%                                         [B:[]:prop(K,app(app(VP,NP),CC))]),
%                                   app(E,K))))).

semlex(Cat,_Sym,_Index,Att-[sem:'NIL'|Att],Sem):-
   category(comp,Cat,_), !,
   Sem = lam(S,lam(F,app(S,F))).

semlex(Cat,_,Index,Att-[sem:'SUB'|Att],Sem):-
   Cat = np/s:dcl, !,
   plosing(CC),
   Sem = lam(S,lam(P,merge(B:drs([B:Index:X],
                                 [B:[]:prop(X,app(S,CC))]),
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Locative Adverbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'DIS'|Att2],Sem):-
   category(vpadv,Cat,_),
   member(Sym,[somewhere]), !,
   role(['Location'],Att1-Att2,[Role]),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:Z],[B:Index:pred(Z,location,n,1),
                                                                    B:[]:role(E,Z,Role,1)]),app(F,E))))))).

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   category(vpadv,Cat,_),
   member(Sym,[anywhere,everywhere]), !,
   role(['Location'],Att1-Att2,[Role]),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,B1:drs([],[B1:[]:imp(B2:drs([B2:[]:Z],[B2:Index:pred(Z,location,n,1)]),
                                                                   merge(B3:drs([],[B3:[]:role(E,Z,Role,1)]),app(F,E)))])))))).

semlex(Cat,Sym,Index,Att1-[sem:'NOT'|Att2],Sem):-
   category(vpadv,Cat,_),
   member(Sym,[nowhere]), !,
   role(['Location'],Att1-Att2,[Role]),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:Z],[B2:[]:pred(Z,location,n,1),
                                                                                              B2:[]:role(E,Z,Role,1)]),app(F,E)))])))))).


/* -------------------------------------------------------------------------
   Not 
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   category(vpadv,Cat,_),
   option('--semantics',drg),
   notSymbol(Sym), !,
   Sem = lam(X,lam(Q,lam(F,B1:drs([],[B1:[]:not(app(app(X,Q),lam(E,merge(B2:drs([],[B2:Index:pred(E,Sym,s,1)]),app(F,E)))))])))).

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   category(vpadv,Cat,_),
   notSymbol(Sym), 
   option('--x',true), !,
%  Sem = lam(V,lam(Q,lam(F,app(Q,lam(X,B:drs([],[B:Index:not(app(app(V,lam(P,app(P,X))),F))])))))). %%% subject wide scope (preferred?)
   Sem = lam(X,lam(Q,lam(F,B:drs([],[B:Index:not(app(app(X,Q),F))])))).                             %%% negation wide scope (dispreferred?)

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   category(vpadv,Cat,_),
   notSymbol(Sym), !,
   Sem = lam(V,lam(Q,lam(F,app(Q,lam(X,B:drs([],[B:Index:not(app(app(V,lam(P,app(P,X))),F))])))))). %%% subject wide scope (preferred?)
%  Sem = lam(X,lam(Q,lam(F,B:drs([],[B:Index:not(app(app(X,Q),F))])))).                             %%% negation wide scope (dispreferred?)

semlex(Cat,Sym,Index,Att-[sem:'POS'|Att],Sem):-
   category(vpadv,Cat,_),
   member(Sym,[perhaps,maybe,possibly]),
   option('--modal',true), !,
   Sem = lam(V,lam(Q,lam(F,app(Q,lam(X,B:drs([],[B:Index:pos(app(app(V,lam(P,app(P,X))),F))])))))). %%% subject wide scope (preferred?)
%  Sem = lam(X,lam(Q,lam(F,B:drs([],[B:Index:not(app(app(X,Q),F))])))).                             %%% negation wide scope (dispreferred?)


/* -------------------------------------------------------------------------
   Cardinals that function as VP modifiers (often wrongly analysed)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'QUA'|Att2],Sem):-
   att(Att1,pos,'CD'),
   category(vpadv,Cat,_),
   string2digit(Sym,Digit), !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:X],[B:Index:card(X,Digit,eq),
                                                                    B:[]:rel(E,X,Relation,0)]),app(F,E))))))).

/* -------------------------------------------------------------------------
   NPs that function as VP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:Tag|Att2],Sem):-
   category(vpadv,Cat,_),
   att(Att1,pos,Pos),
   member(Pos,['NNP','NNPS']), !,
   att(Att1,namex,NE), neClassType(NE,Class,Type,Tag),
   rel(on,Att1-Att2,Relation),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:X],[B:Index:named(X,Sym,Class,Type),
                                                                    B:[]:rel(E,X,Relation,0)]),app(F,E))))))).

semlex(Cat,Sym,Index,Att1-[sem:'CON'|Att2],Sem):-
   category(vpadv,Cat,_),
   att(Att1,pos,Pos),
   member(Pos,['NN','NNS']), !,
   att(Att1,sense,Sense),
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense),
                                                                    B:[]:role(E,X,Role,1)]),app(F,E))))))).


/* -------------------------------------------------------------------------
   Comparative (more)

semlex(Cat,Sym,Index,Att-[sem:'MOR'|Att],Sem):-
   Sym = more,
   Cat = (s:adj\np)/(s:adj\np), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(D1,merge(B:drs([B:[]:D2],[B:Index:rel(D1,D2,more,0)]),app(F,D1))))))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Adverbs (VP modifying)
------------------------------------------------------------------------- */

%semlex(Cat,Sym,Index,Att-[sem:'UNK'|Att],Sem):-
%   category(vpadv,Cat,_), 
%   option('--x',true),
%   negprefix(_, Sym, Prefix, Core), !,
%   Sem = lam(X,lam(Q,lam(F,B1:drs([],[B1:Index:not(app(app(X,Q),lam(E,merge(B2:drs([],[B2:Index:pred(E,Prefix,a,71),B2:Index:pred(E,Core,a,1)]),app(F,E)))))])))).

%semlex(Cat,Sym,Index,Att-[sem:'UNK'|Att],Sem):-
%   category(vpadv,Cat,_), 
%   option('--x',true), 
%   negsuffix(_, Sym, Suffix, Core), !,
%   Sem = lam(X,lam(Q,lam(F,B1:drs([],[B1:Index:not(app(app(X,Q),lam(E,merge(B2:drs([],[B2:Index:pred(E,Suffix,a,72),B2:Index:pred(E,Core,a,1)]),app(F,E)))))])))).

semlex(Cat,Sym,Index,Att1-[sem:'QUA'|Att2],Sem):-
   category(vpadv,Cat,_), 
   member(Sym:Card,[once:1,twice:2,thrice:3]), !,
   role(['Frequency'],Att1-Att2,[Role]),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:M],[B:[]:role(E,M,Role,1),
                                                                    B:Index:pred(M,time,n,1),
                                                                    B:[]:card(M,Card,eq)]),app(F,E))))))).

semlex(Cat,Sym,Index,Att1-[sem:'IST'|Att2],Sem):-
   category(vpadv,Cat,_), !, 
   att(Att1,sense,Sense),
   role(['Manner'],Att1-Att2,[Role]),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:M],[B:[]:role(E,M,Role,1),
                                                                    B:Index:pred(M,Sym,a,Sense)]),app(F,E))))))).
%   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E))))))).


/* -------------------------------------------------------------------------
   Definite prepositions 
------------------------------------------------------------------------- */

% "the" as apposition trigger
%
semlex(Cat,the,Index,Att-[sem:'APP'|Att],Sem):- 
   Cat = (np\np)/n, !,
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:eq(X,Y)]),
                                             merge(app(N,Y),app(P,X)))))))).

% temporal
%
semlex(Cat,Sym,Index,Att1-[sem:'PRX'|Att2],Sem):- 
   option('--semantics',amr),
   member(Sym,[this,those]),
   Cat = (np\np)/n, !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:pred(Y,this,r,2),B:[]:role(X,Y,Role,1)]),
                                             merge(app(N,Y),app(P,X)))))))).

semlex(Cat,Sym,Index,Att1-[sem:'DST'|Att2],Sem):- 
   option('--semantics',amr),
   member(Sym,[that,these]),
   Cat = (np\np)/n, !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:pred(Y,that,r,2),B:[]:role(X,Y,Role,1)]),
                                             merge(app(N,Y),app(P,X)))))))).

semlex(Cat,Sym,Index,Att1-[sem:'PRX'|Att2],Sem):- 
   member(Sym,[this,these]),
   Cat = (np\np)/n, !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:role(X,Y,Role,1)]),
                                             merge(app(N,Y),app(P,X)))))))).

semlex(Cat,Sym,Index,Att1-[sem:'DST'|Att2],Sem):- 
   member(Sym,[this,that,those,these]),
   Cat = (np\np)/n, !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:role(X,Y,Role,1)]),
                                             merge(app(N,Y),app(P,X)))))))).

% seven cents a share
%
semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Sym,[a,an,each]),
   Cat = (np\np)/n, !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(N,lam(Q,lam(P,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:[]:Y],[]),
                                                      app(N,Y)),
                                                app(Q,lam(X,merge(B3:drs([],[B3:Index:rel(X,Y,Relation,0)]),
                                                               app(P,X)))))])))).

% release June 5
%
semlex(Cat,Sym,Index,Att1-[sem:'MOY'|Att2],Sem):- 
   Cat = (np\np)/n, 
   att(Att1,pos,'NNP'),
   att(Att1,namex,NE), neClass(NE,tim),
   role(['Time'],Att1-Att2,[Role]),
   month(Sym,MID), !,
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(merge(B:drs([B:[]:Y],[B:[]:role(X,Y,Role,1)]),
                                                   merge(B:drs([],[B:Index:timex(Y,date([]:'+',[]:'XXXX',Index:MID,[]:'XX'))]),app(N,Y))),
                                             app(P,X))))))).

% To be done: $, another, both, no
%
semlex(Cat,_Sym,Index,Att-[sem:'UNK'|Att],Sem):- 
   Cat = (np\np)/n, !,
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,rel,0)]),
                                             merge(app(N,Y),app(P,X)))))))).


/* -------------------------------------------------------------------------
   Prepositional Phrases
------------------------------------------------------------------------- */

semlex(pp,Sym,Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(X,B:drs([B:[]:Y],[B:Index:pred(Y,thing,n,12),
                               B:[]:rel(X,Y,Sym,0)])).


/* -------------------------------------------------------------------------
   Prepositions
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((np\np)\(np\np))/np,
               ((np\np)/(np\np))/np]), !,
   Sem = lam(Q1,lam(R,lam(Q2,lam(P,merge(app(Q1,lam(X,app(Q2,lam(Y,B:drs([],[B:Index:rel(Y,X,Sym,0)]))))),app(app(R,Q2),P)))))).

semlex(Cat,_Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Cat = (np/np)/n, !,
   Sem = lam(N,lam(Q,lam(P,merge(merge(B1:drs([B1:[]:Y],[]),app(N,Y)),app(Q,lam(X,merge(B2:drs([],[B2:Index:rel(X,Y,rel,0)]),app(P,X)))))))).

% except
%
semlex(Cat,except,Index,Att-[sem:'NOT'|Att],Sem):-
   Cat = (np\np)/pp, !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(B:drs([],[B:Index:not(app(PP,X))]),app(P,X))))))).

% ... countries depending on ...
%
semlex(Cat,Sym,Index,Att1-[sem:'PRE'|Att2],Sem):-
   Cat = (np\np)/pp, 
   att(Att1,sense,Sense),
   att(Att1,pos,'VBG'), 
   roles(Sym,s:dcl\np,[Role],Att1-Att2), !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(B:drs([B:[]:E],[B:Index:pred(E,Sym,v,Sense),
                                                                B:[]:role(X,E,Role,-1)]),merge(app(PP,E),app(P,X)))))))).

% ... infrastructure compared to ...
%
semlex(Cat,Sym,Index,Att1-[sem:'PAS'|Att2],Sem):-
   Cat = (np\np)/pp, 
   att(Att1,sense,Sense),
   att(Att1,pos,'VBN'), 
   roles(Sym,s:pss\np,[Role],Att1-Att2), !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(B:drs([B:[]:E],[B:Index:pred(E,Sym,v,Sense),
                                                                B:[]:role(X,E,Role,-1)]),merge(app(PP,E),app(P,X)))))))).

% double prepositions: 
%
semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Cat = (np\np)/pp, !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(B:drs([B:[]:Y],[B:[]:pred(Y,thing,n,12), 
                                                                B:Index:rel(X,Y,Sym,Sense)]),merge(app(PP,Y),app(P,X)))))))).

semlex(Cat,Tok,Index,Att-[sem:'NOT'|Att],Sem):-
   \+ option('--semantics',amr),
   Tok = without, Sym = with,
   member(Cat,[(np\np)/np,(np/np)/np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,merge(B1:drs([],[B1:Index:not(app(Q1,lam(Y,B2:drs([],[B2:Index:rel(X,Y,Sym,0)]))))]),app(P,X))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(np\np)/np,(np\np)\np,(np/np)/np]),
   att(Att,scope,inv), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q1,lam(Y,app(Q2,lam(X,merge(B:drs([],[B:Index:rel(X,Y,Sym,0)]),app(P,X))))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(np\np)/np,(np\np)\np,(np/np)/np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(B:drs([],[B:Index:rel(X,Y,Sym,0)]),app(P,X))))))))).

% permafrost three meters below the surface
semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Cat = ((np\np)\np)/np, !,
   Sem = lam(Q1,lam(Q2,lam(Q3,lam(P,app(Q3,lam(X,app(Q2,lam(Y,app(Q1,lam(Z,merge(B:drs([],[B:[]:rel(X,Y,rel,0),
                                                                                           B:Index:rel(Y,Z,Sym,0)]),
                                                                                 app(P,X)))))))))))).

% for it to take effect
%
semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):- 
   Cat = ((np\np)/(s:to\np))/np, !,
   Sem = lam(NP1,lam(VP,lam(NP2,lam(P,app(NP2,lam(X,merge(app(app(VP,NP1),
                                                              lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)]))),
                                                          app(P,X)))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):- 
   Cat = ((np\np)/(s:ng\np))/np, !,
   plosing(CC),
   Sem = lam(NP1,lam(VP,lam(NP2,lam(P,app(NP2,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,Sym,0),
                                                                          B:[]:prop(Y,app(app(VP,NP1),CC))]),
                                                          app(P,X)))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):- 
   member(Cat,[((np\np)/pp)/np]), !,
   Sem = lam(Q1,lam(PP,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(B:drs([],[B:Index:rel(X,Y,Sym,0)]),
                                                                    merge(app(PP,X),app(P,X))))))))))).

semlex((n/pp)/(s:adj\np),Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Sem = lam(VP,lam(PP,lam(X,app(app(VP,lam(P,app(P,X))),lam(E,merge(B:drs([],[B:Index:rel(X,E,Sym,0)]),app(PP,E))))))).

semlex(((s:wq/s:q)\(s:wq/s:q))/np,Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(NP,lam(U,lam(YNQ,lam(F,app(app(U,YNQ),lam(E,merge(app(NP,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))),app(F,E)))))))).


semlex(Cat,_,Index,Att-[sem:'INT'|Att],Sem):-
   member(Cat,[((s:adj\np)/(s:adj\np))/n]), !, %  a bit
   Sem = lam(N,lam(A,lam(Q,lam(F,app(app(A,Q),lam(E,merge(merge(B:drs([B:[]:X],[B:Index:rel(E,X,rel,0)]),
                                                                 app(N,X)),
                                                           app(F,E)))))))).

% Passive Clause
%
semlex(Cat,by,Index,Att1-[sem:'AND'|Att2],Sem):-
   Cat = ((s:pss\np)\(s:pss\np))/np, 
   roles(by,Cat,[Role],Att1-Att2), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(B:drs([],[B:Index:role(E,Y,Role,1)]),app(F,E)))))))))).
     
               
semlex(Cat,Tok,Index,Att-[sem:'NOT'|Att],Sem):-
   \+ option('--semantics',amr),
   Tok = without, Sym = with,
   member(Cat,[((s:X\np)\(s:X\np))/np, 
               ((s:X\np)/(s:X\np))/np]), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B1:drs([],[B1:Index:not(app(Q2,lam(Y,B2:drs([],[B2:Index:rel(E,Y,Sym,0)]))))]),
                                                           app(F,E)))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/np,
	       ((s:X\np)/(s:X\np))/np,
               ((s:X\np)\(s:X\np))\np,
               ((s:X\np)/(s:X\np))\np]),
   att(Att,scope,inv), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(Q2,lam(Y,app(app(V,Q),lam(E,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/np, 
               ((s:X\np)/(s:X\np))/np, 
               ((s:X\np)\(s:X\np))\np, 
               ((s:X\np)/(s:X\np))\np]), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).

% from 8% the week before
semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\np)/np,
               (((s:X\np)\(s:X\np))/np)/np]), !,
   Sem = lam(Q3,lam(Q2,lam(V,lam(Q1,lam(F,app(app(V,Q1),lam(E,app(Q3,lam(Z,merge(B1:drs([],[B1:Index:rel(E,Z,Sym,0)]),
                                                                                 app(Q2,lam(Y,merge(B2:drs([],[B2:[]:rel(E,Y,rel,0)]),
                                                                                                    app(F,E)))))))))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/np,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/np,
               (((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)))/np,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\np]), !,
   Sem = lam(Q,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(app(Q,lam(Z,B:drs([],[B:Index:rel(E,Z,Sym,0)]))),
                                                                             app(F,E))))))))).

semlex(Cat,_Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/n,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/n,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\n,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/pp]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(merge(B:drs([B:[]:Z],[B:Index:role(E,Z,Role,1)]),
                                                                                   app(N,Z)),
                                                                             app(F,E))))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/s:dcl,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/s:dcl,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\s:dcl]), !,
   Sem = lam(S,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(app(S,lam(Z,B:drs([],[B:Index:rel(E,Z,Sym,0)]))),
                                                                             app(F,E))))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(s:pss\np),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(s:adj\np)]), !,
   Sem = lam(VP1,lam(AV,lam(VP2,lam(NP,lam(F,app(app(app(AV,VP2),NP),lam(E,merge(app(app(VP1,lam(P,merge(B1:drs([B1:[]:Z],[B1:[]:pred(Z,thing,n,12)]),
                                                                                                         app(P,Z)))),lam(Z,B2:drs([],[B2:Index:rel(E,Z,Sym,0)]))),
                                                                                 app(F,E))))))))).

/* -------------------------------------------------------------------------
   Discourse connectors: VP if S
------------------------------------------------------------------------- */

semlex(Cat,if,Index,Att-[sem:'IMP'|Att],Sem):-
   option('--semantics',amr),
   member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
   Sem = lam(S,lam(VP,lam(Q,lam(F,app(app(VP,Q),lam(Y,merge(app(F,Y),app(S,lam(E,B2:drs([],[B2:Index:rel(Y,E,condition,1)])))))))))).

semlex(Cat,if,Index,Att-[sem:'IMP'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
   plosing(CC),
   Sem = lam(S,lam(VP,lam(Q,lam(F,B:drs([],[B:Index:imp(app(S,CC),app(app(VP,Q),F))]))))).


/* -------------------------------------------------------------------------
   Discourse connectors: where
------------------------------------------------------------------------- */

semlex(Cat,where,Index,Att1-[symbol:'location',sem:'SUB'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(S,lam(V,lam(Q,lam(F,merge(merge(B1:drs([B1:[]:T],[B1:Index:pred(T,location,n,1)]),
                                                app(app(V,Q),lam(E,merge(B2:drs([],[B2:[]:role(E,T,Role,1)]),app(F,E))))),
                                       app(S,lam(E2,B3:drs([],[B3:[]:role(E2,T,Role,1)])))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: when
------------------------------------------------------------------------- */

semlex(Cat,when,Index,Att-[sem:'SUB'|Att],Sem):-
   option('--tense',true),
   option('--theory',drt),
   member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
   Sem = lam(S,lam(V,lam(Q,lam(F,merge(merge(B1:drs([B1:[]:T],[B1:Index:pred(T,time,n,1)]),
                                             app(S,lam(E,B2:drs([],[B2:[]:rel(E,T,temp_included,1)])))),
                                       app(app(V,Q),lam(E,merge(B3:drs([],[B3:[]:rel(E,T,temp_included,1)]),app(F,E))))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: as does NP
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   option('--theory',drt),
   Cat = ((s:X\np)\(s:X\np))/s:inv, !,
   att(Att,sense,Sense),
   Sem = lam(S,lam(V,lam(Q,lam(F,merge(app(app(V,Q),lam(E,app(F,E))),
                                       app(S,lam(E,B:drs([],[B:Index:pred(E,Sym,r,Sense)])))))))).

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   option('--theory',sdrt),
   Cat = ((s:X\np)\(s:X\np))/s:inv, !,
   plosing(CC),
   Sem = lam(S,lam(V,lam(Q,lam(F,sdrs([sub(lab(K1,B1),lab(K2,B2))],[Index:rel(K1,K2,Sym)]))))),
   B1 = app(app(V,Q),lam(E,app(F,E))),
   B2 = app(S,CC).


/* -------------------------------------------------------------------------
   Discourse connectors (VP modifying)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   option('--theory',sdrt),
   member(Cat,[((s:X\np)\(s:X\np))/s:_,
               ((s:X\np)/(s:X\np))/s:_]), !,
   plosing(CC),
   Sem = lam(S,lam(V,lam(Q,lam(F,sdrs([sub(lab(K1,B1),lab(K2,B2))],[Index:rel(K1,K2,Sym)]))))),
   B1 = app(app(V,Q),F),
   B2 = app(S,CC).

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/s,
               ((s:X\np)/(s:X\np))/s,
               ((s:X\np)\(s:X\np))/s:_,
               ((s:X\np)/(s:X\np))/s:_]), !,
   plosing(CC),
   Sem = lam(S,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Z],
                                                                [B:Index:rel(E,Z,Sym,0),
                                                                 B:[]:prop(Z,app(S,CC))]),
                                                          app(F,E)))))))).

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\np)/s:dcl ]), !,
   plosing(CC),
   Sem = lam(S,lam(NP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Z],
                                                                       [B:Index:rel(E,Z,Sym,0),
                                                                        B:[]:prop(Z,app(S,CC))]),
                                                                 merge(app(NP,lam(U,B2:drs([],[B2:[]:rel(E,U,rel,0)]))),
                                                                       app(F,E)))))))))).

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   member(Cat,[((s:X\np)/s:_)/(s:X\np)]), !,
   plosing(CC),
   Sem = lam(V,lam(S,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Z],
                                                                [B:Index:rel(E,Z,Sym,0),
                                                                 B:[]:prop(Z,app(S,CC))]),
                                                          app(F,E)))))))).


/* -------------------------------------------------------------------------
   Prepositions:  "VP prep VPing"
------------------------------------------------------------------------- */

semlex(Cat,without,Index,Att-[sem:'NOT'|Att],Sem):-
   \+ option('--semantics',amr),
   member(Cat,[((s:X\np)\(s:X\np))/(s:ng\np),
               ((s:X\np)/(s:X\np))/(s:ng\np)]), !,
   CC=lam(Y,B2:drs([],[B2:Index:rel(E,Y,manner,1)])),
   Sem = lam(VA,lam(VM,lam(Q,lam(F,app(Q,
                                       lam(Z,app(app(VM,lam(P,app(P,Z))),
                                                 lam(E,merge(app(F,E),
                                                             B:drs([],[B:Index:not(app(app(VA,lam(P,app(P,Z))),
                                                                                   CC))])))))))))).

semlex(Cat,if,Index,Att-[sem:'IMP'|Att],Sem):-
   option('--semantics',amr),
   member(Cat,[((s:X\np)\(s:X\np))/(s:adj\np),((s:X\np)/(s:X\np))/(s:adj\np),
               ((s:X\np)\(s:X\np))/(s:pss\np),((s:X\np)/(s:X\np))/(s:pss\np)]), !,
   Sem = lam(VA,lam(VM,lam(Q,lam(F,app(Q,
                                       lam(Z,app(app(VM,lam(P,app(P,Z))),
                                                 lam(E,merge(app(F,E),
                                                             app(app(VA,lam(P,app(P,Z))),
                                                                 lam(G,B:drs([],[B:Index:rel(E,G,condition,1)])))))))))))).

semlex(Cat,if,Index,Att-[sem:'IMP'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/(s:adj\np),((s:X\np)/(s:X\np))/(s:adj\np),
               ((s:X\np)\(s:X\np))/(s:pss\np),((s:X\np)/(s:X\np))/(s:pss\np)]), !,
   plosing(CC),
   Sem = lam(VA,lam(VM,lam(Q,lam(F,app(Q,lam(Z,B:drs([],[B:Index:imp(app(app(VA,lam(P,app(P,Z))),CC),
                                                                     app(app(VM,lam(P,app(P,Z))),F))]))))))).


semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/(s:ng\np),((s:X\np)/(s:X\np))/(s:ng\np),
               ((s:X\np)\(s:X\np))/(s:adj\np),((s:X\np)/(s:X\np))/(s:adj\np),
               ((s:X\np)\(s:X\np))/(s:pss\np),((s:X\np)/(s:X\np))/(s:pss\np)]), !,
   Sem = lam(VA,lam(VM,lam(Q,lam(F,app(Q,
                                       lam(Z,app(app(VM,lam(P,app(P,Z))),
                                                 lam(E,merge(app(F,E),
                                                             app(app(VA,lam(P,app(P,Z))),
                                                                 lam(G,B:drs([],[B:Index:rel(E,G,Sym,0)])))))))))))).

% used by type-changing rule
%
semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/(s:to\np), 
               ((s:X\np)/(s:X\np))/(s:to\np)]), !,
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2), 
   Sem = lam(VP,lam(V,lam(Q,lam(F,app(Q,lam(Z,app(app(V,Q),
                                      lam(E,app(app(VP,lam(P,app(P,Z))),
                                                lam(Y,merge(B2:drs([],[B2:Index:role(E,Y,Role,1)]),app(F,E)))))))))))).

% it is clear who planted the bomb
%
semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   Sym = who,
   member(Cat,[
               ((s:X\np)\(s:X\np))/(s:dcl\np), % which
               ((s:X\np)\(s:X\np))/(s:dcl/np), % who
               ((s:X\np)/(s:X\np))/(s:dcl\np)]), !,
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2), 
   NP = lam(R,merge(B1:drs([B1:[]:W],[B1:Index:pred(W,person,n,1)]),app(R,W))),
   Sem = lam(VP,lam(V,lam(Q,lam(F,app(Q,lam(Z,app(app(V,lam(P,app(P,Z))),
                                      lam(E,app(app(VP,NP),
                                                lam(Y,merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),app(F,E)))))))))))).

% specify which was the 
%
semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   Sym = which,
   member(Cat,[((s:X\np)\(s:X\np))/(s:dcl\np),  
               ((s:X\np)\(s:X\np))/(s:dcl/np),  
               ((s:X\np)/(s:X\np))/(s:dcl\np)]), !,
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2), 
   NP = lam(R,merge(B1:drs([B1:[]:W],[B1:Index:pred(W,thing,n,12)]),app(R,W))),
   Sem = lam(VP,lam(V,lam(Q,lam(F,app(Q,lam(Z,app(app(V,lam(P,app(P,Z))),
                                      lam(E,app(app(VP,NP),
                                                lam(Y,merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),app(F,E)))))))))))).

% all other cases
%
semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/(s:_\np),  
               ((s:X\np)\(s:X\np))/(s:_/np)]), !,
   NP = lam(R,merge(B1:drs([B1:[]:W],[B1:Index:pred(W,thing,n,12)]),app(R,W))),
   Sem = lam(VP,lam(V,lam(Q,lam(F,app(Q,lam(Z,app(app(V,lam(P,app(P,Z))),
                                      lam(E,app(app(VP,NP),
                                                lam(Y,merge(B2:drs([],[B2:[]:rel(E,Y,Sym,0)]),app(F,E)))))))))))).

%
%
semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[
               ((s:adj\np)/(s:to\np))/(s:adj\np), 
               ((s:adj\np)/(s:for\np))/(s:adj\np), 
               ((s:b\np)/(s:ng\np))/(s:adj\np), 
               ((s:b\np)/(s:dcl\np))/(s:adj\np), 
               ((s:pt\np)/(s:ng\np))/(s:adj\np), 
               ((s:dcl\np)/(s:b\np))/(s:adj\np), 
               ((s:dcl\np)/(s:adj\np))/(s:adj\np)]), !,
   Sem = lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),
                                      lam(E,app(app(VP,
                                                    lam(P,merge(B1:drs([B1:[]:Z],[B1:[]:pred(Z,thing,n,12)]),app(P,Z)))),
                                                lam(Y,merge(B2:drs([],[B2:Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).


/* -------------------------------------------------------------------------
   Control Prepositions (NP)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/(s:ng\np))/np,
               (((s:X\np)\(s:X\np))/(s:pt\np))/np,
               (((s:X\np)\(s:X\np))/(s:pss\np))/np,
               (((s:X\np)\(s:X\np))/(s:b\np))/np,
               (((s:X\np)\(s:X\np))/(s:to\np))/np]), !,
   Sem =  lam(NP,lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(app(VP,lam(P,app(NP,lam(Y,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),
                                                                                                 app(P,Y)))))),F)))))))).

/* -------------------------------------------------------------------------
   Control Prepositions (N)
   Example: "in", as in: "I had a plan in place to respond."
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/(s:ng\np))/n,
               (((s:X\np)\(s:X\np))/(s:pt\np))/n,
               (((s:X\np)\(s:X\np))/(s:pss\np))/n,
               (((s:X\np)\(s:X\np))/(s:b\np))/n,
               (((s:X\np)\(s:X\np))/(s:to\np))/n]), !,
   plosing(CC),
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2),
   Sem = lam(N,lam(VP,lam(Y,lam(Q,lam(F,app(Q,lam(U,app(app(Y,lam(P,app(P,U))),lam(E,merge(B:drs([B:[]:Z,B:[]:K],
                                                                                                 [B:Index:rel(E,Z,Sym,0),
								                                  B:[]:prop(K,app(app(VP,lam(P,app(P,U))),CC)),
                                                                                                  B:[]:rel(E,K,Role,0)]),
                                                                 merge(app(N,Z),app(F,E)))))))))))).


semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Cat = (((s:X\np)\(s:X\np))/pp)/np, !,
   Sem =  lam(NP,lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(app(NP,lam(Y,B:drs([],[B:Index:rel(E,Y,Sym,0)]))),
                                                                   merge(app(PP,E),app(F,E)))))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:_\s:_)/pp)/np,
               ((s:_/s:_)/pp)/np]), !,   
   Sem = lam(Q,lam(PP,lam(S,lam(F,app(S,lam(E,merge(app(Q,lam(Y,B:drs([],[B:Index:rel(E,Y,Sym,0)]))),
                                                    merge(app(PP,E),app(F,E))))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(s:wq\s:wq)/np,
               (s:X/s:X)\np,
               (s:X/s:X)/np,
               (s:X\s:X)/np]), !,   
   Sem = lam(Q,lam(S,lam(F,app(S,lam(E,merge(app(Q,lam(Y,B:drs([],[B:Index:rel(E,Y,Sym,0)]))),app(F,E))))))).


/* -------------------------------------------------------------------------
   Sentence-initial determiners
------------------------------------------------------------------------- */

semlex(Cat,Lemma,Index,Att1-[sem:'PRX'|Att2],Sem):-
   option('--semantics',amr),
   member(Lemma,[this,these]), 
   member(Cat,[(s:X/s:X)/n,(s:X\s:X)/n]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(P,lam(S,lam(F,alfa(def,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,this,r,2)]),app(P,Y)),
                                    app(S,lam(E,merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),app(F,E)))))))).

semlex(Cat,Lemma,Index,Att1-[sem:'DST'|Att2],Sem):-
   option('--semantics',amr),
   member(Lemma,[that,those]), 
   member(Cat,[(s:X/s:X)/n,(s:X\s:X)/n]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(P,lam(S,lam(F,alfa(def,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,that,r,2)]),app(P,Y)),
                                    app(S,lam(E,merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),app(F,E)))))))).

semlex(Cat,Lemma,Index,Att1-[sem:'PRX'|Att2],Sem):-
   member(Lemma,[this,these]), 
   member(Cat,[(s:X/s:X)/n,(s:X\s:X)/n]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(P,lam(S,lam(F,alfa(def,merge(B1:drs([B1:Index:Y],[]),app(P,Y)),
                                    app(S,lam(E,merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),app(F,E)))))))).

semlex(Cat,Lemma,Index,Att1-[sem:'DST'|Att2],Sem):-
   member(Lemma,[the,that,those]), 
   member(Cat,[(s:X/s:X)/n,(s:X\s:X)/n]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(P,lam(S,lam(F,alfa(def,merge(B1:drs([B1:Index:Y],[]),app(P,Y)),
                                    app(S,lam(E,merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),app(F,E)))))))).

semlex(Cat,Lemma,Index,Att1-[sem:'DEF'|Att2],Sem):-
   member(Lemma,[the]), 
   member(Cat,[(s:X/s:X)/n,(s:X\s:X)/n]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(P,lam(S,lam(F,alfa(def,merge(B1:drs([B1:Index:Y],[]),app(P,Y)),
                                    app(S,lam(E,merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),app(F,E)))))))).

semlex(Cat,Lemma,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Lemma,[all,every,each,any]), 
   member(Cat,[(s:X/s:X)/n,(s:X\s:X)/n]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(P,lam(S,lam(F,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:Y],[]),app(P,Y)),
                                          app(S,lam(E,merge(B3:drs([],[B3:[]:role(E,Y,Role,1)]),app(F,E)))))])))).

semlex(Cat,_Sym,Index,Att1-[sem:'DIS'|Att2],Sem):-
   member(Cat,[(s:X/s:X)/n,
               (s:X\s:X)/n]), !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(P,lam(S,lam(F,merge(merge(B1:drs([B1:Index:Y],[]),app(P,Y)),
                                 app(S,lam(E,merge(B2:drs([],[B2:[]:rel(E,Y,Relation,0)]),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Example: With violence escalating in Kosovo, S
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X/s:X)/(s:ng\np))/np,
               ((s:X/s:X)/(s:pt\np))/np,
               ((s:X/s:X)/(s:b\np))/np,
               ((s:X/s:X)/(s:adj\np))/np]), !,   
   closing(CC),
   Sem = lam(Q,lam(VP,lam(S,lam(F,app(S,lam(E,app(app(VP,lam(P,app(Q,lam(Y,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),merge(app(P,Y),app(F,E))))))),CC))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X/s:X)\np)/np]), !,
   Sem = lam(Q1,lam(Q2,lam(S,lam(F,app(S,lam(E,app(Q2,lam(Y,app(Q1,lam(Z,merge(B:drs([],[B:Index:rel(Y,Z,Sym,0)]),app(F,E)))))))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X/s:X)\np)/s:dcl]), !,
   Sem = lam(S1,lam(Q2,lam(S,lam(F,app(S,lam(E,app(Q2,lam(Y,app(S1,lam(E,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),app(F,E)))))))))))).

% Where on the body ...
semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Cat = ((s:wq/(s:q/pp))\(s:wq/(s:q/pp)))/np,
   Sem = lam(NP,lam(Q,lam(VP,lam(F,app(app(Q,VP),lam(E,merge(app(NP,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))),
                                                             app(F,E)))))))). 


/* -------------------------------------------------------------------------
   instead (of)
------------------------------------------------------------------------- */

semlex(Cat,instead,Index,Att-[sem:'NOT'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,B:drs([],[B:Index:not(merge(app(PP,E),app(F,E)))]))))))). 


/* -------------------------------------------------------------------------
   VP ... according to ...
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   att(Att1,pos,Pos),
   member(Pos,['VBG','VBN']),
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2),
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,v,0),
                                                                           B:[]:role(Y,E,Role,1)]),
                                                           merge(app(PP,Y),app(F,E))))))))). 


/* -------------------------------------------------------------------------
   Double prepositions, such as "out of", "together with"
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,0)]),merge(app(PP,E),app(F,E))))))))). 


/* -------------------------------------------------------------------------
   Double prepositions, 
     such as "According to NP, ..." (VBG)
         and "Based on reports, ..." (VBN)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   att(Att1,pos,Pos),
   member(Pos,['VBG','VBN']),
   member(Cat,[(s:X/s:X)/pp,(s:X\s:X)/pp]), !, 
   plosing(CC),
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2),
   Sem = lam(PP,lam(S,lam(F,merge(B:drs([B:[]:E,B:[]:P],[B:Index:pred(E,Sym,v,0),
                                                         B:[]:role(E,P,Role,1), 
                                                         B:[]:prop(P,app(S,CC))]),
                                  merge(app(PP,E),app(F,E)))))).

semlex(Cat,Sym,Index,Att1-[sem:'NOT'|Att2],Sem):-
   Sym = nowhere,
   member(Cat,[(s:X/s:X)/pp,
               (s:X\s:X)/pp]), !, 
   role(['Location'],Att1-Att2,[Role]),
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,B:drs([],[B:Index:not(merge(B:drs([B:Index:Y],[B:[]:pred(Y,location,n,1),
                                                                                       B:[]:role(E,Y,Role,1)]),
                                                                merge(app(PP,Y),app(F,E))))])))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(s:X/s:X)/pp,
               (s:X\s:X)/pp]), !, 
   att(Att,sense,Sense),
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),
                                              merge(app(PP,E),
                                                    app(F,E)))))))). 


/* -------------------------------------------------------------------------
   VP adverb modifier (negation)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   notSymbol(Sym),
   option('--semantics',drg),
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,B1:drs([],[B1:[]:not(app(app(app(AV,VP),NP),lam(E,merge(B2:drs([],[B2:Index:pred(E,Sym,s,1)]),app(F,E)))))]))))).


semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   notSymbol(Sym), 
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,B:drs([],[B:Index:not(app(app(app(AV,VP),NP),lam(E,app(F,E))))]))))).


/* -------------------------------------------------------------------------
   VP adverb modifier (Cardinals that function as modifiers)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'QUA'|Att2],Sem):-
   att(Att1,pos,'CD'),
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), 
   string2digit(Sym,Digit), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(B:drs([B:[]:Y],[B:Index:card(Y,Digit,eq),
                                                                                       B:[]:role(E,Y,Role,1)]),
                                                                       app(F,E)))))))).

/* -------------------------------------------------------------------------
   VP adverb modifier (NPs that function as modifiers) `Saturday evening`
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), 
   att(Att1,pos,Pos),
   member(Pos,['NN','NNS','NNP','NNPS']), !,
   att(Att1,sense,Sense),
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,n,Sense),
                                                                                       B:[]:role(E,Y,Role,1)]),
                                                                       app(F,E)))))))).


/* -------------------------------------------------------------------------
   VP adverb modifier (intersective)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:adj\np)\(s:adj\np))/((s:adj\np)\(s:adj\np)),  
               ((s:adj\np)/(s:adj\np))/((s:adj\np)/(s:adj\np)),
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), !,
   att(Att,sense,Sense),
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E)))))))).

% VP adverb modifier (negation)

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   option('--semantics',drg),
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np))),
               (((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))/(((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))]), 
   notSymbol(Sym), !,
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,B1:drs([],[B1:Index:not(app(app(app(app(M,AV),VP),NP),lam(E,merge(B2:drs([],[B2:Index:pred(E,Sym,s,1)]),app(F,E)))))])))))).

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np))),
               (((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))/(((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))]), 
   notSymbol(Sym), !,
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,B:drs([],[B:Index:not(app(app(app(app(M,AV),VP),NP),lam(E,app(F,E))))])))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/((s:X\np)\(s:X\np))]), !,
   att(Att,sense,Sense),
   Sem = lam(AV1,lam(AV2,lam(VP,lam(NP,lam(F,app(app(app(AV2,app(AV1,VP)),NP),
                                             lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E))))))))).

% VP adverb modifier (intersective)
semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))), 
               (((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))/(((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np))),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))]), !,
   att(Att,sense,Sense),
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(app(M,AV),VP),NP),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E))))))))).

/* -------------------------------------------------------------------------
   If VP, then ... S
------------------------------------------------------------------------- */

semlex(Cat,if,Index,Att-[sem:'IMP'|Att],Sem):-
   option('--semantics',amr),
   member(Cat,[(s:X/s:X)/(s:_\np),(s:X\s:X)/(s:_\np)]), !,
   Sem = lam(VP,lam(S,lam(G,app(app(VP,lam(P,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,thing,n,12)]),app(P,Y)))),
                                lam(F,merge(app(G,F),app(S,lam(E,B2:drs([],[B2:[]:rel(E,F,condition,0)]))))))))).

semlex(Cat,if,Index,Att-[sem:'IMP'|Att],Sem):-
   member(Cat,[(s:X/s:X)/(s:_\np),(s:X\s:X)/(s:_\np)]), !,
   plosing(CC),
   Sem = lam(VP,lam(S,lam(F,B:drs([],[B:Index:imp(app(app(VP,lam(P,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,thing,n,12)]),app(P,Y)))),CC),
                                                  app(S,F))])))).

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   member(Cat,[(s:X/s:X)/(s:_\np),(s:X\s:X)/(s:_\np)]), !,
   Sem = lam(VP,lam(S,lam(G,app(app(VP,lam(P,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,thing,n,12)]),app(P,Y)))),
                                lam(F,merge(app(G,F),app(S,lam(E,B2:drs([],[B2:[]:rel(E,F,Sym,0)]))))))))).


/* -------------------------------------------------------------------------
   Preposition (in front of WH, as in "From where ...")
------------------------------------------------------------------------- */

semlex((s:wq/(s:q/pp))/(s:wq/(s:q/np)),Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Sem = lam(Q,lam(W,lam(F,app(app(Q,V),F)))),
   V = lam(N,lam(E,app(N,lam(X,app(app(W,lam(Y,B:drs([],[B:Index:rel(Y,X,Sym,0)]))),E))))).

semlex((s:wq/(s:q/np))/(s:wq/(s:q/np)),_Sym,_Index,Att-[sem:'NIL'|Att],Sem):-
   Sem = lam(X,X).


/* -------------------------------------------------------------------------
   Possessive 
------------------------------------------------------------------------- */

semlex(Cat,_Lemma,Index,Att-[sem:'HAS'|Att],Sem):-
   member(Cat,[(np:nb/n)/(n/n),
               (np/n)/(n/n)]), !,
   Sem = lam(S,lam(P,lam(Q,merge(B:drs([B:[]:U],[]),
                                 merge(app(app(S,lam(X,merge(app(P,X),B1:drs([B1:[]:Y],[B1:Index:rel(X,Y,of,1)])))),U),
                                       app(Q,U)))))).



semlex(Cat,_,Index,Att1-[sem:'HAS'|Att2],Sem):- 
   member(Cat,[(np/n)\np, 
               (np:nb/n)\np]), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(NP,lam(N,lam(P,app(NP,lam(Y,alfa(def,merge(B:drs([B:[]:X],[B:Index:rel(X,Y,Relation,1)]),
                                                        app(N,X)),
                                                  app(P,X))))))).

semlex(Cat,_,Index,Att-[sem:'HAS'|Att],Sem):- 
   member(Cat,[(np/(n/pp))\np, 
               (np:nb/(n/pp))\np]), !,
   Sem = lam(NP,lam(RN,lam(P,app(NP,lam(Y,alfa(def,merge(B:drs([B:[]:X],[]),
                                                         app(app(RN,lam(Z,B2:drs([],[B2:Index:rel(Z,Y,of,1)]))),X)),
                                                   app(P,X))))))).


semlex(Cat,_,Index,Att-[sem:'HAS'|Att],Sem):- 
   member(Cat,[((np:nb/n)/(n/n))\np,
               ((np/n)/(n/n))\np]), !,
   Sem = lam(N,lam(S,lam(P,lam(Q,merge(B1:drs([B1:[]:U],[]),
                                       merge(app(app(S,lam(X,merge(app(P,X),
                                                                   app(N,lam(Y,B2:drs([],[B2:Index:rel(X,Y,of,1)])))))),U),
                                             app(Q,U))))))).

semlex((n/n)\n,_,Index,Att-[sem:'HAS'|Att],Sem):- !,
   Sem = lam(N1,lam(N2,lam(X,merge(B1:drs([B1:[]:Y],[]),
                                   merge(app(N1,Y),
                                         merge(app(N2,X),
                                               B2:drs([],[B2:Index:rel(X,Y,of,1)]))))))).
semlex(Cat,_,Index,Att-[sem:'HAS'|Att],Sem):- 
   member(Cat,[((s:wq/(s:q/np))/n)\(s:wq/(s:q/np)),
               ((s:wq\(s:dcl/np))/n)\(s:wq\(s:dcl/np)),
               ((s:wq/(s:dcl\np))/n)\(s:wq/(s:dcl\np))]), !,
   XXX = lam(U,lam(E,app(U,lam(Y,alfa(def,merge(B:drs([B:[]:X],[B:Index:rel(X,Y,of,1)]),
                                          app(N,X)),
                                    app(app(V,lam(Q,app(Q,X))),E)))))),                      
   Sem = lam(NP,lam(N,lam(V,lam(P,app(app(NP,XXX),P))))).

%  Sem = lam(NP,lam(N,lam(V,app(V,lam(P2,app(NP,lam(V2,app(V2,lam(Y,alfa(def,merge(B:drs([B:Index:X],[B:Index:rel(X,Y,of,1)]),app(N,X)),app(P2,X))))))))))).


/* -------------------------------------------------------------------------
   Emphasising Pronouns
------------------------------------------------------------------------- */

semlex(np\np, himself,Index,Att-[sem:'EMP'|Att],Sem):- 
   option('--semantics',amr), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,he,n,1)]),app(P,X)))))).

semlex(np\np, himself,Index,Att-[sem:'EMP'|Att],Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,male,n,2)]),app(P,X)))))).

semlex(np\np, herself,Index,Att-[sem:'EMP'|Att],Sem):-
   option('--semantics',amr), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,she,n,1)]),app(P,X)))))).

semlex(np\np, herself,Index,Att-[sem:'EMP'|Att],Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,female,n,2)]),app(P,X)))))).

semlex(np\np, itself,Index,Att-[sem:'EMP'|Att],Sem):-
   option('--semantics',amr), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,it,n,1)]),app(P,X)))))).

semlex(np\np, itself,Index,Att-[sem:'EMP'|Att],Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,thing,n,12)]),app(P,X)))))).

semlex(np\np, Sym,Index,Att-[sem:'EMP'|Att],Sem):-
   option('--semantics',amr), 
   member(Sym,[myself,yourself,thyself,ourselves,themselves]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,we,n,1)]),app(P,X)))))).

semlex(np\np, Sym,Index,Att-[sem:'EMP'|Att],Sem):-
   member(Sym,[myself,yourself,thyself,ourselves,themselves]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,person,n,1)]),app(P,X)))))).


/* -------------------------------------------------------------------------
   NP modifiers: floating quantifiers 
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   option('--semantics',amr),
   member(Cat,[np\np, np/np]), 
   member(Sym,[all,each]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,r,2)]),app(P,X)))))).

semlex(Cat,Sym,Index,Att-[sem:'EXC'|Att],Sem):-
   option('--semantics',amr),
   member(Cat,[np\np, np/np]), 
   member(Sym,[only]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,r,2)]),app(P,X)))))).


semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[np\np, np/np]), 
   member(Sym,[all,each]), !,
   Sem = lam(Q,lam(P,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:X],[]),app(Q,lam(Y,B3:drs([],[B3:[]:eq(X,Y)])))),app(P,X))]))).


/* -------------------------------------------------------------------------
   NP modifiers: only
------------------------------------------------------------------------- */

semlex(Cat,only,Index,Att-[sem:'EXC'|Att],Sem):-
   \+ option('--semantics',amr),
   member(Cat,[np\np, np/np]), !,
   Sem = lam(NP,lam(P,alfa(fac,merge(B1:drs([B1:Index:Z],[]),
                                     app(NP,lam(X,merge(app(P,X),
                                                        B2:drs([],[B2:[]:eq(Z,X)]))))),
                               B3:drs([],[B3:[]:imp(merge(B4:drs([B4:[]:Y],[]),app(P,Y)),
                                                          B5:drs([],[B5:[]:eq(Z,Y)]))])))).


/* -------------------------------------------------------------------------
   NP modifiers: negation
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   member(Cat,[np\np, np/np]), 
   notSymbol(Sym), !,
   Sem = lam(NP,lam(P,B:drs([],[B:Index:not(app(NP,P))]))).

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   Cat = (((np\np)/(np\np))/((np\np)/(np\np))),
   notSymbol(Sym), !,
   Sem = lam(A1,lam(A2,lam(NP,lam(P,B:drs([],[B:Index:not(app(app(app(A1,A2),NP),P))]))))).


/* -------------------------------------------------------------------------
   NP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-['TIM'|Att2],Sem):-
   att(Att1,pos,Pos),
   member(Pos,['NNP','NNPS']),
   att(Att1,namex,NE), neClassType(NE,tim,Type),
   member(Cat,[np\np, np/np]), !,
   rel(on,Att1-Att2,Relation),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:named(Y,Sym,tim,Type),B:[]:rel(X,Y,Relation,0)]),
                                       app(P,X)))))).

semlex(Cat,Sym,Index,Att-[sem:Tag|Att],Sem):-
   att(Att,pos,Pos),
   member(Pos,['NNP','NNPS']),
   member(Cat,[np\np, np/np]), !,
   att(Att,namex,Ne), neClassType(Ne,Class,Type,Tag),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                                       app(P,X)))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   att(Att,pos,Pos),
   member(Pos,['IN','RB','JJ','JJR','RBR']),
   member(Cat,[np\np, np/np]), !,
   att(Att,sense,Sense),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,r,Sense)]),
                                       app(P,X)))))).


semlex(Cat,Sym,Index,Att1-[sem:'SCO'|Att2],Sem):-
   member(Cat,[np\np, np/np]),
   att(Att1,pos,'CD'),
   string2score(Sym,Score), !,
   rel(with,Att1-Att2,Relation),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:named(Y,Score,sco,num),
                                                       B:[]:rel(X,Y,Relation,0)]),
                                       app(P,X)))))).

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Cat,[np\np, np/np]), !,
   att(Att1,sense,Sense),
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,n,Sense),
                                                       B:[]:role(X,Y,Role,1)]),
                                       app(P,X)))))).


/* -------------------------------------------------------------------------
   NP modifiers (superlative contruction)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'TOP'|Att],Sem):-
   member(Cat,[d/np]), !,
   Sem = lam(X,lam(Y,B:drs([],[B:Index:rel(Y,X,Sym,0)]))).


/* -------------------------------------------------------------------------
   NP modifier modifiers: deitics

semlex(Cat,Sym,_Index,Att-[sem:'UNK'|Att],Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), 
   member(Sym,[there,here,ago,such,now]), !,
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),P)))). 
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   NP modifier modifiers (proper names)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:Tag|Att],Sem):-
   att(Att,pos,Pos),
   member(Pos,['NNP','NNPS']),
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
   att(Att,namex,Ne), neClassType(Ne,Class,Type,Tag),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                                                    app(P,X))))))).

/* -------------------------------------------------------------------------
   NP modifier modifiers (not)
------------------------------------------------------------------------- */

semlex(Cat,not,Index,Att-[sem:'NOT'|Att],Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
%   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,B:drs([],[B:Index:not(app(P,X))])))))).
   Sem = lam(M,lam(Q,lam(P,B:drs([],[B:Index:not(app(app(M,Q),lam(X,app(P,X))))])))).


/* -------------------------------------------------------------------------
   NP modifier modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
   att(Att,sense,Sense),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                                    app(P,X))))))).



/* -------------------------------------------------------------------------
   NP modifier modifiers, superlative ("most notably")
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-[sem:'TOP'|Att],Sem):-
   member(Cat,[(np/np)/(d/np)]), !, 
   Sem = lam(R,lam(Q,lam(P,app(Q,lam(X,merge(B1:drs([],[B1:Index:imp(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),
                                                               app(app(R,X),Y))]),
                                             app(P,X))))))).


/* -------------------------------------------------------------------------
   NPs that function as S modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:Tag|Att2],Sem):-
   category(smod,Cat,Sym), 
   att(Att1,pos,Pos),
   member(Pos,['NNP','NNPS']), !,
   att(Att1,namex,Ne), neClassType(Ne,Class,Type,Tag),
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([B:[]:X],[B:Index:named(X,Sym,Class,Type),
                                                       B:[]:role(E,X,Role,1)]),app(F,E)))))).

semlex(Cat,Sym,Index,Att1-[sem:'TIM'|Att2],Sem):-
   category(smod,Cat,Sym), 
   att(Att1,pos,Pos),
   member(Pos,['NN','NNS']), !,
   att(Att1,sense,Sense),
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense),
                                                       B:[]:role(E,X,Role,1)]),app(F,E)))))).

semlex(Cat,Sym,Index,Att1-[sem:'NOT'|Att2],Sem):-
   category(smod,Cat,Sym), 
   member(Sym,[nowhere]), !,
   role(['Location'],Att1-Att2,[Role]),
   Sem = lam(S,lam(F,app(S,lam(E,B1:drs([],[B1:Index:not(merge(B2:drs([B2:Index:X],[B2:[]:pred(X,location,n,1),
                                                                                    B2:[]:role(E,X,Role,1)]),app(F,E)))]))))).

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   category(smod,Cat,Sym), 
   member(Sym,[everywhere,anywhere]), !,
   role(['Location'],Att1-Att2,[Role]),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,location,n,1),
                                                       B:[]:role(E,X,Role,1)]),app(F,E)))))).

semlex(Cat,Sym,Index,Att1-[sem:'DIS'|Att2],Sem):-
   category(smod,Cat,Sym), 
   member(Sym,[somewhere]), !,
   role(['Location'],Att1-Att2,[Role]),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,location,n,1),
                                                       B:[]:role(E,X,Role,1)]),app(F,E)))))).


/* -------------------------------------------------------------------------
   S modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   notSymbol(Sym),
   category(smod,Cat,Sym), !,
   Sem = lam(S,lam(F,B:drs([],[B:Index:not(app(S,F))]))).

semlex(Cat,Sym,Index,Att-[sem:'POS'|Att],Sem):-
   member(Sym,[perhaps,maybe,possibly]),
   option('--modal',true),
   category(smod,Cat,Sym), !,
   Sem = lam(S,lam(F,B:drs([],[B:Index:pos(app(S,F))]))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   category(smod,Cat,Sym), !,
   att(Att,sense,Sense),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E)))))).


/* -------------------------------------------------------------------------
   S modifier modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):- 
   notSymbol(Sym), 
   member(Cat,[(s:X/s:X)/(s:X/s:X),
               (s:X/s:X)\(s:X/s:X),
               (s:X\s:X)/(s:X\s:X),
               (s:X\s:X)\(s:X\s:X)]), !, 
   Sem = lam(M,lam(S,lam(F,B:drs([],[B:Index:not(app(app(M,S),F))])))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):- 
   member(Cat,[(s:X/s:X)/(s:X/s:X),
               (s:X/s:X)\(s:X/s:X),
               (s:X\s:X)/(s:X\s:X),
               (s:X\s:X)\(s:X\s:X)]), !, 
   att(Att,sense,Sense),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),
                                                    app(P,E))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):- 
   member(Cat,[((s:X/s:X)/(s:X/s:X))/np,
               ((s:X/s:X)\(s:X/s:X))/np,
               ((s:X\s:X)/(s:X\s:X))/np,
               ((s:X\s:X)\(s:X\s:X))/np]), !, 
   Sem = lam(Q,lam(M,lam(S,lam(F,app(app(M,S),lam(E,merge(app(Q,lam(Y,B:drs([],[B:Index:rel(E,Y,Sym,0)]))),app(F,E)))))))).

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):- 
   member(Cat,[((s:X/s:X)/(s:X/s:X))/s:dcl,
               ((s:X/s:X)\(s:X/s:X))/s:dcl,
               ((s:X\s:X)/(s:X\s:X))/s:dcl,
               ((s:X\s:X)\(s:X\s:X))/s:dcl]), !, 
   plosing(CC),
   Sem = lam(S1,lam(M,lam(S2,lam(F,merge(B1:drs([B1:[]:E,B1:[]:Z,B1:[]:Y],
                                                [B1:[]:prop(E,B2:drs([],[B2:Index:rel(Z,Y,Sym,0)])),
                                                 B1:[]:prop(Z,app(S1,CC)),
                                                 B1:[]:prop(Y,app(app(M,S2),CC))]),
                                   app(F,E)))))).


/* -------------------------------------------------------------------------
   Mostly Temporal modifiers: "every month", "this week", "Nov. 29"
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'UOM'|Att],Sem):-
   att(Att,pos,'$'), !,
   member(Cat,[((s:X\np)\(s:X\np))/n,((s:X\np)/(s:X\np))/n]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(B1:drs([B1:[]:Y],
                                                                          [B1:[]:pred(Y,Sym,n,1)]),
                                                                   app(N,Y)),
                                                             merge(B2:drs([],[B2:Index:rel(E,Y,rel,0)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-[sem:'PRX'|Att2],Sem):-
   option('--semantics',amr),
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[this,these]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,this,r,2)]),
                                                                   app(N,Y)),
                                                             merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-[sem:'DST'|Att2],Sem):-
   option('--semantics',amr),
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[that,those]), !,
   role(['Time'],Att1-Att2,Role),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,that,r,2)]),
                                                                   app(N,Y)),
                                                             merge(B2:drs([],[B2:[]:role(E,Y,Role,1)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-[sem:'PRX'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[this,these]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(B1:drs([B1:[]:Y],[]),
                                                                   app(N,Y)),
                                                             merge(B2:drs([],[B2:Index:role(E,Y,Role,1)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-[sem:'DST'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[that,those]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(B1:drs([B1:[]:Y],[]),
                                                                   app(N,Y)),
                                                             merge(B2:drs([],[B2:Index:role(E,Y,Role,1)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-[sem:'DEF'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[the,that,those]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(B1:drs([B1:[]:Y],[]),
                                                                   app(N,Y)),
                                                             merge(B2:drs([],[B2:Index:role(E,Y,Role,1)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[every,each,all,any,either]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(V,lam(Q,lam(F,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:Y],[]),app(N,Y)),
                                                app(app(V,Q),lam(E,merge(B3:drs([],[B3:[]:role(E,Y,Role,1)]),
                                                                         app(F,E)))))]))))).

semlex(Cat,Sym,Index,Att1-[sem:'DIS'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[a,an,some]), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:Index:Y],[B:[]:role(E,Y,Role,1)]),
                                                          merge(app(N,Y),app(F,E))))))))).

semlex(Cat,no,Index,Att1-[sem:'NOT'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,B1:drs([],[B1:Index:not(merge(B2:drs([B2:Index:Y],[B2:[]:rel(E,Y,Relation,0)]),
                                                                            merge(app(N,Y),app(F,E))))]))))))).

semlex(Cat,Sym,Index,Att1-[sem:'MOY'|Att2],Sem):-
   att(Att1,pos,'NNP'),
   att(Att1,namex,NE), neClass(NE,tim),
   member(Cat,[((s:X\np)\(s:X\np))/n, ((s:X\np)/(s:X\np))/n]), 
   month(Sym,MID), !,
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(P,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B1:drs([B1:[]:Y],[B1:Index:timex(Y,date([]:'+',[]:'XXXX',Index:MID,[]:'XX')),
                                                                            B1:[]:role(E,Y,Role,1)]),
                                                          merge(app(P,Y),app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), !,
   att(Att1,sense,Sense),
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(P,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,n,Sense),
                                                                          B:[]:role(E,Y,Role,1)]),
                                                          merge(app(P,Y),app(F,E))))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: when
------------------------------------------------------------------------- */

semlex(Cat,when,Index,Att-[sem:'SUB'|Att],Sem):-
   option('--tense',true),
   option('--theory',drt),
   member(Cat,[(s:X/s:X)/s:dcl,
               (s:wq/s:wq)/s:dcl]), !, 
   Sem = lam(S1,lam(S2,lam(F,merge(merge(B1:drs([B1:[]:T],[B1:[]:pred(T,time,n,1)]),
                                         app(S1,lam(E,B2:drs([],[B2:Index:rel(E,T,temp_included,1)])))),
                                   app(S2,lam(E,merge(B3:drs([],[B3:[]:rel(E,T,temp_included,1)]),app(F,E)))))))).

semlex(Cat,when,Index,Att-[sem:'SUB'|Att],Sem):-
   option('--tense',true),
   option('--theory',drt),
   member(Cat,[(s:X\s:X)/s:dcl,
               (s:wq\s:wq)/s:dcl]), !, 
   Sem = lam(S2,lam(S1,lam(F,merge(merge(B1:drs([B1:[]:T],[[]:pred(T,time,n,1)]),
                                         app(S1,lam(E,B2:drs([],[B2:Index:rel(E,T,temp_included,1)])))),
                                   app(S2,lam(E,merge(B3:drs([],[B3:[]:rel(E,T,temp_included,1)]),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: if S1 then S2; S2 if S1
------------------------------------------------------------------------- */

semlex(Cat,if,Index,Att-[sem:'IMP'|Att],Sem):-
   option('--semantics',amr),
   member(Cat,[(s:X/s:X)/s:dcl,(s:wq/s:wq)/s:dcl,
               (s:X\s:X)/s:dcl,(s:wq\s:wq)/s:dcl]), !, 
   Sem = lam(S1,lam(S2,lam(F,app(S2,lam(Y,merge(app(F,Y),app(S1,lam(E,B2:drs([],[B2:Index:rel(Y,E,condition,1)]))))))))).

semlex(Cat,if,Index,Att-[sem:'IMP'|Att],Sem):-
   member(Cat,[(s:X/s:X)/s:dcl,(s:wq/s:wq)/s:dcl,
               (s:X\s:X)/s:dcl,(s:wq\s:wq)/s:dcl]), !, 
   plosing(CC),
   Sem = lam(S1,lam(S2,lam(F,B:drs([],[B:Index:imp(app(S1,CC),app(S2,F))])))).


/* -------------------------------------------------------------------------
   Discourse connectors: if
------------------------------------------------------------------------- */

semlex((s:X\s:X)/s:dcl,and,_Index,Att-[sem:'IMP'|Att],Sem):-
   option('--theory',drt), !,
   plosing(CC),
   Sem = lam(S2,lam(S1,lam(F,merge(app(S1,CC),app(S2,F))))).


/* -------------------------------------------------------------------------
   Discourse connectors: all others
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   option('--theory',sdrt),
   Cat = (s:X/s:X)/s:_, !,
   plosing(CC),
%  Sem = lam(S1,lam(S2,lam(F,sdrs([lab(K1,B1),lab(K2,B2)],[Index:rel(K1,K2,Sym)])))),
   Sem = lam(S2,lam(S1,lam(F,sdrs([sub(lab(K1,B1),lab(K2,B2))],[Index:rel(K1,K2,Sym)])))),
   B1 = app(S1,CC),
   B2 = app(S2,F).

semlex(Cat,Sym,Index,Att-[sem:'COO'|Att],Sem):-
   option('--theory',sdrt),
   Cat = (s:X\s:X)/s:_, !,
   plosing(CC),
   Sem = lam(S2,lam(S1,lam(F,sdrs([lab(K1,B1),lab(K2,B2)],[Index:rel(K1,K2,Sym)])))),
   B1 = app(S1,CC),
   B2 = app(S2,F).

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   member(Cat,[(s:X/s:X)/s:dcl,
               (s:X/s:X)/s:inv,
               (s:wq/s:wq)/s:dcl]), !, 
   plosing(CC),
   Sem = lam(S1,lam(S2,lam(F,merge(B1:drs([B1:[]:E,B1:[]:Z,B1:[]:Y],
                                          [B1:[]:prop(E,B2:drs([],[B2:Index:rel(Z,Y,Sym,0)])),
                                           B1:[]:prop(Z,app(S1,CC)),
                                           B1:[]:prop(Y,app(S2,CC))]),
                                   app(F,E))))).

semlex(Cat,Sym,Index,Att-[sem:'SUB'|Att],Sem):-
   member(Cat,[(s:X\s:X)/s:dcl,
               (s:wq\s:wq)/s:dcl]), !, 
   plosing(CC),
   Sem = lam(S2,lam(S1,lam(F,merge(B1:drs([B1:[]:E,B1:[]:Z,B1:[]:Y],
                                          [B1:[]:prop(E,B2:drs([],[B2:Index:rel(Z,Y,Sym,0)])),
                                           B1:[]:prop(Z,app(S1,CC)),
                                           B1:[]:prop(Y,app(S2,CC))]),
                                   app(F,E))))).


/* -------------------------------------------------------------------------
   Non-Restrictive Relative Pronous
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Att1-[sem:'AND'|Att2],Sem):-
   Cat = (np\np)/((s:to\np)/np), !,
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2),
   NP = lam(R,merge(B:drs([B:[]:Y],[B:[]:pred(Y,thing,n,12)]),app(R,Y))),
   Sem = lam(TV,lam(Q,lam(P,app(Q,lam(X,merge(app(app(app(TV,NP),
                                                      lam(P,app(P,X))),
                                                  lam(Z,B2:drs([],[B2:[]:role(X,Z,Role,1)]))),
                                              app(P,X))))))).

semlex(Cat,Sym,_,Att1-[sem:'AND'|Att2],Sem):-
   Cat = (np\np)/(s:to\np), !,
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2),
   Sem = lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,lam(P,app(P,X))),
                                                  lam(Y,B:drs([],[B:[]:role(X,Y,Role,1)]))),
                                              app(P,X))))))).

semlex(Cat,_Sym,_,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(np\np)/(s:_\np),(np\np)/(s:_/np)]),
   option('--elimeq',true), !,
   closing(CC),
   Sem = lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,
                                                      lam(P,app(P,X))),
                                                  CC),
                                              app(P,X))))))).

semlex(Cat,_Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(np\np)/(s:_\np), (np\np)/(s:_/np)]),
   option('--elimeq',false), !,
   closing(CC),
   Sem = lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,
                                                      lam(P,merge(B:drs([B:Index:Y],[B:[]:eq(X,Y)]),app(P,Y)))),
                                                  CC),
                                              app(P,X))))))).



/* -------------------------------------------------------------------------
   Other kind of relative pronous (pied piping)
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Cat=((np\np)/(s:dcl\np))\(np/np), !,
   closing(CC),
   Sem = lam(M,lam(VP,lam(NP,lam(P,app(NP,lam(Y,merge(B:drs([B:[]:Z],[B:Index:eq(Y,Z)]),
                                                      merge(app(P,Y),
                                                            app(app(VP,app(M,lam(Q,app(Q,Z)))),CC))))))))).


/* -------------------------------------------------------------------------
   whose
------------------------------------------------------------------------- */

semlex(((np\np)/(s:dcl\np))/n,_,Index,Att1-[sem:'HAS'|Att2],Sem):- !,
   closing(CC),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,lam(P,merge(B:drs([B:[]:Y],[B:Index:rel(Y,X,Relation,1)]),
                                                                           merge(app(N,Y),app(P,Y))))),CC),
                                                    app(P,X)))))))). 


/* -------------------------------------------------------------------------
   Further relative pronouns
------------------------------------------------------------------------- */

semlex((np\np)/s:_,Sym,Index,Att-[sem:'SUB'|Att],Sem):- !,
   plosing(CC),
   Sem = lam(S,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Z],[B:Index:rel(X,Z,Sym,0),
                                                             B:[]:prop(Z,app(S,CC))]),
                                             app(P,X))))))).


semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((np\np)\(np\np))/s:dcl]), !, 
   Sem = lam(S,lam(M,lam(NP,lam(Q,app(app(M,NP),
                                      lam(X,merge(app(S,lam(E,B:drs([],[B:Index:rel(E,X,Sym,0)]))),
                                                  app(Q,X)))))))).


/* -------------------------------------------------------------------------
   Interjections and Sentential Categories
------------------------------------------------------------------------- */

%semlex(Cat,Sym,Index,Att-[sem:'UNK'|Att],Sem):-
%   option('--x',true),
%   member(Sym,[no]),
%   category(s,Cat,intj), !,
%   att(Att,sense,Sense),
%   Sem = lam(E,merge(B1:drs([B1:[]:X],[B1:Index:not(B2:drs([],[B2:Index:pred(X,Sym,n,Sense)]))]),app(E,X))).

semlex(Cat,Sym,Index,Att-[sem:'ACT'|Att],Sem):-
   category(s,Cat,_), !,
   att(Att,sense,Sense),
   Sem = lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense)]),app(E,X))).


/* =========================================================================
   Aux Predicates
========================================================================= */

notSymbol(not).
notSymbol(nor).
