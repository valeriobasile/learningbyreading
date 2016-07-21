
:- module(tuples,[tuples/4,write_tuples/2]).

:- use_module(semlib(drs2tacitus),[label/4]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(knowledge(punctuation),[punctuation/2]).
:- use_module(library(lists),[member/2,append/3,last/2,select/3]).


/* =======================================================================
   Dynamic predicates
======================================================================= */

:- dynamic cond_ctr/1. cond_ctr(0).


/* =======================================================================
   Main: tuples (DRG)
======================================================================= */

tuples(Tags,DRS,N1,Sorted):- 
   label(N1,k,K0,N2),
   initcondcounter,
   tuples(DRS,K0,[]-_,Tags,[]-Tuples,N2-_,[]-_),
   inverse(Tuples,Tuples1),
   main_label(Tuples1,Tuples2),
   surface(Tags,Tuples2,Extended),
%  writet(Extended),
   order(Extended,Extended,Ordered),
%  writet(Ordered),
   sort(Ordered,Sorted),
   unboxer(Sorted,Tags).


/* =======================================================================
   Print tuples (just for testing purposes)
======================================================================= */

writet([X|L]):- write(X), nl, writet(L). 
writet([]):- nl.


/* =======================================================================
   Invert Tuples if needed (e.g. for relative clauses and gerunds)
======================================================================= */

inverse(Tuples0,Tuples5):-
   member(tuple(_, _:equality, ext, X,          _),Tuples0),
   select(tuple(L1, C:Role:1,   ext, X,         I1),Tuples0,Tuples1),    T1=tuple(L1,C:Role:-1,int, X,        I1),
   select(tuple(L2, C:Role:1,   int, Y,         I2),Tuples1,Tuples2),    T2=tuple(L2,C:Role:-1,ext, Y,        I2),
   select(tuple(L3, K,          role, C:Role:1, I3),Tuples2,Tuples3), !, T3=tuple(L3,K,        role,C:Role:-1,I3),
   Tuples4=[T1,T2,T3|Tuples3],
%  warning('inverted role tuple for ~p',[X]),
   inverse(Tuples4,Tuples5).

inverse(Tuples,Tuples).


/* =======================================================================
   Promote Main Tuples out of embedded structures
======================================================================= */

% Remove main labels that have inverted roles
%
main_label(T1,T3):-
   select(tuple(_,K:E,main,K,_),T1,T2),
   member(tuple(_,K,role,C:R:-1,_),T2),
   member(tuple(_,C:R:-1,ext,K:E,_),T2), !,
   main_label(T2,T3).

main_label(T1,T3):-
   select(tuple(J,K:R,main,K1,Word),T1,T2),
   member(tuple(_,CC:B,_,K1,_),T2),
   member(tuple(_,K2,binary,CC:B,_),T2), !,
   main_label([tuple(J,K:R,main,K2,Word)|T2],T3).

main_label(T1,T3):-
   select(tuple(J,K:R,main,K1,Word),T1,T2),
   member(tuple(_,CC:B,_,K1,_),T2),
   member(tuple(_,K2,unary,CC:B,_),T2), !,
   main_label([tuple(J,K:R,main,K2,Word)|T2],T3).

main_label(T,T).


/* =======================================================================
   Order edges for each discourse referent
======================================================================= */

order([],_,[]).

order([T|L1],Refs,Ordered):-
   T = tuple(_,_,_,X,_),
   split(L1,X,WithX,WithoutX),       %%% split tuple wrt X
   position([T|WithX],Refs,Pos),     %%% get positions for X
%  write(X:Pos),nl,
   sort(Pos,Sorted),
   localOrder(Sorted,1,L2,Ordered),  %%% normalise order starting with 1
   order(WithoutX,Refs,L2).


/* =======================================================================
   Determine local position (of relations)
======================================================================= */

position([],_,[]).

position([tuple([],N1,int,N2,W)|L1],Tuples,[tuple([P],N1,int,N2,W)|L2]):-
   member(tuple(_,N1,ext,X,_),Tuples),
   member(tuple([P|_],_,_,X,_),Tuples), !,
   position(L1,Tuples,L2).

position([tuple([],N1,int,N2,W)|L1],Tuples,[tuple([P],N1,int,N2,W)|L2]):-
   member(tuple(_,N1,ext,B,_),Tuples),
   member(tuple(_,B,referent,X,_),Tuples), 
   member(tuple([P|_],_,_,X,_),Tuples), !,
   position(L1,Tuples,L2).

position([tuple([],N1,int,N2,W)|L1],Tuples,[tuple([P],N1,int,N2,W)|L2]):-
   member(tuple(_,N1,ext,B,_),Tuples),
   member(tuple(_,B,dominates,C,_),Tuples),
   member(tuple(_,C,referent,X,_),Tuples), 
   member(tuple([P|_],_,_,X,_),Tuples), !,
   position(L1,Tuples,L2).

position([tuple([],R1,int,N2,W)|L1],Tuples,[tuple([P],R1,int,N2,W)|L2]):-
   member(tuple(_,R1,ext,Y,_),Tuples),
   member(tuple(_,R2,int,Y,_),Tuples),
   member(tuple(_,R2,ext,Z,_),Tuples),
   member(tuple([P|_],_,_,Z,_),Tuples), !,
   position(L1,Tuples,L2).

position([T|L1],Tuples,[T|L2]):-
   position(L1,Tuples,L2).


/* =======================================================================
   Determine local order
======================================================================= */

localOrder([],_,L,L).

localOrder([tuple(A,B,C,D,E)|L],N,L1,[tuple(A,0,B,C,D,E)|L2]):- 
   A = [], !,
   localOrder(L,N,L1,L2).

localOrder([tuple(A,B,C,D,E)|L],N,L1,[tuple(A,N,B,C,D,E)|L2]):-
   M is N + 1,
   localOrder(L,M,L1,L2).


/* =======================================================================
   Split tuples into two sets based on third argument
======================================================================= */

split([],_,[],[]).

split([T|L1],X,[T|L2],L3):-
   T = tuple(_,_,_,X,_), !,
   split(L1,X,L2,L3).

split([T|L1],X,[T|L2],L3):-
   T = tuple(_,_,_,_,X,_), !,
   split(L1,X,L2,L3).

split([T|L1],X,L2,[T|L3]):-
   split(L1,X,L2,L3).


/* =======================================================================
   Add surface tuples (ideally should be eliminated)
======================================================================= */

surface([],T,T).

surface([Index:_|W],T1,T2):-                % if token is
   member(tuple(I,_,_,_,_),T1),             % already part of a tuple
   member(Index,I), !,                      % then 
   surface(W,T1,T2).                        % take next token

surface([Index1:[tok:Word|Tags]|W],T1,[T|T2]):- 
   member(pos:POS,Tags),
   punctuation(POS,left),
   member(tuple([Index2|_],_,_,X,_),T1),
   Index1 is Index2 - 1,
   member(tuple(_,K,referent,X,_),T1), !,
   T = tuple([Index1],K,punctuation,X,[Word]),
   surface(W,T1,T2).

surface([Index1:[tok:Word|Tags]|W],T1,[T|T2]):- 
   member(pos:POS,Tags),
   punctuation(POS,right),
   member(tuple(Indices,_,_,X,_),T1),
   last(Indices,Index2),
   Index1 is Index2 + 1,
   member(tuple(_,K,referent,X,_),T1), !,
   T = tuple([Index1],K,punctuation,X,[Word]),
   surface(W,T1,T2).

surface([Index:[tok:Word|_]|W],T1,[T|T2]):- 
   I is div(Index,1000),
   event(T1,Index,I,K,E,Distance), 
   \+ (event(T1,Index,I,_,_,Smaller), Smaller < Distance), !,
   T = tuple([Index],K,surface,E,[Word]),
%  warning('surface tuple: ~p (~p)',[Word,Index]),
   surface(W,T1,T2).

surface([Index:[tok:Word|_]|W],T1,[T|T2]):- 
   T = tuple([Index],k,error,x,[Word]),
   warning('word not part of tuples: ~p',[Word]),
   surface(W,T1,T2).


/* =======================================================================
   Find an event tuple with distance to Index1 (slow!)
======================================================================= */

event(Tuples,Index1,I,K,E,Distance):-
   member(tuple(_,K,event,Event,_),Tuples),
   member(tuple([Index2|_],Event,instance,E,_),Tuples),
   I is div(Index2,1000),
   Distance is abs(Index1-Index2).

event(Tuples,Index1,I,K,E,Distance):-
   member(tuple(_,K,attribute,Event,_),Tuples),
   member(tuple([Index2|_],Event,arg,E,_),Tuples),
   I is div(Index2,1000),
   Distance is abs(Index1-Index2).

%event(Tuples,Index1,K,E,Distance):-
%   member(tuple(_,K,event,Event,_),Tuples),
%   member(tuple([Index2|_],Event,instance,E,_),Tuples),
%   Distance is abs(Index1-Index2),
%   Distance < 1000.


/* =======================================================================
   Counter for DRS-conditions
======================================================================= */

condcounter(CC):-
   retract(cond_ctr(X)),
   label(X,c,CC,N),
   assert(cond_ctr(N)).

initcondcounter:-
   retract(cond_ctr(_)),
   assert(cond_ctr(0)).


/* =======================================================================
   Converting DRSs into graph tuples

   tuples(+DRS,+CurrentDRSid,+Refs,+Words,?Tuples,?Counter,?Indices)
   
   where: tuple(Index,Node1,Edge,Node2,Words)

======================================================================= */

tuples(sdrs(D,R),K,R1-R3,W,T1-T3,N1-N3,I1-I3):- !, tuples(D,K,R1-R2,W,T1-T2,N1-N2,I1-I2), tuples(R,K,R2-R3,W,T2-T3,N2-N3,I2-I3).
tuples(merge(B1,B2),K,R1-R3,W,T1-T3,N1-N3,I1-I3):- !, tuples(B1,K,R1-R2,W,T1-T2,N1-N2,I1-I2), tuples(B2,K,R2-R3,W,T2-T3,N2-N3,I2-I3).
tuples(alfa(_,B1,B2),K,R1-R3,W,T1-T3,N1-N3,I1-I3):- !, tuples(B1,K,R1-R2,W,T1-T2,N1-N2,I1-I2), tuples(B2,K,R2-R3,W,T2-T3,N2-N3,I2-I3).

%tuples(lab(K0,sdrs([sub(lab(L,B),Sub)|D],R)),K,R1-R3,W,T1-[T|T3],N1-N3,I1-I3):- !, 
%   T = tuple([],K0,dominates,L,[]),
%   tuples(sub(lab(L,B),Sub),K,R1-R2,W,T1-T2,N1-N2,I1-I2), 
%   tuples(lab(K0,sdrs(D,R)),K,R2-R3,W,T2-T3,N2-N3,I2-I3).

%tuples(lab(K0,sdrs([lab(L,B)|D],R)),K,R1-R3,W,T1-[T|T3],N1-N3,I1-I3):- !, 
%   T = tuple([],K0,dominates,L,[]),
%   tuples(lab(L,B),K,R1-R2,W,T1-T2,N1-N2,I1-I2), 
%   tuples(lab(K0,sdrs(D,R)),K,R2-R3,W,T2-T3,N2-N3,I2-I3).

tuples(lab(L,B),K,R1-R2,W,T1-[T|T2],N1-N2,I1-I2):- !, 
   T = tuple([],K,dominates,L,[]),
   tuples(B,L,R1-R2,W,T1-T2,N1-N2,I1-I2).

tuples(sub(lab(L1,B1),B2),K,R1-R3,W,T1-[T|T3],N1-N3,I1-I3):- !, 
   T = tuple([],K,dominates,L1,[]),
   tuples(B1,L1,R1-R2,W,T1-T2,N1-N2,I1-I2), 
   tuples(B2,L1,R2-R3,W,T2-T3,N2-N3,I2-I3).

tuples(_:drs([],C),K,R1-R2,W,T1-T2,N1-N2,I1-I2):- !, tuples(C,K,R1-R2,W,T1-T2,N1-N2,I1-I2).

% This clause adds a 'main' label for events and states
%
tuples(B:drs([_:I:R|L],C),K,R1-R2,W,T1-[Tu1,Tu2|T2],N1-N2,I1-I2):- 
    member(_:_:role(R,_,_,1),C), 
    \+ member(_:_:role(_,R,_,-1),C),   % don't add main lables for inversed roles!
    member(POS,[v,a]),
    member(_:J:pred(R,_,POS,_),C), !,
    word(I,I1,W,Word),
    Tu1 = tuple(I,K,referent,K:R,Word),
    Tu2 = tuple(J,K:R,main,K,Word),
    tuples(B:drs(L,C),K,[K:R|R1]-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples(B:drs([_:I:R|L],C),K,R1-R2,W,T1-[T|T2],N1-N2,I1-I2):- !,
    word(I,I1,W,Word),
    T = tuple(I,K,referent,K:R,Word),
    tuples(B:drs(L,C),K,[K:R|R1]-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([lab(A,B)|L],K,R1-R3,W,T1-T3,N1-N3,I1-I3):- !, tuples(lab(A,B),K,R1-R2,W,T1-T2,N1-N2,I1-I2),tuples(L,K,R2-R3,W,T2-T3,N2-N3,I2-I3).

tuples([sub(A,B)|L],K,R1-R3,W,T1-T3,N1-N3,I1-I3):- !, tuples(sub(A,B),K,R1-R2,W,T1-T2,N1-N2,I1-I2),tuples(L,K,R2-R3,W,T2-T3,N2-N3,I2-I3).

tuples([_:I:pred(X,Sym,n,Sense)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2,I1-I2):-
    word(I,I1,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,          concept,  CC:Sym:Sense,[]),
    E2 = tuple(I,CC:Sym:Sense,instance, Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:I:pred(X,Sym,v,Sense)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2,I1-I2):- 
    word(I,I1,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,          event,CC:Sym:Sense,[]),
    E2 = tuple(I,CC:Sym:Sense,instance,    Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:I:pred(X,_,s,1)|L],K,R1-R2,W,T1-[E|T2],N1-N2,I1-I2):- 
    word(I,I1,W,Word), nonvar(X), member(Dom:X,R1), !,
    E = tuple(I,K,function,       Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:I:pred(X,Sym,a,Sense)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2,I1-I2):- 
    word(I,I1,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,          attribute,CC:Sym:Sense,[]),
    E2 = tuple(I,CC:Sym:Sense,arg,       Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:I:pred(X,Sym,r,Sense)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2,I1-I2):- 
    word(I,I1,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,          attribute,CC:Sym:Sense,[]),
    E2 = tuple(I,CC:Sym:Sense,arg,       Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:I:named(X,Sym,Type,_)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2,I1-I2):- 
    word(I,I1,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,         named,CC:Sym:Type,[]),
    E2 = tuple(I,CC:Sym:Type,instance,  Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([B:_:timex(X,date(_,I2:Y,I3:M,I4:D))|L],K,R1-R2,W,T,N,I):- !,
    tuples([B:I2:timex(X,year,Y),B:I3:timex(X,month,M),B:I4:timex(X,day,D)|L],K,R1-R2,W,T,N,I).

tuples([_:I:timex(X,Type,Sym)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2,I1-I2):- 
    word(I,I1,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,         Type,CC:Sym:Type,[]),
    E2 = tuple(I,CC:Sym:Type,arg,  Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:I:eq(X,Y)|L],K,R1-R2,W,T1-[E1,E2,E3|T2],N1-N2,I1-I2):- 
    word(I,I1,W,Word), nonvar(X), nonvar(Y), member(D1:X,R1), member(D2:Y,R1), !,
    condcounter(CC),
    E1 = tuple(I, K,    relation,CC:equality,Word),
    E2 = tuple([],CC:equality,int,D1:X,[]),
    E3 = tuple([],CC:equality,ext,D2:Y,[]),
%    E1 = tuple([], K,    relation,CC:equality,Word),
%    E2 = tuple([],CC:equality,int,D1:X,[]),
%    E3 = tuple( I,CC:equality,ext,D2:Y,[]),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([B:I:rel(X,Y,subset_of,Sense)|L],K,R,W,T,N,Is):-
   tuples([B:I:rel(Y,X,superset_of,Sense)|L],K,R,W,T,N,Is).

tuples([_:I:rel(X,Y,Sym,Sense)|L],K,R1-R2,W,T1-[E1,E2,E3|T2],N1-N2,I1-I2):-
    word(I,I1,W,Word), nonvar(X), nonvar(Y), member(D1:X,R1), member(D2:Y,R1), !,
    condcounter(CC),
    E1 = tuple([],K,relation,CC:Sym:Sense,[]),
    E2 = tuple([],CC:Sym:Sense,int,D1:X,[]),
    E3 = tuple( I,CC:Sym:Sense,ext,D2:Y,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:I:role(X,Y,Sym,Dir)|L],K,R1-R2,W,T1-[E1,E2,E3|T2],N1-N2,I1-I2):-
    word(I,I1,W,Word), nonvar(X), nonvar(Y), member(D1:X,R1), member(D2:Y,R1), !,
    condcounter(CC),
    E1 = tuple([],K,role,CC:Sym:Dir,[]),
    E2 = tuple([],CC:Sym:Dir,int,D1:X,[]),
    E3 = tuple( I,CC:Sym:Dir,ext,D2:Y,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([I:rel(X,Y,Sym)|L],K,R1-R2,W,T1-[E|T2],N1-N2,I1-I2):-
%    word(I,I1,W,Word), !, 
%    E = tuple(I,X,Sym,Y,Word),
    word(_,I1,W,Word), !, 
    E = tuple([],X,Sym,Y,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:I:card(X,Y,Type)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2,I1-I2):-
    word(I,I1,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,       cardinality,CC:Y:Type,[]),
    E2 = tuple(I,CC:Y:Type,arg,         Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2,[I|I1]-I2).

tuples([_:_:prop(X,B)|L],K,R1-R1,W,T1-T3,N1-N3,I1-I3):-
    nonvar(X), member(Dom:X,R1), !,
    tuples(B,Dom:X,R1-_,W,T1-T2,N1-N2,I1-I2),
    tuples(L,K,R1-_,W,T2-T3,N2-N3,I2-I3).

tuples([_:_:prop(X,B)|L],K,R1-R1,W,T1-[T|T3],N1-N3,I1-I3):-
    nonvar(X), member(Dom:X,R1), !,
    T = tuple([],K,subordinates:prop,Dom:X,[]),
    tuples(B,Dom:X,R1-_,W,T1-T2,N1-N2,I1-I2),
    tuples(L,K,R1-_,W,T2-T3,N2-N3,I2-I3).

tuples([_:_:not(B)|L],K1,R1-R1,W,T1-[E1,E2|T3],N1-N4,I1-I3):- !,
    condcounter(CC),
    E1 = tuple([],K1,unary,CC:not,[]),
    E2 = tuple([],CC:not,scope,K2,[]),
    label(N1,k,K2,N2),
    tuples(B,K2,R1-_,W,T1-T2,N2-N3,I1-I2),
    tuples(L,K1,R1-_,W,T2-T3,N3-N4,I2-I3).

tuples([_:_:pos(B)|L],K1,R1-R1,W,T1-[E1,E2|T3],N1-N4,I1-I3):- !,
    condcounter(CC),
    E1 = tuple([],K1,unary,CC:pos,[]),
    E2 = tuple([],CC:pos,scope,K2,[]),
    label(N1,k,K2,N2),
    tuples(B,K2,R1-_,W,T1-T2,N2-N3,I1-I2),
    tuples(L,K1,R1-_,W,T2-T3,N3-N4,I2-I3).

tuples([_:_:nec(B)|L],K1,R1-R1,W,T1-[E1,E2|T3],N1-N4,I1-I3):- !,
    condcounter(CC),
    E1 = tuple([],K1,unary,CC:nec,[]),
    E2 = tuple([],CC:nec,scope,K2,[]),
    label(N1,k,K2,N2),
    tuples(B,K2,R1-_,W,T1-T2,N2-N3,I1-I2),
    tuples(L,K1,R1-_,W,T2-T3,N3-N4,I2-I3).

tuples([_:_:imp(B1,B2)|L],K1,R1-R1,W,T1-[E1,E2,E3|T4],N1-N6,I1-I4):- !,
    condcounter(CC),
    E1 = tuple([],K1,binary,CC:imp,[]),
    E2 = tuple([],CC:imp,antecedent,K2,[]),
    E3 = tuple([],CC:imp,consequent,K3,[]),
    label(N1,k,K2,N2),
    tuples(B1,K2,R1-R2,W,T1-T2,N2-N3,I1-I2),
    label(N3,k,K3,N4),
    tuples(B2,K3,R2-_,W,T2-T3,N4-N5,I2-I3),
    tuples(L,K1,R1-_,W,T3-T4,N5-N6,I3-I4).

tuples([_:_:or(B1,B2)|L],K1,R1-R1,W,T1-[E1,E2,E3|T4],N1-N6,I1-I4):- !,
    condcounter(CC),
    E1 = tuple([],K1,binary,CC:or,[]),
    E2 = tuple([],CC:or,antecedent,K2,[]),
    E3 = tuple([],CC:or,consequent,K3,[]),
    label(N1,k,K2,N2),
    tuples(B1,K2,R1-R2,W,T1-T2,N2-N3,I1-I2),
    label(N3,k,K3,N4),
    tuples(B2,K3,R2-_,W,T2-T3,N4-N5,I2-I3),
    tuples(L,K1,R1-_,W,T3-T4,N5-N6,I3-I4).

tuples([_:_:duplex(Type,B1,_,B2)|L],K1,R1-R1,W,T1-[E1,E2,E3|T4],N1-N6,I1-I4):- !,
    condcounter(CC),
    E1 = tuple([],K1,duplex,CC:Type,[]),
    E2 = tuple([],CC:Type,antecedent,K2,[]),
    E3 = tuple([],CC:Type,consequent,K3,[]),
    label(N1,k,K2,N2),
    tuples(B1,K2,R1-R2,W,T1-T2,N2-N3,I1-I2),
    label(N3,k,K3,N4),
    tuples(B2,K3,R2-_,W,T2-T3,N4-N5,I2-I3),
    tuples(L,K1,R1-_,W,T3-T4,N5-N6,I3-I4).

tuples([Err|L],K,R,W,T,N,I):- !, 
     warning('unknown tuple: ~p',[Err]),
     tuples(L,K,R,W,T,N,I).

tuples([],_,R-R,_,T-T,N-N,I-I).


/* =======================================================================
   Convert words in tuple format
======================================================================= */

word([],_,_,[]):- !.
word([Index|Is],Old,W,Ws):- member(Js,Old), member(Index,Js), !, word(Is,Old,W,Ws).
word([Index|Is],Old,W,[Tok|Ws]):- member(Index:[tok:Tok|_],W), !, word(Is,Old,W,Ws).
word([_|Is],Old,W,Ws):- word(Is,Old,W,Ws).


/* =======================================================================
   Output tuples to stream
======================================================================= */

write_tuples([],_).

write_tuples([tuple(_Index,Order,Node1,Edge,Node2,Words)|L],Stream):- !,
   write_node(Node1,Stream),
   format(Stream,' ~w ',[Edge]),
   write_node(Node2,Stream),
   format(Stream,' ~w [ ',[Order]),
   write_tokens(Words,Stream),
   write_tuples(L,Stream).

write_tuples([T|L],Stream):-
   warning('unable to output tuple ~p',[T]),
   write_tuples(L,Stream).


/* =======================================================================
   Output tokens to stream
======================================================================= */

write_tokens([],Stream):- !, write(Stream,']'), nl(Stream).
write_tokens([X|L],Stream):- format(Stream,'~w ',[X]), write_tokens(L,Stream).


/* =======================================================================
   Output nodes to stream
======================================================================= */

write_node(A:B,Stream):- !, format(Stream,'~w:',[A]), write_node(B,Stream).
write_node(A,Stream):- format(Stream,'~w',[A]).


/* =======================================================================
   Generate from DRG (wrapper)
======================================================================= */

unboxer(_,_):- !.         % still in development!

unboxer(Tuples,Tags):-
   findall(T,(T=tuple(_,_,_,ext,_,_),member(T,Tuples)),Ext),
   gen(Tuples,Ext,[],S),
   warning('unboxer says: ~p',[S]),
   compare(S,Tags), !.

unboxer(_,_):-
   warning('unboxer failed',[]).


/* =======================================================================
   Comparing with gold standard
======================================================================= */

compare([],[]):- !.

compare([Tok|L1],[_:[tok:Tok|_]|L2]):- !,
   compare(L1,L2).

compare([Tok1|_],[_:[tok:Tok2|_]|_]):- !,
   warning('unboxer generated different surface token "~p" instead of "~p"',[Tok1,Tok2]).

compare([Tok1|_],[]):- !,
   warning('unboxer generated exta surface token "~p"',[Tok1]).

compare([],[_:[tok:Tok2|_]|_]):- !,
   warning('unboxer missed surface token "~p"',[Tok2]).


/* =======================================================================
   Generate from DRG
======================================================================= */

gen([],_,G,T):- 
   compose(G,C),
   member(_:[S],C), 
%  write(S),nl,
   text(S,[],T), !.

gen(L1,Ext,G,S):-
   select(tuple(_,_,K1,dominates,K2,[]), L1,L2),  % redundant, remove
   member(tuple(_,_,K1,_,K2,_),L2), !,
   gen(L2,Ext,G,S).

gen(L1,Ext,G1,S):-                                %         K1
   select(tuple(_,_,K1,Rel,K2,T1), L1,L2),        %         |
   member(Rel,[dominates,because]),               % ... --> K2 --> Old
   member(tuple(_,_,_,continuation,K2,T2),L1),    % 
   select(K1:C,G1,G2), !,                         % 
   concatenate([T1,T2,v(K2)|C],Reduced),          %
   gen(L2,Ext,[K1:Reduced|G2],S).                 % Generate: K1 = K2 + Old(K1)

gen(L1,Ext,G1,S):-                                %   K1
   select(tuple(_,_,K1,Rel,K2,T), L1,L2),         %   |
   member(Rel,[dominates,because]),               %   K2 --> Old
   select(K1:C,G1,G2), !,                         % 
   concatenate([T,v(K2)|C],Reduced),              %
   gen(L2,Ext,[K1:Reduced|G2],S).                 % Generate: K1 = K2 + Old(K1)

gen(L1,Ext,G,S):-                                 %         K1
   select(tuple(_,_,K1,Rel,K2,T1), L1,L2),        %         |
   member(Rel,[dominates,because]),               % ... --> K2
   member(tuple(_,_,_,continuation,K2,T2),L1), !, % 
   concatenate([T1,T2,v(K2)],Reduced),            % NOT GUARENTEED TO BE THE LAST!
   gen(L2,Ext,[K1:Reduced|G],S).                  % Generate: K1 = K2

gen(L1,Ext,G,S):-                                 %   K1
   select(tuple(_,_,K1,Rel,K2,T), L1,L2),         %   |
   member(Rel,[dominates,because]), !,            %   K2
   concatenate([T,v(K2)],Reduced),                %
   gen(L2,Ext,[K1:Reduced|G],S).                  % Generate: K1 = K2

gen([T|L],Ext,G1,S):-
   T = tuple(_I,_O,_N,_E,X,_T),
   select(X:C,G1,G2), !,                          % X is already generated
   split(L,X,WithX,WithoutX), 
   partial(1,[T|WithX],Ext,Partial),
   append(Partial,C,Appended),
   concatenate(Appended,Reduced),
   gen(WithoutX,Ext,[X:Reduced|G2],S).

gen([T|L],Ext,G,S):-
   T = tuple(_I,_O,_N,_E,X,_T),
   split(L,X,WithX,WithoutX), 
   partial(1,[T|WithX],Ext,Partial),
   concatenate(Partial,Reduced),
%  write(E:X:Reduced),nl,
   gen(WithoutX,Ext,[X:Reduced|G],S).


/* =======================================================================
   Generate a partial (incomplete) surface string
======================================================================= */

partial(I,L1,Ext,[v(V)|S2]):-
   select(tuple(_,I,N,int,_,_),L1,L2), 
   member(tuple(_,_,N,ext,V,_),Ext), !,
   J is I + 1,
   partial(J,L2,Ext,S2).

partial(I,L1,Ext,[v(V)|S2]):-
   select(tuple(_,I,V,main,_,[]),L1,L2), !,
   J is I + 1,
   partial(J,L2,Ext,S2).

partial(I,L1,Ext,[S1|S2]):-
   select(tuple(_,I,_,_,_,S1),L1,L2), !,
   J is I + 1,
   partial(J,L2,Ext,S2).

partial(_,_,_,[]).


/* =======================================================================
   Concatenate a surface string, v(V) denotes a variable V 
======================================================================= */

concatenate([],[]):- !.
concatenate([[]|L1],L2):- !, concatenate(L1,L2).
concatenate([X],[X]):- !.
concatenate([v(V)|L1],[v(V)|L2]):- !, concatenate(L1,L2).
concatenate([L,v(V)|L1],[L,v(V)|L2]):- !, concatenate(L1,L2).
concatenate([La,Lb|L1],L2):- !, append(La,Lb,L), concatenate([L|L1],L2).


/* =======================================================================
   Compose surface string
======================================================================= */

% compose(L,_):-  write(l:L),nl, fail.

compose(L1,L7):-
   select(V1:[S],L1,L2),          % complete strings, three variables
   secondOccur(S,S2),
   select(V2:Sa1,L2,L3),          % V1 part of V2 and
   subst(Sa1,V1:[S2],Sa2),         
   select(V3:Sb1,L3,L4),          % V1 part of V3 
   subst(Sb1,V1:[S2],Sb2), 
   select(V4:Sc1,L4,L5),          % V1 part of V4 
   subst(Sc1,V1:[S2],Sc2), 
   select(V5:Sd1,L5,L6),          % V1 part of V5
   subst(Sd1,V1:[S2],Sd2), !,
   concatenate(Sa2,Sa3),
   concatenate(Sb2,Sb3),
   concatenate(Sc2,Sc3),
   concatenate(Sd2,Sd3),
   compose([V2:Sa3,V3:Sb3,V4:Sc3,V5:Sd3|L6],L7).

compose(L1,L6):-
   select(V1:[S],L1,L2),          % complete strings, three variables
   secondOccur(S,S2),
   select(V2:Sa1,L2,L3),          % V1 part of V2 and
   subst(Sa1,V1:[S2],Sa2),         
   select(V3:Sb1,L3,L4),          % V1 part of V3 
   subst(Sb1,V1:[S2],Sb2), 
   select(V4:Sc1,L4,L5),          % V1 part of V4 
   subst(Sc1,V1:[S2],Sc2), !,
   concatenate(Sa2,Sa3),
   concatenate(Sb2,Sb3),
   concatenate(Sc2,Sc3),
   compose([V2:Sa3,V3:Sb3,V4:Sc3|L5],L6).

compose(L1,L5):-
   select(V1:[S],L1,L2),          % complete strings, two variables
   secondOccur(S,S2),
   select(V2:Sa1,L2,L3),          % V1 part of V2 and
   subst(Sa1,V1:[S2],Sa2),         
   select(V3:Sb1,L3,L4),          % V1 part of V3 
   subst(Sb1,V1:[S2],Sb2), !,
   concatenate(Sa2,Sa3),
   concatenate(Sb2,Sb3),
   compose([V2:Sa3,V3:Sb3|L4],L5).

compose(L1,L4):-
   select(V1:[S1],L1,L2),          % complete strings, one variable
   select(V2:S2,L2,L3),            % V1 part of V2
   subst(S2,V1:[S1],S3), !,
   concatenate(S3,S4),
   compose([V2:S4|L3],L4).

compose(L1,L4):-
   select(V1:[],L1,L2),            % empty strings
   select(V2:S2,L2,L3),
   subst(S2,V1:[],S3), !,
   concatenate(S3,S4),
   compose([V2:S4|L3],L4).

compose(L1,L3):-
  select(_:[],L1,L2), !,
  compose(L2,L3).

compose(L,L).


/* =======================================================================
   Substitute variable for string
======================================================================= */

subst([v(X)|L],X:[],L):- !.
subst([v(X)|L],X:[V],[V|L]):- !.
subst([X|L1],Y:V,[X|L2]):- subst(L1,Y:V,L2).


/* =======================================================================
   Second Occurrence
======================================================================= */

secondOccur([],[]).
secondOccur([X|L1],[m(X)|L2]):- secondOccur(L1,L2).


/* =======================================================================
   Map Multiple Occurrence to Text
======================================================================= */

text([],_,[]).
text([m(m(X))|L1],A,L2):- !, text([m(X)|L1],A,L2).
text([m(X)|L1],A,L2):- member(X,A), !, text(L1,A,L2).  % already generated
text([m(X)|L1],A,[X|L2]):- !, text(L1,[X|A],L2).
text([X|L1],A,[X|L2]):- text(L1,A,L2).
