
:- module(sortalCheck,[sortalCheckDrs/2]).

:- use_module(knowledge(ontology),[isa/2,isnota/2]).
:- use_module(knowledge(title),[title/3]).
:- use_module(library(lists),[member/2,select/3]).


/*========================================================================
   Sortal Check (main)
========================================================================*/

sortalCheckDrs(B,Var):-
   sortalCheckDrs(B,Var,[],P),  
   allconsistent(P).


/*========================================================================
   Sortal Check (DRSs)
========================================================================*/

sortalCheckDrs(_:drs([],C),Var,P1,P2):- !,
   sortalCheckConds(C,Var,P1,P2).

sortalCheckDrs(K:drs([_:_:X|D],C),Var,P1,P2):- 
   X == Var, !,
   sortalCheckDrs(K:drs(D,C),Var,[ref([])|P1],P2).

sortalCheckDrs(drs([_|D],C),Var,P1,P2):- !,
   sortalCheckDrs(drs(D,C),Var,P1,P2).

sortalCheckDrs(merge(B1,B2),Var,P1,P3):- !,
   sortalCheckDrs(B1,Var,P1,P2),
   sortalCheckDrs(B2,Var,P2,P3).

sortalCheckDrs(alfa(_,B1,B2),Var,P1,P3):- !,
   sortalCheckDrs(B1,Var,P1,P2),
   sortalCheckDrs(B2,Var,P2,P3).


/*========================================================================
   Sortal Check (DRS-Conditions)
========================================================================*/

sortalCheckConds([],_,P,P):- !.

sortalCheckConds([_:Cond|C],Var,P1,P2):- !,
   sortalCheckConds([Cond|C],Var,P1,P2).

sortalCheckConds([not(drs([],[_:eq(X,Y)]))|_],_,_,_):- 
   X==Y, !, fail.

sortalCheckConds([not(B)|C],Var,P1,P3):- !,
   sortalCheckDrs(B,Var,P1,P2),
   sortalCheckConds(C,Var,P2,P3).

sortalCheckConds([nec(B)|C],Var,P1,P3):- !,
   sortalCheckDrs(B,Var,P1,P2),
   sortalCheckConds(C,Var,P2,P3).

sortalCheckConds([pos(B)|C],Var,P1,P3):- !,
   sortalCheckDrs(B,Var,P1,P2),
   sortalCheckConds(C,Var,P2,P3).

sortalCheckConds([prop(_,B)|C],Var,P1,P3):- !,
   sortalCheckDrs(B,Var,P1,P2),
   sortalCheckConds(C,Var,P2,P3).

sortalCheckConds([imp(B1,B2)|C],Var,P1,P4):- !,
   sortalCheckDrs(B1,Var,P1,P2),
   sortalCheckDrs(B2,Var,P2,P3),
   sortalCheckConds(C,Var,P3,P4).

sortalCheckConds([duplex(_,B1,_,B2)|C],Var,P1,P4):- !,
   sortalCheckDrs(B1,Var,P1,P2),
   sortalCheckDrs(B2,Var,P2,P3),
   sortalCheckConds(C,Var,P3,P4).

sortalCheckConds([or(B1,B2)|C],Var,P1,P4):- !,
   sortalCheckDrs(B1,Var,P1,P2),
   sortalCheckDrs(B2,Var,P2,P3),
   sortalCheckConds(C,Var,P3,P4).

sortalCheckConds([pred(X,Sym,_,_)|C],Var,[ref(Ps)|P1],P2):-
   X == Var, !,
%   select(ref(Ps),P1,P2), !,
   sortalCheckConds(C,Var,[ref([Sym|Ps])|P1],P2).

sortalCheckConds([pred(_,_,_,_)|C],Var,P1,P2):-  
   sortalCheckConds(C,Var,P1,P2).

sortalCheckConds([named(X,Title,_,ttl)|C],Var,P1,P3):-  
   X == Var,
   title(_,Title,Sym),  
   select(ref(Ps),P1,P2), !,
   sortalCheckConds(C,Var,[ref([Sym|Ps])|P2],P3).

sortalCheckConds([named(_,_,_,_)|C],Var,P1,P2):-  
   sortalCheckConds(C,Var,P1,P2).

sortalCheckConds([rel(_,_,_,_)|C],Var,P1,P2):-  
   sortalCheckConds(C,Var,P1,P2).

sortalCheckConds([role(_,_,_,_)|C],Var,P1,P2):-  
   sortalCheckConds(C,Var,P1,P2).

sortalCheckConds([card(_,_,_)|C],Var,P1,P2):-  
   sortalCheckConds(C,Var,P1,P2).

sortalCheckConds([timex(_,_)|C],Var,P1,P2):-  
   sortalCheckConds(C,Var,P1,P2).

sortalCheckConds([eq(_,_)|C],Var,P1,P2):-  
   sortalCheckConds(C,Var,P1,P2).


/*========================================================================
   Consistency Check (all referents)
========================================================================*/

allconsistent([]):- !.

allconsistent([ref(Concepts)|L]):-  
   consistent(Concepts),
   allconsistent(L).


/*========================================================================
   Consistency Check
========================================================================*/

consistent([]):- !.

consistent([_]):- !.

consistent(L1):- 
   addSupConcepts(L1,L2),
   \+ conflict(L2).


/*========================================================================
   Add super concepts (by iteration until fixed point is reached)
========================================================================*/

addSupConcepts(C1,C3):- addSuper(C1,[],C2,Add), addSupConcepts(Add,C2,C3).
addSupConcepts([],C1,C2):- !, C2=C1.
addSupConcepts(_,C1,C2):- addSupConcepts(C1,C2).


/*========================================================================
   Add super concepts (one cycle)
========================================================================*/

addSuper([],L,L,[]).

addSuper([X|L1],Accu,L2,[Y|Added]):-
   isa(X,Y),
   \+ member(Y,L1), \+ member(Y,Accu), !,  % if not yet added
   addSuper(L1,[X,Y|Accu],L2,Added).       % then add concept

addSuper([X|L1],Accu,L2,Added):-
   addSuper(L1,[X|Accu],L2,Added).


/*========================================================================
   Check for a conflict
========================================================================*/

conflict(L):-
   member(X,L),
   isnota(X,Y),
   member(Y,L), !.
   

