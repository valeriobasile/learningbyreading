
:- module(freeVarCheck,[freeVarCheckDrs/1,boundVarCheckContext/2,drsCondition/2]).

:- use_module(library(lists),[member/2,append/3]).

/* ========================================================================
   Bound Variable Check (main predicate)
======================================================================== */

boundVarCheckContext(Context,ADRS):-
   \+ \+ ( numbervars(Context,0,_),
           boundVarDrs(Context,[],[],ADRS) ).


/* ========================================================================
   Bound Variable Check (traversing lists of DRSs, last one most global)
======================================================================== */

boundVarDrs([K1:drs(D1,_),K2:B2|L],D2,S,ADRS):- !,
   append(D1,D2,D3), 
   boundVarDrs([K2:B2|L],D3,[sub(K2,K1)|S],ADRS).

boundVarDrs([_,X|_],_,_,_):- !,
   warning('unknown context while checking bound variables: ~p',[X]).

boundVarDrs([K:drs(D1,_)],D2,S1,ADRS):- !,
   append(D1,D2,D3), 
   freePointers(D3,K,S1,S2),
   bound(D3,S2,ADRS).

boundVarDrs([X],_,_,_):- !,
   warning('unknown context while checking bound variables: ~p',[X]).



/* ========================================================================
   Adding free pointers to subordination constraints
======================================================================== */

freePointers([],_,S,S):- !.
freePointers([X:_:_|L],K,S1,S2):- member(sub(X,_),S1), !, freePointers(L,K,S1,S2).
freePointers([X:_:_|L],K,S1,S2):- member(sub(_,X),S1), !, freePointers(L,K,S1,S2).
freePointers([X:_:_|L],K,S1,S2):- \+ X=K, !, freePointers(L,K,[sub(X,K)|S1],S2).
freePointers([_:_:_|L],K,S1,S2):- freePointers(L,K,S1,S2).


/* ========================================================================
   A variable is bound if it is not free
======================================================================== */

bound(Dom,Sub,ADRS):- \+ free(Dom,Sub,ADRS).


/* ========================================================================
   A variable is free if there is a condition with a variable that is not
   in the domain of a super-ordinated DRS
======================================================================== */

free(Dom,Sub,_:drs(_,Conds)):- 
   member(L2:_:Cond,Conds),
   drsCondition(X,Cond),
   \+ (subordinates(L1,L2,Sub), member(L1:_:X,Dom)).


/* ========================================================================
   Subordination
======================================================================== */

subordinates(L1,L2,_):- L1=L2.
subordinates(L1,L2,S):- member(sub(L1,L2),S).
subordinates(L1,L2,S):- member(sub(L1,L3),S), subordinates(L3,L2,S).


/* ========================================================================
   Basic Conditions
======================================================================== */

drsCondition(X,pred(X,_,_,_)).
drsCondition(X,named(X,_,_,_)).
drsCondition(X,card(X,_,_)).
drsCondition(X,timex(X,_)).
drsCondition(X,rel(X,_,_,_)).
drsCondition(X,rel(_,X,_,_)).
drsCondition(X,eq(X,_)).
drsCondition(X,eq(_,X)).
drsCondition(X,role(X,_,_,_)).
drsCondition(X,role(_,X,_,_)).


/*========================================================================
   Free Variable Check (main predicate)
========================================================================*/

freeVarCheckDrs(Drs):-
   freeVarCheckDrs(Drs,[]-_).


/*========================================================================
   Free Variable Check (DRSs)
========================================================================*/

freeVarCheckDrs([],L-L):- !.

freeVarCheckDrs([B|L],L1-L3):- !,
   freeVarCheckDrs(B,L1-L2),
   freeVarCheckDrs(L,L2-L3).

freeVarCheckDrs(K:drs([_:_:X|D],C),L1-L2):- !,
   freeVarCheckDrs(K:drs(D,C),[X|L1]-L2).

freeVarCheckDrs(_:drs([],C),L-L):- !,
   freeVarCheckConds(C,L).

freeVarCheckDrs(merge(B1,B2),L1-L3):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).

freeVarCheckDrs(sdrs([lab(K,B)|O],C),L1-L3):- !,
   freeVarCheckDrs(B,[K|L1]-L2),
   freeVarCheckDrs(sdrs(O,C),L2-L3).

freeVarCheckDrs(sdrs([sub(lab(K1,B1),lab(K2,B2))|O],C),L1-L3):- !,
   freeVarCheckDrs(B1,[K1|L1]-L2),
   freeVarCheckDrs(B2,[K2|L2]-_),
   freeVarCheckDrs(sdrs(O,C),[K2|L2]-L3).

freeVarCheckDrs(sdrs([],C),L-L):- !,
   freeVarCheckConds(C,L).

freeVarCheckDrs(lab(K,B),L1-L2):- !,
   freeVarCheckDrs(B,[K|L1]-L2).

freeVarCheckDrs(alfa(_,B1,B2),L1-L3):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).


/*========================================================================
   Free Variable Check (List of conditions)
========================================================================*/

freeVarCheckConds([],_):- !.

freeVarCheckConds([_:_:X|C],L):-
   freeVarCheckCond(X,L),
   freeVarCheckConds(C,L).


/*========================================================================
   Free Variable Check (Conditions)
========================================================================*/

freeVarCheckCond(not(B),L):- !,
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(nec(B),L):- !,
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(pos(B),L):- !,
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(prop(X,B),L):- !,
   checkTerm(X,L),
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(imp(B1,B2),L1):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(duplex(_,B1,X,B2),L1):- !,
   freeVarCheckDrs(B1,L1-L2),
   checkTerm(X,L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(or(B1,B2),L):- !,
   freeVarCheckDrs(B1,L-_),
   freeVarCheckDrs(B2,L-_).

freeVarCheckCond(pred(Arg,_,_,_),L):- !,
   checkTerm(Arg,L).

freeVarCheckCond(rel(Arg1,Arg2,_,_),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).

freeVarCheckCond(role(Arg1,Arg2,_,_),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).

freeVarCheckCond(rel(Arg1,Arg2,_),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).

freeVarCheckCond(card(Arg1,Arg2,_),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).

freeVarCheckCond(named(Arg,_,_,_),L):- !,
   checkTerm(Arg,L).

freeVarCheckCond(timex(Arg,_),L):- !,
   checkTerm(Arg,L).

freeVarCheckCond(eq(Arg1,Arg2),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).


/*========================================================================
   Check Term
========================================================================*/

checkTerm(X,L):- var(X), member(Y,L), X==Y, !.
checkTerm(X,_):- atomic(X), !.
