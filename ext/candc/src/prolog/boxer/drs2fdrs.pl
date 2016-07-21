
:- module(drs2fdrs,[eqDrs/2,         %%% should go in its own module!
                    instDrs/1,
                    instDrs/2]).

:- use_module(library(lists),[select/3,member/2]).
:- use_module(semlib(options),[option/2]).


/*========================================================================
   Dynamic  Predicates
========================================================================*/

:- dynamic refcounter/2.


/*========================================================================
  Init Counters
========================================================================*/

init:- 
   retractall(refcounter(_,_)),
   assert(refcounter(116,1)), % t
   assert(refcounter(118,1)), % l
   assert(refcounter(120,1)), % x
   assert(refcounter(115,1)), % s
   assert(refcounter(112,1)), % p
   assert(refcounter(101,1)), % e
   assert(refcounter(102,1)), % f
   assert(refcounter(107,1)), % k
   assert(refcounter( 98,1)). % b



/*========================================================================
   Main Predicates
========================================================================*/

instDrs(B):- 
   init, 
   instantDrs(B).

instDrs(B,N):- 
   init, 
   instantDrs(B), 
   refcounter(107,K),
   refcounter(101,E),
   N is K+E.


/*========================================================================
   Variable
========================================================================*/

avar(Var):- var(Var), !.
avar(Var):- atom(Var), !.
avar(Var):- functor(Var,'$VAR',1).


/*========================================================================
   Referent
========================================================================*/

ref(Ref,Code):-
   var(Ref), !,
   getIndex(Code,X),
   number_codes(X,Codes),
   atom_codes(Ref,[Code|Codes]).

ref(_,_).


/*========================================================================
   Get Index
========================================================================*/

getIndex(Sort,X):-
   refcounter(Sort,X), !,
   retract(refcounter(Sort,X)),
   Y is X + 1,
   assert(refcounter(Sort,Y)).

getIndex(Sort,X):- 
   \+ Sort = 120,
   getIndex(120,X).


/*========================================================================
   Sort Referent: time (116), event (101), proposition (112), entity (120)
========================================================================*/

sortref(X,Conds,116):- member(_:_:pred(Y,now,a,1),Conds), X==Y, !.
sortref(X,Conds,116):- member(_:_:rel(_,Y,temp_overlap,1),Conds), X==Y, !.
sortref(X,Conds,116):- member(_:_:rel(_,Y,temp_before,1),Conds), X==Y, !.
sortref(X,Conds,116):- member(_:_:rel(Y,_,temp_before,1),Conds), X==Y, !.
sortref(X,Conds,116):- member(_:_:rel(_,Y,temp_included,1),Conds), X==Y, !.

sortref(X,Conds,101):- member(_:_:pred(Y,_,v,_),Conds), X==Y, !.
sortref(X,Conds,101):- member(_:_:rel(_,Y,temp_abut,1),Conds), X==Y, !.
sortref(X,Conds,101):- member(_:_:rel(Y,_,temp_abut,1),Conds), X==Y, !.
sortref(X,Conds,101):- member(_:_:rel(Y,_,temp_overlap,1),Conds), X==Y, !.

sortref(X,Conds,120):- member(_:_:pred(Y,_,n,_),Conds), X==Y, !.
sortref(X,Conds,115):- member(_:_:pred(Y,_,a,_),Conds), X==Y, !.

sortref(X,Conds,112):- member(_:_:prop(Y,_),Conds), X==Y, !.
sortref(_,_    ,120).


/*========================================================================
   Instantiating DRSs
========================================================================*/

instantDrs(Var):- var(Var), !, ref(Var,102).

instantDrs(Var):- atom(Var), !.

instantDrs(Var):- Var =.. ['$VAR',_], !.

instantDrs(drs([_:Ref|Dom],Conds)):- !,
   sortref(Ref,Conds,Sort),
   ref(Ref,Sort), 
   instantDrs(drs(Dom,Conds)).

instantDrs(B:drs([Lab:_:Ref|Dom],Conds)):- !,
   ref(Lab,98), 
   sortref(Ref,Conds,Sort),
   ref(Ref,Sort), 
   instantDrs(B:drs(Dom,Conds)).

instantDrs(B:drs([],Conds)):- !,
   ref(B,98), 
   instantConds(Conds).

instantDrs(drs([],Conds)):- !,
   instantConds(Conds).

instantDrs(merge(A1,A2)):- !,
   instantDrs(A1),
   instantDrs(A2).

instantDrs(sdrs([],_)):- !.

instantDrs(sdrs([X|L],C)):- !,
   instantDrs(X),
   instantDrs(sdrs(L,C)).

instantDrs(lab(K,B)):- !,
   ref(K,107),
   instantDrs(B).

instantDrs(sub(B1,B2)):- !,
   instantDrs(B1),
   instantDrs(B2).

instantDrs(alfa(_,A1,A2)):- !,
   instantDrs(A1),
   instantDrs(A2).

instantDrs(app(A1,A2)):- !,
   instantDrs(A1),
   instantDrs(A2).

instantDrs(lam(X,A)):- !,
   ref(X,118),
   instantDrs(A).


/*========================================================================
   Instantiating DRS-Conditions
========================================================================*/

instantConds([]).

instantConds([Label:_:Cond|Conds]):- !,
   ref(Label,98),
   instantCond(Cond),
   instantConds(Conds).

instantConds([_:Cond|Conds]):- !,
   instantCond(Cond),
   instantConds(Conds).


/*========================================================================
   Instantiating DRS-Condition
========================================================================*/

instantCond(imp(A1,A2)):- !, instantDrs(A1), instantDrs(A2).

instantCond(or(A1,A2)):- !,  instantDrs(A1), instantDrs(A2).

instantCond(duplex(_,A1,_,A2)):- !, instantDrs(A1), instantDrs(A2).

instantCond(not(A)):- !, instantDrs(A).

instantCond(nec(A)):- !, instantDrs(A).

instantCond(pos(A)):- !, instantDrs(A).

instantCond(prop(_,A)):- !, instantDrs(A).

instantCond(_).


/*========================================================================
   Eliminate Equality from DRS 
========================================================================*/

eqDrs(xdrs(Tags,DRS1),xdrs(Tags,DRS2)):-
   option('--elimeq',true), !,
   elimEqDrs(DRS1,DRS2).

eqDrs(DRS1,DRS2):-
   option('--elimeq',true), !,
   elimEqDrs(DRS1,DRS2).

eqDrs(DRS,DRS).


/*========================================================================
   Eliminate Equality
========================================================================*/

elimEqDrs(Var,Var):- avar(Var), !.

elimEqDrs(B:drs(Dom1,Conds1),B:drs(Dom2,Conds2)):-
   elimEqConds(Conds1,Conds2,Dom1,Dom2).

elimEqDrs(merge(A1,A2),merge(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(sub(A1,A2),sub(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(sdrs([],C),sdrs([],C)).

elimEqDrs(sdrs([X1|L1],C1),sdrs([X2|L2],C2)):-
   elimEqDrs(X1,X2),
   elimEqDrs(sdrs(L1,C1),sdrs(L2,C2)).

elimEqDrs(alfa(T,A1,A2),alfa(T,B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(lab(X,A1),lab(X,B1)):-
   elimEqDrs(A1,B1).

elimEqDrs(lam(X,A1),lam(X,B1)):-
   elimEqDrs(A1,B1).

elimEqDrs(app(A1,A2),app(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).


/*========================================================================
   Instantiating DRS-Conditions
========================================================================*/

elimEqConds([],[],D,D).

elimEqConds([B:I:imp(A1,A2)|Conds1],[B:I:imp(B1,B2)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([B:I:or(A1,A2)|Conds1],[B:I:or(B1,B2)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([B:I:duplex(X,A1,T,A2)|Conds1],[B:I:duplex(X,B1,T,B2)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([B:I:not(A1)|Conds1],[B:I:not(B1)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([B:I:nec(A1)|Conds1],[B:I:nec(B1)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([B:I:pos(A1)|Conds1],[B:I:pos(B1)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([B:I:prop(X,A1)|Conds1],[B:I:prop(X,B1)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([_:_:eq(X,Y)|Conds1],Conds2,D1,D2):- 
   select(_:Z,D1,D3), X==Z, !, X=Y,
   elimEqConds(Conds1,Conds2,D3,D2).

elimEqConds([_:_:eq(X,Y)|Conds1],Conds2,D1,D2):- 
   select(_:Z,D1,D3), Y==Z, !, X=Y,
   elimEqConds(Conds1,Conds2,D3,D2).

elimEqConds([C|Conds1],[C|Conds2],D1,D2):- !,
   elimEqConds(Conds1,Conds2,D1,D2).
