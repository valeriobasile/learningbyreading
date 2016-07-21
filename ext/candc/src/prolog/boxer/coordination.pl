
:- module(coordination,[coordMacro/2,argCard/2]).
:- use_module(boxer(slashes)).

/* =========================================================================
   Coordination Macros
========================================================================= */

coordMacro(N,lam(C,lam(X2,lam(X1,Body)))):-
   addLambdas(N,[],C,X2,X1,Body).


/* =========================================================================
   Adding right amount of lambdas
========================================================================= */

addLambdas(0,Vars,C,X,Y,lam(A,app(app(C,X1),Y1))):- !,
   addApplications([A|Vars],X,Y,X1,Y1).

addLambdas(N,Vars,C,X,Y,lam(A,Body)):-
   M is N - 1, addLambdas(M,[A|Vars],C,X,Y,Body).


/* =========================================================================
   Adding right amount of applications
========================================================================= */

addApplications([A],X,Y,app(X,A),app(Y,A)):- !.

addApplications([A|L],X,Y,app(X1,A),app(Y1,A)):-
   addApplications(L,X,Y,X1,Y1).


/* =========================================================================
   Argument Cardinality
========================================================================= */

argCard(_:_,C):- !, C = 0.
argCard(X/_,C):- !, argCard(X,N), C is N + 1.
argCard(X\_,C):- !, argCard(X,N), C is N + 1.
argCard(_,0).


