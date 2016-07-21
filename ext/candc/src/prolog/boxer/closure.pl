
:- module(closure,[closure/3,closing/1,plosing/1]).

:- use_module(boxer(slashes)).
:- use_module(library(lists),[member/2]).
:- use_module(boxer(lexicon),[semlex/5]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).


/* =========================================================================
   Closing and propositonal closing (Plosing)
========================================================================= */

closing(lam(_,_:drs([],[]))).

plosing(lam(X,B:drs([],[B:[]:pred(X,closing,v,99)]))):- option('--semantics',amr), !.
plosing(lam(X,B:drs([],[B:[]:pred(X,closing,v,99)]))):- option('--semantics',tacitus), !.
plosing(lam(_,_:drs([],[]))).


/* -------------------------------------------------------------------------
   Closure
------------------------------------------------------------------------- */

closure(Cat,Sem,Closed):-
   member(Cat,[t:_, t]), !, 
   Closed = Sem.

closure(s:_,Sem,Closed):- !,
   plosing(CC),
   Closed = app(Sem,CC).

closure(Cat,Sem,Closed):-
   member(Cat,[s:b\np]), 
   option('--semantics',amr), !,
   Imp=lam(X,B:drs([],[B:[]:pred(X,closing,v,99),B:[]:pred(X,imperative,r,0)])),
   Closed = app(app(Sem,lam(P,merge(B:drs([B:[]:X],[B:[]:pred(X,you,n,1)]),app(P,X)))),Imp).

closure(Cat,Sem,Closed):-
   member(Cat,[s:_\np, s:_/np]), !,
   plosing(CC),
   Closed = app(app(Sem,lam(P,merge(B:drs([B:[]:X],[B:[]:pred(X,thing,n,12)]),app(P,X)))),CC).

closure(Cat,Sem,Closed):-
   member(Cat,[s:_/pp, s:_\pp]), !,
   plosing(CC),
   Closed = app(app(Sem,lam(X,B:drs([],[B:[]:pred(X,thing,n,12)]))),CC).

closure(Cat,Sem,Closed):-
   member(Cat,[s:_/s:_, s:_\s:_]), !,
   plosing(CC),
   Closed = app(app(Sem,lam(P,merge(B:drs([B:[]:X],[B:[]:pred(X,event,n,1)]),app(P,X)))),CC).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_/s:_)/(s:_/s:_), (s:_/s:_)\(s:_/s:_),
               (s:_\s:_)/(s:_\s:_), (s:_\s:_)\(s:_\s:_)]), !,
   plosing(CC),
   Closed = app(app(app(Sem,lam(S,lam(F,app(S,lam(E,app(F,E)))))),
                    lam(P,merge(B:drs([B:[]:X],[]),app(P,X)))),CC).

closure(Cat,Sem,Closed):-   
   option('--semantics',amr),
   member(Cat,[np, np:_]), !,
   Closed = app(Sem,lam(X,B:drs([],[B:[]:pred(X,closing,v,99)]))).

closure(Cat,Sem,Closed):-
   member(Cat,[np, np:_]), !,
   Closed = app(Sem,lam(X,B:drs([],[B:[]:pred(X,topic,a,1)]))).

closure(Cat,Sem,Closed):-
   option('--semantics',amr),
   member(Cat,[np:nb/n, np/n]), !, 
   Closed = app(app(Sem,lam(_,_:drs([],[]))),lam(X,B:drs([],[B:[]:pred(X,closing,v,99)]))).

closure(Cat,Sem,Closed):-
   member(Cat,[np:nb/n, np/n]), !, 
   Closed = app(app(Sem,lam(_,_:drs([],[]))),lam(X,B:drs([],[B:[]:pred(X,topic,a,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,[np\np, np/np]), !, 
   Closed = app(app(Sem,lam(P,merge(B1:drs([B1:[]:X],[B1:[]:pred(X,thing,n,12)]),app(P,X)))),
                lam(X,B2:drs([],[B2:[]:pred(X,thing,n,12)]))).

closure(Cat,Sem,Closed):-
   option('--semantics',amr),
   member(Cat,[n, pp]), !,
   Closed = merge(B:drs([B:[]:X],[B:[]:pred(X,closing,v,99)]),app(Sem,X)).

closure(Cat,Sem,Closed):-
   member(Cat,[n, pp]), !,
   Closed = merge(B:drs([B:[]:X],[B:[]:pred(X,topic,a,1)]),app(Sem,X)).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_\np)\((s:_\np)/np)]), !,
   semlex((s:dcl\np)/np,event,[],[]-_,TV),
   closure(s:dcl\np,app(Sem,TV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_\np)\((s:_\np)/pp)]), !,
   semlex((s:dcl\np)/pp,event,[],[]-_,TV),
   closure(s:dcl\np,app(Sem,TV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_\np)\(((s:_\np)/pp)/np)]), !,
   semlex(((s:dcl\np)/pp)/np,event,[],[]-_,DTV),
   closure(s:dcl\np,app(Sem,DTV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:dcl\np)/(s:b\np),
               (s:to\np)\(s:to\np),
               (s:b\np)\(s:b\np)]), !,
   semlex(s:b\np,event,[],[]-_,IV),
   closure(s:dcl\np,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_\np)\(s:_\np),
               (s:_\np)/(s:_\np),
               (s:dcl/np)\(s:dcl/np),
               (s:dcl\np)\(s:dcl\np)]), !,
   semlex(s:dcl\np,event,[],[]-_,IV),
   closure(s:dcl\np,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:dcl/(s:adj\np)]), !,
   semlex(s:adj\np,event,[],[]-_,IV),
   closure(s:dcl,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:_/(s:_\np)]), !,
   semlex(s:dcl\np,event,[],[]-_,IV),
   closure(s:dcl,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:wq/(s:q/np), s:wq/(s:adj\np)]), !,
   semlex(s:dcl\np,event,[],[]-_,IV),
   closure(s:wq,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:wq/(s:pss\np), s:q/(s:pss\np)]), !,
   semlex(s:pss\np,event,[],[]-_,IV),
   closure(s:wq,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:wq/(s:b\np),
               s:q/(s:b\np),
               s:q/(s:ng\np),
               s:qem/(s:dcl/np)]), !,
   semlex(s:dcl\np, event,[],[]-_,IV),
   closure(s:wq, app(Sem,IV),Closed).

closure(X,_,_):-
   warning('no closure operation for ~p',[X]), 
   fail.
