
:- module(typechange,[typechange/5  % +OldCat,+OldSem,+Att,+NewCat,-NewSem
                     ]).

:- use_module(boxer(slashes)).
:- use_module(boxer(lexicon),[semlex/5]).
:- use_module(boxer(categories),[att/3]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(library(lists),[member/2]).


/* -------------------------------------------------------------------------

   This file contains the rules for the phenomenon called 'type
   changing' (not to be confused with type raising). These are called
   "lexical rules" in CCGbank. They are unary type changing rules that
   change the type of the category, or derived from binary type
   changing rules represented as unary type changing rule.  As they
   are not regular, like type shifting rules, it is impossible to give
   a general semantic pattern for them. Instead each pair of
   categories (consisting of the old and the new type of the category,
   will get its own semantic interpretation. Often, a type-changing
   rule corresponds to ellipsis. In the comments below, the elided
   phrases are indicated by square brackets.

------------------------------------------------------------------------- */

% Ex: a company [that is] based in ...
%
typechange(Old,Phi,_,New,app(Psi,Phi)):- 
   member(Old,[s:_\np,s:_/np]), 
   New = np\np, 
   semlex(New/Old,that,[],[]-_,Psi), !.

% Ex: an effort [ ... ] to end the violence
%
typechange(Old,Phi,_,New,app(Psi,Phi)):- 
   member(Old,[s:_\np,s:_/np]), 
   member(New,[n/n,n\n]), 
   semlex(New/Old,that,[],[]-_,Psi), !.

% Ex: a sign [that] the effort is working 
% 
typechange(Old,Phi,_,New,app(Psi,Phi)):- 
   Old = s:_,
   New = np\np, 
   semlex(New/Old,that,[],[]-_,Psi), !.

% Ex: sign [that] the effort is working 
%
typechange(Old,Phi,_,New,app(Psi,Phi)):- 
   Old = s:_,
   New = n\n, 
   semlex(New/Old,that,[],[]-_,Psi), !.

% <example missing>
%
typechange(Old,Phi,_,New,app(Psi,Phi)):-
   Old = (s:_\np)/np, 
   member(New,[np/np,np\np]),
   semlex(New/Old,empty,[],[]-_,Psi), !.

% Ex: walking [in order] to get fit
%
typechange(Old,Phi,_,New,app(Psi,Phi)):- 
   Old = s:_\np,
   member(New,[(s:X\np)\(s:X\np), (s:X\np)/(s:X\np)]),
   semlex(New/Old,for,[],[]-_,Psi), !.

% Ex: [the] man
%
typechange(Old,Phi,_,New,app(Psi,Phi)):-
   Old = n, New = pn,                     %%% preserved for Kilian
   semlex(New/Old,the,[],[]-_,Psi), !.

% Ex: [the] men
%
typechange(Old,Phi,Att,New,app(Psi,Phi)):-
   Old = n, New = np,
   att(Att,pos,POS), 
   member(POS,['NNP','NNPS']), 
   semlex(New/Old,the,[],[]-_,Psi), !.

% Ex: [some] men
%
typechange(Old,Phi,_,New,app(Psi,Phi)):-
   Old = n, New = np,
   semlex(New/Old,some,[],[]-_,Psi), !.

% Ex: there is hope [and] the rain will end
%
typechange(Old,Phi,_,New,app(Psi,Phi)):-
   Old = s:_, 
   member(New,[s:X\s:X,s:X/s:X]),
   semlex(New/Old,and,[],[]-_,Psi), !.

% Ex: (they say) [the event of] running a marathon (helps)
%
typechange(Old,Phi,_,New,app(Psi,Phi)):-
   Old = (s:ng\np),
   New = np, !, 
   Psi = lam(VP,lam(F,app(app(VP,lam(P,merge(B:drs([B:[]:X],[B:[]:pred(X,thing,n,12)]),
                                             app(P,X)))),lam(E,app(F,E))))).

% Ex: [while] regarded as the winner John lost the game
%
typechange(Old,Phi,_,New,app(Psi,Phi)):-
   Old = s:_\np, 
   member(New,[s:X/s:X,s:X\s:X]),
   semlex(New/Old,while,[],[]-_,Psi), !.

% General rule, could be further instantiated (rel)
%
typechange(Old,Phi,_,New,app(Psi,Phi)):-
   semlex(New/Old,rel,[],[]-_,Psi), !,
   warning('type changing for ~p',[New/Old]).

%typechange(np,Phi,_, (s:X/s:X),app(New,Phi)):- !,
%   New = lam(Old,lam(S,lam(E,app(S,lam(X,merge(app(Old,lam(Y,B:drs([],[B:[]:rel(X,Y,rel,0)]))),app(E,X))))))).

%typechange(np,Phi,_,(s:X\np)\(s:X\np),app(New,Phi)):- !,
%   New = lam(Old,lam(V,lam(N,lam(E,app(app(V,N),lam(X,merge(app(Old,lam(Y,B:drs([],[B:[]:rel(X,Y,rel,0)]))),app(E,X)))))))).

%typechange((s:dcl/s:dcl),Old,_,Mod,app(New,Old)):- 
%   member(Mod,[(s:X\np)\(s:X\np),(s:X\np)/(s:X\np)]), !,
%   New = lam(Old,lam(V,lam(N,app(Old,app(V,N))))).


/* -------------------------------------------------------------------------
   Type Change (old rules, might need revision)
------------------------------------------------------------------------- */

typechange((s:dcl/s:dcl),Sem,_, (s:X/s:X),Sem):- !.
typechange((s:dcl\s:dcl),Sem,_, (s:X/s:X),Sem):- !.
typechange((s:dcl/s:dcl),Sem,_, (s:X\s:X),Sem):- !.
typechange((s:dcl\s:dcl),Sem,_, (s:X\s:X),Sem):- !.

% not used anymore?
%
typechange(np,Phi,_, (np/np),app(New,Phi)):- !,
   New = lam(Old,lam(M,lam(P,app(Old,lam(X,app(M,lam(Y,merge(B:drs([],[B:[]:eq(X,Y)]),app(P,X))))))))).

typechange(np,Old,_,np/(np\np),app(New,Old)):- !,
   New = lam(Old,lam(M,app(M,Old))).


/* -------------------------------------------------------------------------
   Print warning message if no type-changing rules are found
------------------------------------------------------------------------- */

typechange(Cat1,_,_,Cat2,_):-
   warning('no type changing rule for ~p --> ~p',[Cat1,Cat2]),
   !, fail.
