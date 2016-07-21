
:- module(relation,[resolve_relations/2]).

:- use_module(library(lists),[member/2]).
:- use_module(boxer(slashes)).
:- use_module(knowledge(relations),[nn/3]).
:- use_module(semlib(options),[option/2]).

resolve_relations(X,Y):-
   option('--nn',false), !, X=Y.

resolve_relations(X,Y):-
   rel(X,Y,[],Ts), 
   resolve(Ts).


rel(X,Y,T,T):- var(X), !, X=Y.

rel(X1,X2,T,[C1:C2|T]):-
   C1 = [_,_,_,_,_],
   X1 =.. [t|C1], !, 
   C2 = [_,_,_,_,_],
   X2 =.. [t|C2].

rel(X1,X2,T1,T2):-
   X1 =.. [F|L1],
   rels(L1,L2,T1,T2),
   X2 =.. [F|L2].


rels([],[],T,T).
rels([X1|L1],[X2|L2],T1,T3):-
   rel(X1,X2,T2,T3), rels(L1,L2,T1,T2).


resolve([]).

resolve([[C,W,S,A,I]:[C,W,S,A,I]|L]):- member(relation:_,A), !, resolve(L).

resolve([C1:C2,N1:N2|L]):- 
   C1 = [n/n,W,S,A,I], 
   member(lemma:S1,A), member(pos:'NN',A),
   N1 = [_,_,_,B,_], 
   member(lemma:S2,B), member(pos:'NN',B),
   n_n(S1,S2,Rel), !,
   C2 = [n/n,W,S,[relation:Rel|A],I],
   resolve([N1:N2|L]).

resolve([[n/n,W,S,A,I]:[n/n,W,S,[relation:of|A],I]|L]):- member(pos:'NN',A), !, resolve(L).
resolve([[n/n,W,S,A,I]:[n/n,W,S,[relation:'='|A],I]|L]):- member(pos:'NNP',A), !, resolve(L).
resolve([X:X|L]):- resolve(L).


/*========================================================================
  Function Interpretation: Noun-noun compounds


interpretFunction(f(name,[_,B1],Sym),Sym):-
   etaConversion(B1,B2), !,
   ( nn(_,B2,Sym), !
   ; Sym = '=' ).

interpretFunction(F,F).
========================================================================*/

n_n(A,B,Sym):- nn(A,B,Sym), !.
n_n(_,B,Sym):- nn(_,B,Sym), !.
n_n(A,_,Sym):- nn(A,_,Sym), !.


