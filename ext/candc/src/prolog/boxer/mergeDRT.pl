
:- module(mergeDRT,[mergeDrs/2]).

:- use_module(library(lists),[member/2,append/3]).
:- use_module(boxer(noncomp),[noncomp/3]).


/* ========================================================================
   Merge reduction
======================================================================== */

mergeDrs(Var,Var):- var(Var), !.

mergeDrs(lab(K,B),lab(K,R)):- !,
   mergeDrs(B,R).

mergeDrs(sdrs([],C),sdrs([],C)):- !.

mergeDrs(sdrs([sub(B1,B2)|L1],C1),sdrs([sub(R1,R2)|L2],C2)):- !,
   mergeDrs(B1,R1),
   mergeDrs(B2,R2),
   mergeDrs(sdrs(L1,C1),sdrs(L2,C2)).

mergeDrs(sdrs([lab(K2,sdrs([sub(lab(K2,B2),B3)],C1))|L],C2),R):- !,
   append(C1,C2,C3),
   mergeDrs(sdrs([sub(lab(K2,B2),B3)|L],C3),R).

mergeDrs(sdrs([lab(K2,sdrs([lab(K2,B2)],C1))|L],C2),R):- !,
   append(C1,C2,C3),
   mergeDrs(sdrs([lab(K2,B2)|L],C3),R).

mergeDrs(sdrs([lab(K,B)|L1],C1),sdrs([lab(K,R)|L2],C2)):- !,
   mergeDrs(B,R),
   mergeDrs(sdrs(L1,C1),sdrs(L2,C2)).

mergeDrs(B,R):- 
   pnf(B,P), 
   reduceMerge(P,R).


/* ========================================================================
   Merge reduction
======================================================================== */

% Cannot reduce a variable
%
reduceMerge(Var,Var):- var(Var), !.

% Cannot reduce if one the arguments is a variable
%
reduceMerge(merge(Var,B),merge(Var,R)):- var(Var), !, reduceMerge(B,R). 
reduceMerge(merge(B,Var),merge(R,Var)):- var(Var), !, reduceMerge(B,R). 
reduceMerge(alfa(T,Var,B),alfa(T,Var,R)):- var(Var), !, reduceMerge(B,R). 
reduceMerge(alfa(T,B,Var),alfa(T,R,Var)):- var(Var), !, reduceMerge(B,R). 

% Reduce if both are basic DRSs
%
reduceMerge(merge(B:B1,B:B2),B:B3):- !,
   merge(B1,B2,B3).

reduceMerge(alfa(_,_:B1,B:B2),B:B3):-  !,
   merge(B1,B2,B3).

% Reduce if basic DRS and SDRS
%
reduceMerge(merge(K1:B1,sdrs([lab(K,B2)|L],R)),SDRS):- !,
   mergeDrs(sdrs([lab(K,merge(K1:B1,B2))|L],R),SDRS).

reduceMerge(merge(K1:B1,sdrs([sub(lab(K,B2),B3)|L],R)),SDRS):- !,
   mergeDrs(sdrs([sub(lab(K,merge(K1:B1,B2)),B3)|L],R),SDRS).

reduceMerge(merge(sdrs([lab(K,K1)],R),B2),SDRS):- !,
   mergeDrs(sdrs([lab(K,merge(K1,B2))],R),SDRS).

reduceMerge(merge(sdrs([sub(lab(K,K1),B3)],R),B2),SDRS):- !,
   mergeDrs(sdrs([sub(lab(K,merge(K1,B2)),B3)],R),SDRS).

reduceMerge(merge(sdrs([X|L1],R1),B),sdrs([X|L2],R2)):- !,
   reduceMerge(merge(sdrs(L1,R1),B),sdrs(L2,R2)).

reduceMerge(alfa(T,K1:B1,sdrs([lab(K,B2)|L],R)),SDRS):- !,
   mergeDrs(sdrs([lab(K,alfa(T,K1:B1,B2))|L],R),SDRS).

reduceMerge(alfa(T,K1:B1,sdrs([sub(lab(K,B2),B3)|L],R)),SDRS):- !,
   mergeDrs(sdrs([sub(lab(K,alfa(T,K1:B1,B2)),B3)|L],R),SDRS).

reduceMerge(alfa(T,sdrs([lab(K,K1:B1)],R),B2),SDRS):- !,
   mergeDrs(sdrs([lab(K,alfa(T,K1:B1,B2))],R),SDRS).

reduceMerge(alfa(T,sdrs([sub(lab(K,K1),B3)],R),B2),SDRS):- !,
   mergeDrs(sdrs([sub(lab(K,alfa(T,K1,B2)),B3)],R),SDRS).

% Recursive case 
%
reduceMerge(merge(B:B1,merge(B:B2,K)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(merge(B:B3,K),Reduced).

reduceMerge(alfa(_,_:B1,merge(B:B2,K)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(merge(B:B3,K),Reduced).

reduceMerge(merge(B:B1,alfa(_,_:B2,K)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(merge(B:B3,K),Reduced).

reduceMerge(alfa(_,_:B1,alfa(T,B:B2,K)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(alfa(T,B:B3,K),Reduced).

reduceMerge(merge(B:B1,merge(sdrs([lab(K,B:B2)|L],R),M)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(merge(sdrs([lab(K,B:B3)|L],R),M),Reduced).

reduceMerge(alfa(_,_:B1,merge(sdrs([lab(K,B:B2)|L],R),M)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(merge(sdrs([lab(K,B:B3)|L],R),M),Reduced).

reduceMerge(alfa(_,_:B1,alfa(T,sdrs([lab(K,B:B2)|L],R),M)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(alfa(T,sdrs([lab(K,B:B3)|L],R),M),Reduced).

reduceMerge(merge(B:B1,merge(sdrs([sub(lab(K,B:B2),B4)|L],R),M)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(merge(sdrs([sub(lab(K,B:B3),B4)|L],R),M),Reduced).

reduceMerge(alfa(_,_:B1,merge(sdrs([sub(lab(K,B:B2),B4)|L],R),M)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(merge(sdrs([sub(lab(K,B:B3),B4)|L],R),M),Reduced).

reduceMerge(alfa(_,_:B1,alfa(T,sdrs([sub(lab(K,B:B2),B4)|L],R),M)),Reduced):- !,
   merge(B1,B2,B3),
   reduceMerge(alfa(T,sdrs([sub(lab(K,B:B3),B4)|L],R),M),Reduced).

reduceMerge(B:drs(D,C1),B:drs(D,C2)):- !, mergeConds(C1,C2).

reduceMerge(M,M).


/* ========================================================================
   Projection Normal Form
======================================================================== */

pnf(Var,Var):- var(Var), !.
pnf(alfa(T1,alfa(T2,B1,B2),B3),alfa(T2,N1,N2)):- !, pnf(B1,N1), pnf(alfa(T1,B2,B3),N2).
pnf(merge(Var,B),merge(Var,B)):- var(Var), !.
pnf(alfa(T,Var,B),alfa(T,Var,B)):- var(Var), !.
pnf(merge(merge(B1,B2),B3),merge(N1,N2)):- !, pnf(B1,N1), pnf(merge(B2,B3),N2).
pnf(merge(alfa(T,B1,B2),B3),alfa(T,N1,N2)):- !, pnf(B1,N1), pnf(merge(B2,B3),N2).
pnf(merge(B1,B2),merge(B1,R2)):- !, pnf(B2,R2).
pnf(alfa(T,B1,B2),alfa(T,R1,R2)):- !, pnf(B1,R1), pnf(B2,R2).
pnf(B,B).


/*========================================================================
   DRS-merge (Conditions)
========================================================================*/

mergeConds([B:F:Cond1|C1],[B:F:Cond2|C2]):- !,
   mergeConds([Cond1|C1],[Cond2|C2]).

mergeConds([F:Cond1|C1],[F:Cond2|C2]):- !,
   mergeConds([Cond1|C1],[Cond2|C2]).

mergeConds([imp(B1,B2)|C1],[imp(B3,B4)|C2]):- !,
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeConds(C1,C2).

mergeConds([duplex(Type,B1,Var,B2)|C1],[duplex(Type,B3,Var,B4)|C2]):- !,
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeConds(C1,C2).

mergeConds([or(B1,B2)|C1],[or(B3,B4)|C2]):- !,
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeConds(C1,C2).

mergeConds([not(B1)|C1],[not(B2)|C2]):- !,
   mergeDrs(B1,B2),
   mergeConds(C1,C2).

mergeConds([nec(B1)|C1],[nec(B2)|C2]):- !,
   mergeDrs(B1,B2),
   mergeConds(C1,C2).

mergeConds([pos(B1)|C1],[pos(B2)|C2]):- !,
   mergeDrs(B1,B2),
   mergeConds(C1,C2).

mergeConds([prop(X,B1)|C1],[prop(X,B2)|C2]):- !,
   mergeDrs(B1,B2),
   mergeConds(C1,C2).

mergeConds([pred(A,B,C,D)|C1],[pred(A,B,C,D)|C2]):- !,
   mergeConds(C1,C2).

mergeConds([rel(A,B,C,D)|C1],[rel(A,B,C,D)|C2]):- !,
   mergeConds(C1,C2).

mergeConds([role(A,B,C,D)|C1],[role(A,B,C,D)|C2]):- !,
   mergeConds(C1,C2).

mergeConds([card(A,S,T)|C1],[card(A,S,T)|C2]):- !,
   mergeConds(C1,C2).

mergeConds([named(A,S,T,U)|C1],[named(A,S,T,U)|C2]):- !,
   mergeConds(C1,C2).

mergeConds([timex(A,S)|C1],[timex(A,S)|C2]):- !,
   mergeConds(C1,C2).

mergeConds([eq(X,Y)|C1],[eq(X,Y)|C2]):- !,
   mergeConds(C1,C2).

mergeConds([],[]).


/* ========================================================================
   Merge
======================================================================== */

merge(drs([],[C1]),drs(D,[C2|L]),drs(D,[C3|L])):- noncomp(C1,C2,C3), !.
merge(drs(D1,C1),drs(D2,C2),drs(D3,C3)):- !, merge(D1,D2,D3), merge(C1,C2,C3).
merge([],L,L):- !.
merge([X|L1],L2,L3):- member(Y,L2), X==Y, !, merge(L1,L2,L3).
merge([X|L1],L2,[X|L3]):- merge(L1,L2,L3).

