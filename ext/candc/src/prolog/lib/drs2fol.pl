
:- module(drs2fol,[drs2fol/2,symbol/4,timex/2]).

:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(library(lists),[append/3]).


/* ========================================================================
    Main Predicate
======================================================================== */

drs2fol(B,some(W,and(possible_world(W),F))):-
   option('--modal',true), !,
   drsfol(B,W,F).

drs2fol(B,F):-
   option('--modal',false), !,
   drsfol(B,F).


/* ========================================================================
   Translate DRSs into FOL formulas 
======================================================================== */

drsfol(lab(_,B),Form):- !, drsfol(B,Form).

drsfol(sub(B1,B2),Form):- !, drsfol(merge(B1,B2),Form).

drsfol(sdrs([B],_),Form):- !, drsfol(B,Form).

drsfol(sdrs([B|L],C),Form):- !, drsfol(merge(B,sdrs(L,C)),Form).
    
drsfol(alfa(_,B1,B2),Form):- !, drsfol(merge(B1,B2),Form).

drsfol(drs([],[Cond]),Formula):- !, cond2fol(Cond,Formula).

drsfol(drs([],[Cond1,Cond2|Conds]),and(Formula1,Formula2)):- !,
   cond2fol(Cond1,Formula1), drsfol(drs([],[Cond2|Conds]),Formula2).

drsfol(drs([Indexed|Referents],Conds),Formula):-
   nonvar(Indexed), !, Indexed=_:Var, 
   drsfol(drs([Var|Referents],Conds),Formula).

drsfol(drs([X|Referents],Conds),some(X,and(some(Y,member(Y,X)),Formula))):- 
   option('--plural',true), !,
   drsfol(drs(Referents,Conds),Formula).

drsfol(drs([X|Referents],Conds),some(X,Formula)):-
   option('--plural',false), !,
   drsfol(drs(Referents,Conds),Formula).

drsfol(merge(B1,B2),Formula):- !, 
   drsfolGap(B1,Gap^Formula), drsfol(B2,Gap).

drsfol(X,_):-
   warning('drsfol/2 failed for: ~p',[X]), fail.


/* ========================================================================
   Translate DRSs into FOL formulas (Modal translation)
======================================================================== */

drsfol(lab(_,B),W,Form):- !, drsfol(B,W,Form).

drsfol(sub(B1,B2),W,Form):- !, drsfol(merge(B1,B2),W,Form).

drsfol(sdrs([B],_),W,Form):- !, drsfol(B,W,Form).

drsfol(sdrs([B|L],C),W,Form):- !, drsfol(merge(B,sdrs(L,C)),W,Form).

drsfol(alfa(_,B1,B2),W,Form):- !, drsfol(merge(B1,B2),W,Form).

drsfol(drs([],[Cond]),W,Formula):- !, cond2fol(Cond,W,Formula).

drsfol(drs([],[Cond1,Cond2|Conds]),W,and(Formula1,Formula2)):- !,
   cond2fol(Cond1,W,Formula1), drsfol(drs([],[Cond2|Conds]),W,Formula2).

drsfol(drs([Indexed|Referents],Conds),W,Formula):-
   nonvar(Indexed), !, Indexed=_:Var, 
   drsfol(drs([Var|Referents],Conds),W,Formula).

drsfol(drs([X|Referents],Conds),W,some(X,Formula)):- !,
   drsfol(drs(Referents,Conds),W,Formula).

drsfol(merge(B1,B2),W,Form):- !, 
   drsfolGap(B1,W,Gap^Form), drsfol(B2,W,Gap).

drsfol(X,_,_):-
   warning('drsfol/3 failed for: ~p',[X]), fail.


/* ========================================================================
   Translate DRS into FOL formula with "gap"

   This is to ensure that discourse referents in the LHS of a merge
   bind occurrences of DRSs in the RHS of a merge. 
======================================================================== */

drsfolGap(sdrs([B],_),F):- !, drsfolGap(B,F).

drsfolGap(sdrs([B|L],C),F):- !, drsfolGap(merge(B,sdrs(L,C)),F).

drsfolGap(lab(_,B),F):- !, drsfolGap(B,F).

drsfolGap(sub(B1,B2),F):- !, drsfolGap(merge(B1,B2),F).

drsfolGap(alfa(_,B1,B2),Gap^F):- !, drsfolGap(merge(B1,B2),Gap^F).  

drsfolGap(merge(B1,B2),Gap2^F):- !, 
   drsfolGap(B1,Gap1^F), drsfolGap(B2,Gap2^Gap1).

drsfolGap(drs([Indexed|Refs],Conds),F):- 
   nonvar(Indexed), !, Indexed=_:Ref,
   drsfolGap(drs([Ref|Refs],Conds),F).

drsfolGap(drs([X],[]),Gap^some(X,and(some(Y,member(Y,X)),Gap))):-  
   option('--plural',true), !.

drsfolGap(drs([X|Dom],Conds),Gap^some(X,and(some(Y,member(Y,X)),F))):-  
   option('--plural',true), !,
   drsfolGap(drs(Dom,Conds),Gap^F).

drsfolGap(drs([X],[]),Gap^some(X,Gap)):-
   option('--plural',false), !.

drsfolGap(drs([X|Dom],Conds),Gap^some(X,F)):- 
   option('--plural',false), !,
   drsfolGap(drs(Dom,Conds),Gap^F).

drsfolGap(drs([],Conds),Gap^and(F,Gap)):- !,
   drsfol(drs([],Conds),F).

drsfolGap(X,_):-
   warning('drsfolGap/2 failed for: ~p',[X]), fail.


/* ========================================================================
   Translate DRS into FOL formula with "gap" (Modal translation)

   This is to ensure that discourse referents in the LHS of a merge
   bind occurrences of DRSs in the RHS of a merge. 
======================================================================== */

drsfolGap(sdrs([B],_),W,F):- !, drsfolGap(B,W,F).

drsfolGap(sdrs([B|L],C),W,F):- !, drsfolGap(merge(B,sdrs(L,C)),W,F).

drsfolGap(lab(_,B),W,F):- !, drsfolGap(B,W,F).

drsfolGap(sub(B1,B2),W,F):- !, drsfolGap(merge(B1,B2),W,F).

drsfolGap(alfa(_,B1,B2),W,Gap^F):- !, drsfolGap(merge(B1,B2),W,Gap^F).  

drsfolGap(merge(B1,B2),W,Gap2^F):- !, 
   drsfolGap(B1,W,Gap1^F), drsfolGap(B2,W,Gap2^Gap1).

drsfolGap(drs([Indexed|Referents],Conds),W,Formula):-
   nonvar(Indexed), !, Indexed=_:Var, 
   drsfolGap(drs([Var|Referents],Conds),W,Formula).

drsfolGap(drs([X],[]),_,Gap^some(X,Gap)):- !.

drsfolGap(drs([X|Dom],Conds),W,Gap^some(X,F)):- !, 
   drsfolGap(drs(Dom,Conds),W,Gap^F).

drsfolGap(drs([],Conds),W,Gap^and(F,Gap)):- !,
   drsfol(drs([],Conds),W,F).

drsfolGap(X,_,_):-
   warning('drsfolGap/3 failed for: ~p',[X]), fail.


/* ========================================================================
   Translate DRS-Conditions into FOL formulas 
======================================================================== */

cond2fol(_:C,F):- !,
   cond2fol(C,F).

cond2fol(not(Drs),not(Formula)):- !,
   drsfol(Drs,Formula).
 
cond2fol(prop(_,Drs),Formula):- !,
   drsfol(Drs,Formula).

cond2fol(or(Drs1,Drs2),or(Formula1,Formula2)):- !,
   drsfol(Drs1,Formula1),
   drsfol(Drs2,Formula2).

cond2fol(whq(Drs1,Drs2),F):- !, 
   cond2fol(imp(Drs1,Drs2),F).

cond2fol(duplex(most,Drs1,X,Drs2),and(F1,all(Z,imp(F3,F4)))):- !, 
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:not(drs([[]:Y],[[]:rel(Y,X,g,1)]))]))),F1),
   % most(P(x),Q(x)) if Ex P(x) & Q(x) & -Ey g(y,x) [at least one more P&Q than P&-Q] 
   drs2fol(merge(Drs1,drs([],[[]:eq(X,Z),[]:not(Drs2)])),F3),
   % if Ez p(z)&-q(z) then Ex p(x)&q(x) & g(z,x)
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:rel(Z,X,g,1)]))),F4).

cond2fol(duplex(two,Drs1,X,Drs2),some(A,some(B,and(not(eq(A,B)),and(all(Z,imp(F0,or(eq(Z,A),eq(Z,B)))),and(F1,F2)))))):- !, 
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:eq(X,A)]))),F1),
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:eq(X,B)]))),F2),
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:eq(X,Z)]))),F0).

cond2fol(duplex(three,Drs1,X,Drs2),some(A,some(B,some(C,and(not(eq(A,B)),and(not(eq(A,C)),and(not(eq(B,C)),and(all(Z,imp(F0,or(eq(Z,A),or(eq(Z,B),eq(Z,C))))),and(F1,and(F2,F3)))))))))):- !, 
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:eq(X,A)]))),F1),
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:eq(X,B)]))),F2),
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:eq(X,C)]))),F3),
   drs2fol(merge(Drs1,merge(Drs2,drs([],[[]:eq(X,Z)]))),F0).

cond2fol(duplex(_,Drs1,_,Drs2),F):- !, 
   cond2fol(imp(Drs1,Drs2),F).

cond2fol(imp(B1,B2),Formula):- !,
   cond2fol(not(merge(B1,drs([],[not(B2)]))),Formula).

cond2fol(card(X,1,eq),one(X)):- option('--plural',true), !.

cond2fol(card(X,2,eq),two(X)):- option('--plural',true), !.

cond2fol(card(X,3,eq),three(X)):- option('--plural',true), !.

cond2fol(card(X,C,_),some(Y,and(card(X,Y),and(F1,F2)))):-
   integer(C), C > 0,
   symbol(c,number,C,S1), 
   symbol(n,numeral,1,S2), !,
   F1=..[S1,Y],
   F2=..[S2,Y].

cond2fol(card(X,_,_),some(Y,and(card(X,Y),F2))):-
   symbol(n,numeral,1,S2), !,
   F2=..[S2,Y].

cond2fol(named(X,N1,Type,Sense),F):-
   symbol(Type,N1,Sense,N2), !,
   F=..[N2,X].

cond2fol(timex(X,D1),F):-
   timex(D1,D2),
   F=..[D2,X], !.

cond2fol(eq(X),eq(X,X)):- !.

cond2fol(eq(X,Y),eq(X,Y)):- !.

cond2fol(pred(X,Sym1,Type,Sense),all(Y,imp(member(Y,X),F))):- 
   option('--plural',true), 
   symbol(Type,Sym1,Sense,Sym2), !,
   F=..[Sym2,Y].

cond2fol(pred(X,Sym1,Type,Sense),F):- 
   option('--plural',false),
   symbol(Type,Sym1,Sense,Sym2), !,
   F=..[Sym2,X].

cond2fol(rel(X,Y,Sym1,Sense),all(A,imp(member(A,X),all(B,imp(member(B,Y),F))))):- 
   option('--plural',true), 
   symbol(r,Sym1,Sense,Sym2), !,
   F=..[Sym2,A,B].

cond2fol(rel(X,Y,Sym1,Sense),F):- 
   option('--plural',false), 
   symbol(r,Sym1,Sense,Sym2), !,
   F=..[Sym2,X,Y].

cond2fol(role(X,Y,Sym1,1),F):- 
   symbol(r,Sym1,1,Sym2), !,
   F=..[Sym2,X,Y].

cond2fol(role(X,Y,Sym1,-1),F):- 
   symbol(r,Sym1,1,Sym2), !,
   F=..[Sym2,Y,X].

cond2fol(X,_):-
   warning('cond2fol/2 failed for ~p',[X]), fail.


/*========================================================================
   Translate DRS-Conditions into FOL formulas (modal translation)
========================================================================*/

cond2fol(_:C,W,F):- !,
   cond2fol(C,W,F).

cond2fol(not(Drs),W,not(Formula)):- !,
   drsfol(Drs,W,Formula).

cond2fol(nec(Drs),_,all(V,imp(possible_world(V),Formula))):- !,
   drsfol(Drs,V,Formula).

cond2fol(pos(Drs),_,some(V,and(possible_world(V),Formula))):- !,
   drsfol(Drs,V,Formula).
 
cond2fol(prop(V,Drs),_,Formula):- !,
   drsfol(Drs,V,Formula).

cond2fol(or(Drs1,Drs2),W,or(Formula1,Formula2)):- !,
   drsfol(Drs1,W,Formula1),
   drsfol(Drs2,W,Formula2).

cond2fol(whq(Drs1,Drs2),W,F):- !, 
   cond2fol(imp(Drs1,Drs2),W,F).

cond2fol(duplex(_,Drs1,_,Drs2),W,F):- !, 
   cond2fol(imp(Drs1,Drs2),W,F).

cond2fol(imp(B1,B2),W,Formula):- !,
   cond2fol(not(merge(B1,drs([],[not(B2)]))),W,Formula).

cond2fol(card(X,C,_),W,some(Y,and(card(W,X,Y),and(F1,F2)))):-
   integer(C), C > 0,
   symbol(c,number,C,S1), 
   symbol(n,numeral,1,S2), !,
   F1=..[S1,W,Y],
   F2=..[S2,W,Y].

cond2fol(card(X,_,_),W,some(Y,and(card(W,X,Y),F2))):-
   symbol(n,numeral,1,S2), !,
   F2=..[S2,W,Y].

cond2fol(named(X,N1,Type,Sense),W,F):-
   symbol(Type,N1,Sense,N2), !,
   F=..[N2,W,X].

cond2fol(timex(X,D1),W,F):-
   timex(D1,D2),
   F=..[D2,W,X], !.

cond2fol(eq(X,Y),_,eq(X,Y)):- !.

cond2fol(eq(X),W,eq(W,X)):- !.

cond2fol(pred(X,Sym1,Type,Sense),W,F):- 
   symbol(Type,Sym1,Sense,Sym2), !,
   F=..[Sym2,W,X].

cond2fol(rel(X,Y,Sym1,Sense),W,F):- 
   symbol(r,Sym1,Sense,Sym2), !,
   F=..[Sym2,W,X,Y].

cond2fol(role(X,Y,Sym1,1),W,F):- 
   symbol(r,Sym1,1,Sym2), !,
   F=..[Sym2,W,X,Y].

cond2fol(role(X,Y,Sym1,-1),W,F):- 
   symbol(r,Sym1,1,Sym2), !,
   F=..[Sym2,W,Y,X].

cond2fol(X,_,_):-
   warning('cond2fol/2 failed for ~p',[X]), fail.


/* ========================================================================
   Normalising Symbols
======================================================================== */

symbol(t,D,_Sense,F):- !, timex(D,F).

symbol(Type,F1,0,F2):- !, symbol(Type,F1,1,F2).    % WSD (first sense)

symbol(Type,F1,Sense,F2):-
   atom_codes(Type,A0), 
   ( number(Sense), !, number_codes(Sense,A1); atom_codes(Sense,A1) ),
   append(A0,A1,A2),
   ( atom(F1),  !, atom_codes(F1,A3)   
   ; number(F1), number_codes(F1,A3) ),
   normSymbol(A3,A4),
   append(A2,A4,A5),
   maxLen(A5,A6),
   atom_codes(F2,A6).


/* ========================================================================
   Max Length Symbol (for mace, and other theorem provers)
======================================================================== */

maxLen(In,Out):-
   In =  [A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,B1,B2,B3,B4,B5,B6,B7,B8,B9,B0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C0,D1,_|_], !,
   Out = [A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,B1,B2,B3,B4,B5,B6,B7,B8,B9,B0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C0,D1],
   atom_codes(Symbol1,In),
   atom_codes(Symbol2,Out),
   warning('symbol ~p too long, cut to ~p',[Symbol1,Symbol2]).

maxLen(L,L).


/* ========================================================================
   Normalising Symbols
======================================================================== */

normSymbol([],[]):- !.

normSymbol([95|L1],[95|L2]):- !, normSymbol(L1,L2).

normSymbol([X|L1],[X|L2]):- X > 64, X < 91, !, normSymbol(L1,L2).

normSymbol([X|L1],[X|L2]):- X > 96, X < 123, !, normSymbol(L1,L2).

normSymbol([X|L1],[X|L2]):- X > 47, X < 58, !, normSymbol(L1,L2).

normSymbol([X|L1],L3):- 
   number_codes(X,Codes),  
   append([67|Codes],L2,L3),
   normSymbol(L1,L2).


/* ========================================================================
   Time Expressions
======================================================================== */

timex(date(_:_,_:Y,_:M,_:D),Timex):- !,
   timex(date(Y,M,D),Timex).

timex(date(_:Y,_:M,_:D),Timex):- !,
   timex(date(Y,M,D),Timex).

timex(time(_:H,_:M,_:S),Timex):- !,
   timex(time(H,M,S),Timex).

timex(date(Y,M,D),Timex):-
   year(Y,[Y1,Y2,Y3,Y4]),
   month(M,[M1,M2]),
   day(D,[D1,D2]),
   name(Timex,[116,95,Y1,Y2,Y3,Y4,M1,M2,D1,D2]).

timex(time(H,M,S),Timex):-
   hour(H,[H1,H2]),
   minute(M,[M1,M2]),
   second(S,[S1,S2]),
   name(Timex,[116,95,H1,H2,M1,M2,S1,S2]).


/* ========================================================================
   Time Expressions (year)
======================================================================== */

year(Y,C):- variable(Y), !, name('XXXX',C).
year(Y,C):- name(Y,C).


/* ========================================================================
   Time Expressions (month)
======================================================================== */

month(Y,C):- variable(Y), !, name('XX',C).
month(Y,C):- name(Y,C).


/* ========================================================================
   Time Expressions (day)
======================================================================== */

day(Y,C):- variable(Y), !, name('XX',C).
day(Y,C):- name(Y,C).


/* ========================================================================
   Time Expressions (other)
======================================================================== */

hour(A,C):- day(A,C).
minute(A,C):- day(A,C).
second(A,C):- day(A,C).


/* ========================================================================
   Variable
======================================================================== */

variable(X):- var(X), !.
variable(X):- functor(X,'$VAR',1), !.
