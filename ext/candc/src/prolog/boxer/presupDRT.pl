
:- module(presupDRT,[resolveDrs/2,resolveDrs/3]).

:- use_module(boxer(mergeDRT),[mergeDrs/2]).
:- use_module(boxer(bindingViolation),[bindingViolationDrs/1]).
:- use_module(boxer(freeVarCheck),[freeVarCheckDrs/1]).
:- use_module(boxer(sortalCheck),[sortalCheckDrs/2]).
:- use_module(library(lists),[member/2,append/3,select/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2]).

/* ========================================================================
   Main predicate: resolveDrs/2
======================================================================== */

resolveDrs(B1,B2):- 
   resolveDrs(B1,B2,_).


/* ========================================================================
   Main predicate: resolveDrs/3 (DRS)
======================================================================== */

resolveDrs(alfa(top,B1,B2),RDRS,Links):- 
   option('--theory',drt), !,
   resolveDrs(alfa(top,B1,B2),RDRS,[],Links).

resolveDrs(smerge(alfa(top,B1,B2),B3),RDRS,Links):-
   option('--theory',drt), !,
   resolveDrs(alfa(top,B1,smerge(B2,B3)),RDRS,[],Links).

resolveDrs(B,RDRS,Links):- 
   option('--theory',drt), !,
   resolveDrs(alfa(top,drs([],[]),B),RDRS,[],Links).


/* ========================================================================
   Main predicate: resolveDrs/3 (SDRS)
======================================================================== */

resolveDrs(B,SDRS,Links):- 
   option('--theory',sdrt), 
   \+ B = sdrs(_,_), !,
   resolveDrs(sdrs([sub(lab(K1,drs([],[])),lab(K2,B))],[[]:rel(K1,K2,presupposition)]),SDRS,[],Links).

resolveDrs(B,SDRS,Links):- 
   option('--theory',sdrt), 
   B = sdrs(_,Rel), \+ member(_:rel(_,_,presupposition),Rel), !, 
   resolveDrs(sdrs([sub(lab(K1,drs([],[])),lab(K2,B))],[[]:rel(K1,K2,presupposition)]),SDRS,[],Links).

resolveDrs(B,SDRS,Links):- 
   option('--theory',sdrt), 
   resolveDrs(B,SDRS,[],Links).


/* ========================================================================
   Resolution

   Resolves alpha-DRSs one by one until they are all resolved. The variable 
   "Alfa" contains the anaphoric DRS, the variable "Type" the corresponding 
   alpha-type.  The third and fourth argument of resolveDrs/4 represent the 
   binding links. "Ac" holds a list of accommodation sites (represented as 
   a/1 terms), and "Bi" a list of binding sites (represented as r/2 terms).

======================================================================== */

resolveDrs(ADRS,DRS,L1,L3):-
   findAlfaDrs(ADRS,RDRS,alfa(Type,Alfa),Ac,[]-Bi), !,
   resolveAlfa(Alfa,Type,Ac,Bi,RDRS,L1,L2), 
   !, %%% Required for large DRSs (too many choice points)
   resolveDrs(RDRS,DRS,L2,L3).

resolveDrs(RDRS,DRS,L0,L0):-
   mergeDrs(RDRS,DRS), !.


/* ========================================================================
   Find First Alfa-DRS (DRSs)
======================================================================== */

findAlfaDrs(alfa(top,B1,B3),Res,Alfa,Ac,Bi1-Bi2):- !,
   mergeDrs(B1,M1),
   findAlfaDrs(B3,R2,Alfa,Ac0,[r(M1,R1)|Bi1]-Bi2),
   Res = alfa(top,merge(R1,A),R2),
   Ac = [a(A)|Ac0].

findAlfaDrs(alfa(T,B1,B2),Res,Alfa,Ac,Bi1-Bi2):- !,
   (  
      findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !,
      Res=alfa(T,R1,B2)
   ;
      Res=merge(A,B2),
      mergeDrs(B1,M1),
      Alfa=alfa(T,M1),
      Ac=[a(A)],
      Bi1=Bi2
   ).

findAlfaDrs(merge(B1,B2),Res,Alfa,Ac,Bi1-Bi2):- !,
   (  
      findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !,
      Res = merge(R1,B2)
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B2,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2),
      Res = merge(R1,R2)
   ). 

findAlfaDrs(smerge(B1,B2),Res,Alfa,Ac,Bi1-Bi2):- !,
   (  
      findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !,
      Res = smerge(R1,B2)
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B2,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2),
      Res = smerge(R1,R2)
   ). 

findAlfaDrs([sub(lab(K1,B1),lab(K2,B2))|C1],Res,Alfa,Ac,Bi1-Bi2):- !,
   (  
      findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !,
      Res = [sub(lab(K1,R1),lab(K2,B2))|C1]
   ;
      mergeDrs(B1,M1),
      (
         findAlfaDrs(B2,R2,Alfa,Ac0,[r(M1,R1)|Bi1]-Bi2), !,
         Res = [sub(lab(K1,merge(R1,A)),lab(K2,R2))|C1],
         Ac = [a(A)|Ac0]
      ;
         findAlfaDrs(C1,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2),
         Res = [sub(lab(K1,R1),lab(K2,B2))|R2]
      )
   ). 

findAlfaDrs([lab(K,B1)|C1],Res,Alfa,Ac,Bi1-Bi2):- !,
   (  
      findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !,
      Res = [lab(K,R1)|C1]
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(C1,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2),
      Res = [lab(K,R1)|R2]
   ). 

findAlfaDrs(drs(D,C1),merge(A,R),Alfa,[a(A)|Ac],Bi1-Bi2):- !,
   findAlfaConds(C1,C2,Alfa,Ac,[r(drs(D,C2),R)|Bi1]-Bi2).

findAlfaDrs(sdrs(D1,C),sdrs(D2,C),Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(D1,D2,Alfa,Ac,Bi1-Bi2).


/* ========================================================================
   Find First Alfa-DRS (DRS-Conditions)
======================================================================== */

findAlfaConds([I:X1|C1],[I:X2|C2],Alfa,Ac,Bi1-Bi2):- !,
   findAlfaConds([X1|C1],[X2|C2],Alfa,Ac,Bi1-Bi2).

findAlfaConds([imp(B1,B3)|C],Res,Alfa,Ac,Bi1-Bi2):- 
   (
      findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !,
      Res = [imp(B2,B3)|C]
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B3,R2,Alfa,Ac0,[r(M1,R1)|Bi1]-Bi2),
      Res = [imp(merge(R1,A),R2)|C],
      Ac = [a(A)|Ac0]
   ), !.

findAlfaConds([duplex(Type,B1,Var,B3)|C],Res,Alfa,Ac,Bi1-Bi2):- 
   (
      findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !,
      Res = [duplex(Type,B2,Var,B3)|C]
   ;
      mergeDrs(B1,M1),
      findAlfaDrs(B3,R2,Alfa,Ac0,[r(M1,R1)|Bi1]-Bi2),
      Ac = [a(A)|Ac0],
      Res = [duplex(Type,merge(R1,A),Var,R2)|C]
   ), !.      

findAlfaConds([or(B1,B2)|C],Res,Alfa,Ac,Bi1-Bi2):-
   (
      findAlfaDrs(B1,B3,Alfa,Ac,Bi1-Bi2), !,
      Res = [or(B3,B2)|C]
   ;
      findAlfaDrs(B2,B3,Alfa,Ac,Bi1-Bi2), 
      Res = [or(B1,B3)|C]
   ), !.

findAlfaConds([not(B1)|C],[not(B2)|C],Alfa,Ac,Bi1-Bi2):- 
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([nec(B1)|C],[nec(B2)|C],Alfa,Ac,Bi1-Bi2):- 
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([pos(B1)|C],[pos(B2)|C],Alfa,Ac,Bi1-Bi2):- 
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([prop(X,B1)|C],[prop(X,B2)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([Cond|C1],[Cond|C2],Alfa,Ac,Bi1-Bi2):-
   findAlfaConds(C1,C2,Alfa,Ac,Bi1-Bi2).


/* ========================================================================
   Resolve alfa: binding or accommodation
======================================================================== */

resolveAlfa(Alfa,Type,Ac,Bi,B,L1,L2):-
   option('--presup',max),
   bindAlfa(Type,Bi,Alfa,L1,L2),
   dontResolve(Ac),
   \+ bindingViolationDrs(B),
   freeVarCheckDrs(B), !.

resolveAlfa(Alfa,Type,Ac,Bi,B,L0,L0):-
   accommodateAlfa(Type,Ac,Alfa),
   dontResolve(Bi),
   freeVarCheckDrs(B).


/* ------------------------------------------------------------------------
   Typology:   atype(Type, Global, Local, Binding)
------------------------------------------------------------------------ */

atype(def,1,1,1). % definite descriptions
atype(nam,1,0,1). % proper names
atype(pro,1,0,1). % personal pronouns
atype(dei,1,0,1). % anaphoric tense
atype(ref,1,0,1). % reflexive pronouns
atype(fac,1,1,0). % factives, clefts, "only"
atype(ind,0,1,0). % indefinites


/* ------------------------------------------------------------------------
   Binding: select an antecedent, then merge the domain and the conditions
------------------------------------------------------------------------ */

bindAlfa(Type,[a(drs([],[]))|P],Alfa,L1,L2):- 
   !,  %%% cannot bind here, so try next level of DRS
   bindAlfa(Type,P,Alfa,L1,L2).

bindAlfa(_,[r(drs(D2,C2),DRS)|P],drs([AnaIndex:X|D1],C1),L,[bind(AnaIndex,AntIndex)|L]):-
   input:coref(Ana,Ant), 
   common(AnaIndex,Ana),                         % there is external coref info for this anaphor
   member(AntIndex:X,D2),                        % select candidate antecedent discourse referent
   common(AntIndex,Ant), !,
   warning('using external coref info for anaphoric expression ~p',[AnaIndex]),
   mergeDomains(D1,D2,D3), 
   mergeConditions(C1,C2,C3),
   DRS = drs(D3,C3),
   dontResolve(P).

bindAlfa(Type,[r(drs(D2,C2),DRS)|P],drs([AnaIndex:X|D1],C1),L,[bind(AnaIndex,AntIndex)|L]):-
   \+ option('--semantics',drg),
   atype(Type,_,_,1),                            % check whether type permits binding
   member(AntIndex:X,D2),                        % select candidate antecedent discourse referent
   match(Type,C1,C2,X),                          % check if this candidate matches
   coordinated(AnaIndex,AntIndex,L),
   mergeDomains(D1,D2,D3), 
   mergeConditions(C1,C2,C3),
   DRS = drs(D3,C3),
   sortalCheckDrs(DRS,X),
   dontResolve(P).

bindAlfa(Type,[r(drs(D2,C2),DRS)|P],drs([AnaIndex:X|D1],C1),L,[bind(AnaIndex,AntIndex)|L]):-
   option('--semantics',drg),
   atype(Type,_,_,1),                            % check whether type permits binding
   member(AntIndex:Y,D2),                        % select candidate antecedent discourse referent
   coordinated(AnaIndex,AntIndex,L),             % copied material must have same antecedent
   \+ \+ (Y=X, match(Type,C1,C2,X)),             % check if this candidate matches
   mergeDomains(D1,D2,D3), 
   mergeConditions(C1,C2,C3),
   DRS = drs([AnaIndex:X|D3],[[]:eq(X,Y)|C3]),
   \+ \+ (Y=X, sortalCheckDrs(drs(D3,C3),X)),
   dontResolve(P).

bindAlfa(Type,[r(R,R)|P],Alfa,L1,L2):-
   bindAlfa(Type,P,Alfa,L1,L2).


/* ------------------------------------------------------------------------
   Check if two indices share a common element
------------------------------------------------------------------------ */

common(Index1,Index2):- member(X,Index1), member(X,Index2), !.


/* ------------------------------------------------------------------------
   Check if coordinated items are resolved to the same antecedent 
------------------------------------------------------------------------ */

coordinated(AnaIndex,AntIndex,Bound):-
   \+ ( member(bind(AnaIndex,OtherIndex),Bound),
        \+ AnaIndex=[], \+ OtherIndex=[],
        \+ OtherIndex = AntIndex ).


/*------------------------------------------------------------------------
   Check for partial match 
------------------------------------------------------------------------*/

match(nam,C1,C2,X0):-
   member(_:named(X1,Sym,Type,Sense),C1), X0==X1, \+ Type=ttl,
   member(_:named(X2,Sym,Type,Sense),C2), X1==X2, !.

match(nam,C1,C2,X0):-
   member(_:timex(X1,date(_:D1,_:D2,_:D3,_:D4)),C1), X0==X1, 
   member(_:timex(X2,date(_:D1,_:D2,_:D3,_:D4)),C2), X1==X2, !.

match(def,C1,C2,X0):-
   member(_:pred(X1,Sym,n,Sense),C1), X0==X1,
   member(_:pred(X2,Sym,n,Sense),C2), X1==X2, !.

match(def,C1,C2,X0):-
   member(_:named(X1,Sym,Type,Sense),C1), X0==X1, \+ Type=ttl,
   member(_:named(X2,Sym,Type,Sense),C2), X1==X2, !.

match(def,C1,C2,X0):-
   member(_:timex(X1,date(_:D1,_:D2,_:D3,_:D4)),C1), X0==X1, 
   member(_:timex(X2,date(_:D1,_:D2,_:D3,_:D4)),C2), X1==X2, !.

match(dei,C1,C2,X0):-
   member(_:pred(X1,Sym,a,_),C1), X0==X1, 
   member(_:pred(X2,Sym,a,_),C2), X1==X2, !.

match(pro,C1,C2,X0):-
   member(_:pred(X1,Sym,a,_),C1), X0==X1, 
   member(_:pred(X2,Sym,a,_),C2), X1==X2, !.

match(pro,C1,C2,X0):-
   member(_:pred(X1,male,n,2),C1), X0==X1, 
   member(_:named(X2,_,per,_),C2), X1==X2, !.

match(pro,C1,C2,X0):-
   member(_:pred(X1,female,n,2),C1), X0==X1, 
   member(_:named(X2,_,per,_),C2),   X1==X2, !.

match(pro,C1,C2,X0):-
   member(_:pred(X1,neuter,a,_),C1), X0==X1, 
   ( NE=org ; NE=loc; NE=art; NE=nat ), 
   member(_:named(X2,_,NE,_),C2),    X1==X2, !.


/* ------------------------------------------------------------------------
   Forced local accommodation (option 'presup --min')
------------------------------------------------------------------------ */

accommodateAlfa(_,[a(Alfa)],Alfa):- 
   option('--presup',min), !. %%% force local accommodation

accommodateAlfa(Type,[a(drs([],[]))|P],Alfa):- 
   option('--presup',min), !, %%% force local accommodation
   accommodateAlfa(Type,P,Alfa).


/* ------------------------------------------------------------------------
   Accommodation

   P is a list of accommodation sites (represented as a/1 terms). The
   order of the list corresponds to the level of the accommodation
   site: the first a/1 term is the most global site, the last element
   the most local accommodation site. 
------------------------------------------------------------------------ */

accommodateAlfa(Type,[r(R,R)|P],Alfa):- !,
   accommodateAlfa(Type,P,Alfa).

accommodateAlfa(Type,[a(B)|P],Alfa):- 
   atype(Type,1,0,_), !, B = Alfa,
   dontResolve(P).

accommodateAlfa(Type,[a(B)|P],Alfa):-
   atype(Type,1,1,_), !,
   ( dontResolve(P), B = Alfa
   ; B = drs([],[]), accommodateAlfa(Type,P,Alfa) ).

accommodateAlfa(Type,[a(B)],Alfa):- 
   atype(Type,0,1,_), !, B = Alfa.

accommodateAlfa(Type,[a(B)|P],Alfa):- 
   atype(Type,0,1,_), !, B = drs([],[]),
   accommodateAlfa(Type,P,Alfa).

accommodateAlfa(Type,[a(B)|P],Alfa):-
   warning('unknown alfa-type: ~p',[Type]), !, B = Alfa,
   dontResolve(P).


/* ========================================================================
   Do not resolve remaining of projection path
======================================================================== */

dontResolve([]):- !.

dontResolve([a(drs([],[]))|L]):- !,
   dontResolve(L).

dontResolve([r(X,X)|L]):- !,
   dontResolve(L).


/* ========================================================================
   Merge Domains - Check for Duplicates; Copy Indexes
======================================================================== */

mergeDomains([],L,L):- !.

mergeDomains([I1:X|R],L1,L3):-
   option('--semantics',tacitus),
   select(I2:Y,L1,L2), X==Y, !,
   append(I1,I2,I3), sort(I3,I4),
   mergeDomains(R,[I4:X|L2],L3).

mergeDomains([_:X|R],L1,L3):-
   select(I:Y,L1,L2), X==Y, !,
   mergeDomains(R,[I:X|L2],L3).

mergeDomains([X|R],L1,[X|L2]):-
   option('--semantics',tacitus), !,
   mergeDomains(R,L1,L2).

mergeDomains([_:X|R],L1,[[]:X|L2]):-
   mergeDomains(R,L1,L2).


/* ========================================================================
   Merge Conditions - Check for Duplicates; Copy Indexes
======================================================================== */

mergeConditions([],L,L):- !.

%mergeConditions([_:named(X,Sym,_,Sense)|R],L1,L3):-      %%% merge names with
%   select(I:named(Y,Sym,Type,Sense),L1,L2), X==Y, !,     %%% different types
%   mergeConditions(R,[I:named(X,Sym,Type,Sense)|L2],L3).

mergeConditions([I1:X|R],L1,L3):-
   option('--semantics',tacitus),  
   select(I2:Y,L1,L2), X==Y, !,
   append(I1,I2,I3), sort(I3,I4),
   mergeConditions(R,[I4:X|L2],L3).

mergeConditions([_:X|R],L1,L3):-                         %%% merge identical
   select(I:Y,L1,L2), X==Y, !,                           %%% conditions, keep
   mergeConditions(R,[I:X|L2],L3).                       %%% index of antecedent

mergeConditions([I1:timex(X,date(D1,D2,D3,D4))|R],L1,L3):-
   option('--semantics',drg),
   select(I2:timex(Y,date(E1,E2,E3,E4)),L1,L2), 
   mergeConditions([D1],[E1],[F1]),
   mergeConditions([D2],[E2],[F2]),
   mergeConditions([D3],[E3],[F3]),
   mergeConditions([D4],[E4],[F4]),
   X==Y, !,
   append(I1,I2,I3), sort(I3,I4),
   mergeConditions(R,[I4:timex(X,date(F1,F2,F3,F4))|L2],L3).

mergeConditions([_:timex(X,date(D1,D2,D3,D4))|R],L1,L3):-
   select(I:timex(Y,date(E1,E2,E3,E4)),L1,L2), 
   mergeConditions([D1],[E1],[F1]),
   mergeConditions([D2],[E2],[F2]),
   mergeConditions([D3],[E3],[F3]),
   mergeConditions([D4],[E4],[F4]),
   X==Y, !,
   mergeConditions(R,[I:timex(X,date(F1,F2,F3,F4))|L2],L3).

mergeConditions([X|R],L1,[X|L2]):-
   mergeConditions(R,L1,L2).




