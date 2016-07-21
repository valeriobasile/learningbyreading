
:- module(vpe,[resolveVPE/2]).

/* =============================================================
   Importing predicates
============================================================= */

:- use_module(library(lists),[member/2,append/3,select/3]).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2]).


/* =============================================================
   Main
============================================================= */

resolveVPE(B,B):- option('--vpe',false), !.

resolveVPE(B,B):- option('--vpe',true), !,
   warning('VPE resolution not activated',[]).

resolveVPE(In,Out):- detectVPE(In,Out,[]-_Stack), !.

resolveVPE(B,B).


/* =============================================================
   VPE detection
============================================================= */

detectVPE(merge(B1,B2),merge(U1,U2),St1-St3):- !, 
   detectVPE(B1,U1,St1-St2), 
   window(St2,St4),
   detectVPE(B2,U2,St4-St3).

detectVPE(alfa(T,B1,B2),alfa(T,U1,U2),St1-St3):- !, 
   detectVPE(B1,U1,St1-St2), 
   detectVPE(B2,U2,St2-St3).

detectVPE(B:drs(Dom,Conds1),Drs,St1-St2):-
   select(_:Ind:pred(E,Sym,v,98),Conds1,Conds2),
   constraints(Conds2,Sym,E),   
   %%% VPE detected, !,
   resolveVPE(B:drs(Dom,Conds2),Ind,Sym,E,St1,ResolvedDrs),
   detectVPE(ResolvedDrs,Drs,St1-St2).

detectVPE(B:drs(Dom,Con1),B:drs(Dom,Con2),St1-St2):-
   detectVPEc(Con1,Con2,[B:drs(Dom,Con1)|St1]-St2).


/* =============================================================
   VPE detection (DRS-conditions)
============================================================= */

detectVPEc([B:I:not(B)|L1],[B:I:not(U)|L2],St1-St3):- !, 
   detectVPE(B,U,St1-St2),
   detectVPEc(L1,L2,St2-St3).

detectVPEc([B:I:pos(B)|L1],[B:I:pos(U)|L2],St1-St3):- !, 
   detectVPE(B,U,St1-St2),
   detectVPEc(L1,L2,St2-St3).

detectVPEc([B:I:nec(B)|L1],[B:I:nec(U)|L2],St1-St3):- !, 
   detectVPE(B,U,St1-St2),
   detectVPEc(L1,L2,St2-St3).

detectVPEc([B:I:prop(X,B)|L1],[B:I:prop(X,U)|L2],St1-St3):- !, 
   detectVPE(B,U,St1-St2),
   detectVPEc(L1,L2,St2-St3).

detectVPEc([B:I:imp(B1,B2)|L1],[B:I:imp(U1,U2)|L2],St1-St4):- !, 
   detectVPE(B1,U1,St1-St2), 
   detectVPE(B2,U2,St2-St3), 
   detectVPEc(L1,L2,St3-St4).

detectVPEc([B:I:or(B1,B2)|L1],[B:I:or(U1,U2)|L2],St1-St4):- !, 
   detectVPE(B1,U1,St1-St2), 
   detectVPE(B2,U2,St2-St3), 
   detectVPEc(L1,L2,St3-St4).

detectVPEc([B:I:duplex(T,B1,V,B2)|L1],[B:I:duplex(T,U1,V,U2)|L2],St1-St4):- !, 
   detectVPE(B1,U1,St1-St2), 
   detectVPE(B2,U2,St2-St3), 
   detectVPEc(L1,L2,St3-St4).

detectVPEc([C|L1],[C|L2],St1-St2):- !,
   detectVPEc(L1,L2,St1-St2).

detectVPEc([],[],S-S).


/* =============================================================
   Constraints
============================================================= */

constraints(Conds,Sym,E):-
   constraint1(Conds,Sym,E),
   constraint2(Conds,Sym,E), 
   constraint3(Conds,Sym,E), 
   constraint4(Conds,Sym,E),
   constraint5(Conds,Sym,E),
   constraint6(Conds,Sym,E).


/* =============================================================
   Constraint 1: Not part of How-question
============================================================= */

constraint1(Conds,_,E):-
   member(_:_:rel(U,_,manner_rel,_),Conds), E==U, 
   !, fail.

constraint1(_,_,_).


/* =============================================================
   Constraint 2: do + ADJ
============================================================= */

constraint2(Conds,do,E):-
    member(_:_:pred(U,Sym,a,_),Conds), E==U, 
    member(Sym,[good,well,great,ok,poorly,terrific,right,wrong,
                badly,better,bad,best,worse,worst]), 
    !, fail.

constraint2(_,_,_).


/* =============================================================
   Constraint 3: do + NP
============================================================= */

constraint3(Conds,do,E):-
   member(_:_:pred(U,Sym,a,_),Conds), E==U, 
   member(Sym,[anything,everything,something,nothing,that,what]), 
   !, fail.

constraint3(_,_,_).


/* =============================================================
   Constraint 4: do + more/enough/...
============================================================= */

constraint4(Conds,do,E):-
   member(_:_:pred(U,Sym,a,_),Conds), E==U, 
   member(Sym,[enough,more,less,little,much]),
   !, fail.

constraint4(_,_,_).


/* =============================================================
   Constraint 5: do away with ...
============================================================= */

constraint5(Conds,do,E):-   
   member(_:_:pred(U,Sym,a,_),Conds), E==U, 
   member(Sym,[away]),
   !, fail.

constraint5(_,_,_).

/* =============================================================
   Constraint 6: where SUBJ BE
============================================================= */

constraint6(Conds,be,E):-   
   member(_:_:rel(U,_,loc_rel,0),Conds), E==U, 
   !, fail.

constraint6(_,_,_).


/* =============================================================
   Antecedent Index (only beginning and end)
============================================================= */

antBegEnd(AntInd,Beg,End):-
   member(Beg,AntInd),
   \+ (member(I,AntInd), I < Beg),
   member(End,AntInd),
   \+ (member(I,AntInd), I > End), !.


/* =============================================================
   Parallel Elements (target clause)
============================================================= */

parallelElements([],_,[],Par-Par,A-A):- 
   member(Sub,[agent,patient,theme]), 
   member(Sub,Par).

parallelElements([_:_:rel(U,X,Rel,_)|L1],E,L2,Par1-Par2,A1-A2):-
   U==E,
   parallelElements(L1,E,L2,[Rel|Par1]-Par2,app(A1,X)-A2). 

parallelElements([C|L1],E,[C|L2],Par1-Par2,A1-A2):-
   parallelElements(L1,E,L2,Par1-Par2,A1-A2). 
   

/* =============================================================
   VPE resolution (real)
============================================================= */

resolveVPE(B:drs(Dom,Conds1),Ind,_Sym,E,Stack,ResolvedDrs):-
   option('--x',false),
   tempStack(B:drs(Dom,Conds1),Stack,TempStack),
   parallelElements(Conds1,E,Conds2,[]-Par,app(Q,E)-App),
   NewDrs = app(VP,lam(Q,merge(B:drs(Dom,Conds2),App))),
   member(AntDrs,TempStack), 
   sortDRS(AntDrs,SortedAntDrs),
   abstractDRS(SortedAntDrs,Ind,Par,VP,_), 
   betaConvert(NewDrs,ResolvedDrs), !.

resolveVPE(B:drs(Dom,Conds),Ind,Sym,E,_Stack,Drs):-
   option('--x',false),
   Drs = B:drs(Dom,[B:Ind:pred(E,Sym,v,0)|Conds]).


/* =============================================================
   VPE resolution (experimental)
============================================================= */

resolveVPE(B:drs(Dom,Conds1),Ind,Sym,E,Stack,Drs):-
   option('--x',true),
   tempStack(B:drs(Dom,Conds1),Stack,TempStack),
%   write('Stack:'),nl,writeStack(TempStack,1),
   findall(Par,parallelElements(Conds1,E,_,[]-Par,_),Pars),
   nSolutions(Pars,TempStack,E,Sym,Ind,0,Conds1,Conds3), !,
   Drs = B:drs(Dom,Conds3).

resolveVPE(B:drs(Dom,Conds),Ind,Sym,E,_Stack,Drs):-
   option('--x',true),
   Drs = B:drs(Dom,[B:Ind:pred(E,Sym,v,97)|Conds]).


/* =============================================================
   N solutions
============================================================= */

nSolutions([],_,_,_,_,N,C,C):- !, N > 0.

nSolutions([P|Ps],Stack,E,Sym,Ind,N1,C1,C3):-
   nSol(Stack,E,Sym,Ind,P,N1,N2,C1,C2),
   nSolutions(Ps,Stack,E,Sym,Ind,N2,C2,C3).

nSol([],_,_,_,_,N,N,Conds,Conds):- !.

nSol([AntDrs|Stack],E,Sym,[Ind],Par,N1,N3,Conds1,Conds2):-
   sortDRS(AntDrs,SortedAntDrs),
   abstractDRS(SortedAntDrs,[Ind],Par,_,AntInd), 
   antBegEnd(AntInd,Beg,End),
   NewInd = [Ind,Beg,End],
   \+ member(NewInd:_,Conds1), !,  % new solution!
   N2 is N1 + 1,
   New = NewInd:pred(E,Sym,vpe,N2),
   parConds(Par,NewInd,E,N2,[New|Conds1],Conds3),
   nSol([AntDrs|Stack],E,Sym,[Ind],Par,N2,N3,Conds3,Conds2).

nSol([_|Stack],E,Sym,[Ind],Par,N1,N2,Conds1,Conds2):-
   nSol(Stack,E,Sym,[Ind],Par,N1,N2,Conds1,Conds2).


/* =============================================================
   Sort DRS
============================================================= */

sortDRS(B:drs(Dom,Conds),B:drs(Dom,SortedConds)):-
   sort(Conds,SortedConds).


/* =============================================================
   Parallel Conditions (experiment only)
============================================================= */

parConds([],_,_,_,C,C).

parConds([Par|L],Index,E,N,C1,[Index:pred(E,Par,par,N)|C2]):-
   parConds(L,Index,E,N,C1,C2).
    

/* =============================================================
   Abstraction (DRSs)
============================================================= */

abstractDRS(B:drs(Dom,Conds),Ind,Par,Abs,AntInd):-
   member(AI:pred(E,_Sym,v,_),Conds),
   before(AI,Ind),             %%% excludes VP cataphora!
   minimalDistance(AI,Ind,2),  %%% excludes Bill doesnt [want] to []
   checkParallelElements(Par,E,Conds,ParRef,Drs-Lam), !,
   conceptAbstraction(Conds,[],Ind,AI,Dom,E,ParRef,[]-AbsDom,[]-AbsCond,[]-AntInd),
   Drs = B:drs(AbsDom,AbsCond),
   Abs = lam(F,app(F,lam(E,Lam))).


/* =============================================================
   Concept Abstraction
============================================================= */

conceptAbstraction([],_,_,_,_,_,_,D-D,C-C,J-J).

conceptAbstraction([I:_|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J2):- 
   before(I,AI), !,                     %%% skip material before antecedent head
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J2).

conceptAbstraction([_:Cond|_],_NotCopied,_Ind,_AI,_Dom,_E,_Par,AD-AD,AC-AC,J-J):- 
   Cond = pred(_,Pred,a,_), 
   member(Pred,[more,less,as,than]), !.       %%% comparative/equative --> block

%conceptAbstraction([_:Cond|_],_NotCopied,_Ind,_AI,_Dom,E,_Par,AD-AD,AC-AC,J-J):- 
%   Cond = pred(X,Pred,a,_), 
%   member(Pred,[more,less,as,than]),                 %%% comparative/equative --> block
%   X==E, !.

conceptAbstraction([I1:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J3):- 
   Cond = pred(X,_,_,_), X==E, !,       %%% Copy all 1-place predicates
   newIndex(I1,I2,J1,J2),
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,[I2:Cond|AC1]-AC2,J2-J3).

conceptAbstraction([I1:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J3):- 
   Cond = named(X,_,_,_), X==E, !,      %%% Copy all 1-place predicates
   newIndex(I1,I2,J1,J2),
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,[I2:Cond|AC1]-AC2,J2-J3).

conceptAbstraction([I1:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J3):- 
   Cond = timex(X,_), X==E, !,          %%% Copy all 1-place predicates
   newIndex(I1,I2,J1,J2),
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,[I2:Cond|AC1]-AC2,J2-J3).

conceptAbstraction([I1:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J3):- 
   Cond = card(X,_,_), X==E, !,          %%% Copy all 1-place predicates
   newIndex(I1,I2,J1,J2),
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,[I2:Cond|AC1]-AC2,J2-J3).

conceptAbstraction([_:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J2):- 
   option('--x',true),                  %%% Skip already parallel-marked elements
   Cond = rel(_,_,parallel,1), !,
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J2).

conceptAbstraction([I1:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J3):- 
   option('--x',true),
   Cond = rel(X,Y,_,_), X==E, 
   member(Z,Par), Z==Y, !,              %%% Parallel Element --> copy
   newIndex(I1,I2,J1,J2),
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,[I2:Cond,[]:rel(X,Y,parallel,1)|AC1]-AC2,J2-J3).

conceptAbstraction([I1:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J3):- 
   option('--x',false),
   Cond = rel(X,Y,_,_), X==E, 
   member(Z,Par), Z==Y, !,              %%% Parallel Element --> copy
   newIndex(I1,I2,J1,J2),
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,[I2:Cond|AC1]-AC2,J2-J3).

conceptAbstraction([_:Cond|_],_NotCopied,_Ind,_AI,_Dom,_E,_Par,AD-AD,AC-AC,J-J):- 
   Cond = rel(_,_,Rel,_), 
   member(Rel,[than,as,like,more,less]), !.   %%% comparative/equative --> block

%conceptAbstraction([_:Cond|_],_NotCopied,_Ind,_AI,_Dom,E,_Par,AD-AD,AC-AC,J-J):- 
%   Cond = rel(X,_,Rel,_), 
%   member(Rel,[than,as,like,more,less]),          %%% comparative/equative --> block
%   X==E, !.

conceptAbstraction([I1:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J3):- 
   Cond = rel(X,Y,_,_), X==E, 
   before(I1,Ind), 
   \+ ( member(_:U,Dom), Y==U ), 
   \+ ( member(I3:rel(X,_,Rel,_),L), X==E,    
        member(Rel,[than,as,like]),
        before(I3,I1) ),   
   !, %%% Copy relations to definites
   newIndex(I1,I2,J1,J2),
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,[I2:Cond|AC1]-AC2,J2-J3).

conceptAbstraction([I1:Cond|L],NotCopied,Ind,AI,Dom,E,Par,AD1-AD3,AC1-AC3,J1-J4):- 
   Cond = rel(X,Y,_Rel,_), X==E,
   before(I1,Ind), 
   member(_:U,Dom), Y==U, 
%   \+ ( member(I3:rel(X,_,Rel,_),L), X==E,    
%        member(Rel,[than,as,like]),
%        before(I3,I1) ),   
   \+ ( member(_:pred(V,_,_,98),L), 
        V==Y ),
   \+ ( member(_:pred(V,_,_,98),NotCopied), 
        V==Y ),
   !, %%% Copy relations to indefinites
   newIndex(I1,I2,J1,J2),
   append(L,NotCopied,Conds), sort(Conds,SortedConds),
   conceptAbstraction(SortedConds,[],Ind,AI,Dom,Y,Par,AD1-AD2,AC1-AC2,J2-J3),
   conceptAbstraction(L,NotCopied,Ind,AI,Dom,E,Par,[[]:Y|AD2]-AD3,[I2:Cond|AC2]-AC3,J3-J4).

conceptAbstraction([I1:Cond|L],NC,Ind,AI,Dom,E,Par,AD1-AD2,AC1-AC2,J1-J3):- 
   Cond = prop(X,PDrs), X==E, 
   PDrs = _:drs(_,PConds),
   \+ member(Ind:_,PConds), !,
   newIndex(I1,I2,J1,J2),
   conceptAbstraction(L,NC,Ind,AI,Dom,E,Par,AD1-AD2,[I2:Cond|AC1]-AC2,J2-J3).

conceptAbstraction([C|L],NotCopied,Ind,AI,Dom,E,Par,AD,AC,J):- 
   conceptAbstraction(L,[C|NotCopied],Ind,AI,Dom,E,Par,AD,AC,J).


/* =============================================================
   New Index (add -11 for evaluation purposes)
============================================================= */

newIndex(I,I,J,J):-
   option('--x',false), !.

newIndex(I1,I2,J1,J2):-
   option('--x',true), !,
   I2 = [-11|I1],
   append(I1,J1,J2).


/* =============================================================
   Check Parallel Elements (should all be present)
============================================================= */

checkParallelElements([],_,_,[],Lam-Lam).

checkParallelElements([Sym|L],E,Conds,[X|Par],Lam1-Lam2):-
   member(_:rel(U,X,Sym,_),Conds), E==U, !,
   checkParallelElements(L,E,Conds,Par,lam(X,Lam1)-Lam2).


/* =============================================================
   Word order
============================================================= */

before(L1,L2):-
   member(I1,L1),
   member(I2,L2), I1 < I2, !.

/* =============================================================
   Minimal distance
============================================================= */

minimalDistance([I1],[I2],Min):- !,
   Difference is abs(I1-I2),
   Difference >= Min.

minimalDistance(_,_,_).


/* =============================================================
   Window (number of DRSs kept on stack)
============================================================= */

window([A,B,C,D,E,F,G,H,I,J|_],[A,B,C,D,E,F,G,H,I,J]):- !.

window(W,W).


/* =============================================================
   Print Stack of antecedents (for debugging purposes)
============================================================= */

writeStack([],_):- nl.
writeStack([X|L],N):- 
   write(stack:N),nl, 
   output:print(user_output,X),nl, 
   M is N + 1, writeStack(L,M).


/* =============================================================
   Stack (only top N)
============================================================= */

tempStack(DRS,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T|_],[DRS,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):- !.
tempStack(DRS,Stack,[DRS|Stack]).



