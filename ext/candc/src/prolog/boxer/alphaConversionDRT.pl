
:- module(alphaConversionDRT,[alphaConvertDRS/2]).

:- use_module(library(lists),[member/2]).
:- use_module(semlib(errors),[warning/2]).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvertDRS(B1,B2):-
   alphaConvertDRS(B1,[]-_,[]-_,B2), !.


/*========================================================================
   Variable
========================================================================*/

variable(X):- var(X), !.
variable(X):- functor(X,'$VAR',1), !.


/*========================================================================
   Alpha Conversion (terms)
========================================================================*/

alphaConvertVar(X,Vars,New):-
   variable(X), !,
   (
      member(sub(Z,Y),Vars),           %%% BOUND
      X==Z, !,
      New=Y
   ;
      New=X                            %%% FREE
   ).

alphaConvertVar(X,_,X).


/*========================================================================
   Alpha Conversion (DRSs)
========================================================================*/

alphaConvertDRS(X1,Var-Var,Ptr-Ptr,X2):-
   variable(X1), !,
   alphaConvertVar(X1,Var,X2).

alphaConvertDRS(lam(X,B1),Vars-Vars,Ptrs,lam(Y,B2)):- !,
   alphaConvertDRS(B1,[sub(X,Y)|Vars]-_,Ptrs,B2).

alphaConvertDRS(B1:drs(D,C),Vars,Ptr1-Ptr2,B2:Drs):- !, 
   alphaConvertDRS(drs(D,C),Vars,[sub(B1,B2)|Ptr1]-Ptr2,Drs).

alphaConvertDRS(drs([],[]),Vars-Vars,Ptr-Ptr,drs([],[])):- !.

alphaConvertDRS(drs([],[B1:I:C1|Conds1]),Vars1-Vars2,Ptr1-Ptr3,drs([],[B2:I:C2|Conds2])):- !,
   alphaConvertVar(B1,Ptr1,B2),
   alphaConvertCondition(C1,Vars1,Ptr1-Ptr2,C2), 
   alphaConvertDRS(drs([],Conds1),Vars1-Vars2,Ptr2-Ptr3,drs([],Conds2)).

alphaConvertDRS(drs([B1:I:Ref|L1],C1),Var1-Var2,Ptr1-Ptr2,drs([B2:I:New|L2],C2)):- !,
   alphaConvertVar(B1,Ptr1,B2),
   alphaConvertDRS(drs(L1,C1),[sub(Ref,New)|Var1]-Var2,Ptr1-Ptr2,drs(L2,C2)).

alphaConvertDRS(alfa(Type,B1,B2),Var1-Var3,Ptr1-Ptr3,alfa(Type,B3,B4)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var2-Var3,Ptr2-Ptr3,B4).

alphaConvertDRS(merge(B1,B2),Var1-Var3,Ptr1-Ptr3,merge(B3,B4)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var2-Var3,Ptr2-Ptr3,B4).

alphaConvertDRS(app(E1,E2),Var-Var,Ptr1-Ptr3,app(E3,E4)):- !,
   alphaConvertDRS(E1,Var-_,Ptr1-Ptr2,E3),
   alphaConvertDRS(E2,Var-_,Ptr2-Ptr3,E4).

alphaConvertDRS(sdrs([],[]),Var-Var,Ptr-Ptr,sdrs([],[])):- !.

alphaConvertDRS(sdrs([],[I:C1|L1]),Var1-Var2,Ptr1-Ptr3,sdrs([],[I:C2|L2])):- !,
   alphaConvertCondition(C1,Var1,Ptr1-Ptr2,C2),
   alphaConvertDRS(sdrs([],L1),Var1-Var2,Ptr2-Ptr3,sdrs([],L2)).

alphaConvertDRS(sdrs([B1|L1],C1),Var1-Var3,Ptr1-Ptr3,sdrs([B2|L2],C2)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr1-Ptr2,B2),
   alphaConvertDRS(sdrs(L1,C1),Var2-Var3,Ptr2-Ptr3,sdrs(L2,C2)).

alphaConvertDRS(lab(Ref,B1),Var1-[sub(Ref,New)|Var2],Ptr,lab(New,B2)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr,B2).

alphaConvertDRS(sub(B1,B2),Var1-Var3,Ptr1-Ptr3,sub(B3,B4)):- !,
   alphaConvertDRS(B1,Var1-Var2,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var2-Var3,Ptr2-Ptr3,B4).

alphaConvertDRS(Sym,Vars-Vars,Ptr-Ptr,Sym):- atomic(Sym), !.

alphaConvertDRS(U,_,_,_):- !,
   warning('Unknown DRS expression: ~p',[U]), fail.


/*========================================================================
   Alpha Conversion (DRS-Conditions)
========================================================================*/

alphaConvertCondition(nec(B1),Vars,Ptr,nec(B2)):- !,
   alphaConvertDRS(B1,Vars-_,Ptr,B2).

alphaConvertCondition(pos(B1),Vars,Ptr,pos(B2)):- !,
   alphaConvertDRS(B1,Vars-_,Ptr,B2).

alphaConvertCondition(not(B1),Vars,Ptr,not(B2)):- !,
   alphaConvertDRS(B1,Vars-_,Ptr,B2).

alphaConvertCondition(prop(X,B1),Vars,Ptr,prop(Y,B2)):- !,
   alphaConvertVar(X,Vars,Y),
   alphaConvertDRS(B1,Vars-_,Ptr,B2).

alphaConvertCondition(imp(B1,B2),Var,Ptr1-Ptr3,imp(B3,B4)):- !,
   alphaConvertDRS(B1,Var -Var1,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var1-_,   Ptr2-Ptr3,B4).

alphaConvertCondition(duplex(Type,B1,X,B2),Var,Ptr1-Ptr3,duplex(Type,B3,Y,B4)):- !,
   alphaConvertDRS(B1,Var-Var1,Ptr1-Ptr2,B3),
   alphaConvertVar(X,Var1,Y),
   alphaConvertDRS(B2,Var1-_,Ptr2-Ptr3,B4).

alphaConvertCondition(or(B1,B2),Var,Ptr1-Ptr3,or(B3,B4)):- !,
   alphaConvertDRS(B1,Var-_,Ptr1-Ptr2,B3),
   alphaConvertDRS(B2,Var-_,Ptr2-Ptr3,B4).

alphaConvertCondition(pred(Arg1,Sym,Type,Sense),Var,Ptr-Ptr,pred(Arg2,Sym,Type,Sense)):- !,
   alphaConvertVar(Arg1,Var,Arg2).

alphaConvertCondition(rel(Arg1,Arg2,Sym),Var,Ptr-Ptr,rel(Arg3,Arg4,Sym)):- !,
   alphaConvertVar(Arg1,Var,Arg3),
   alphaConvertVar(Arg2,Var,Arg4).

alphaConvertCondition(rel(Arg1,Arg2,'=',0),Var,Ptr,Rel):- !,
   warning('Unexpected relation symbol: ~p = ~p',[Arg1,Arg2]),
   alphaConvertCondition(eq(Arg1,Arg2),Var,Ptr,Rel).

alphaConvertCondition(rel(Arg1,Arg2,Sym,Sense),Var,Ptr-Ptr,rel(Arg3,Arg4,Sym,Sense)):- !,
   alphaConvertVar(Arg1,Var,Arg3),
   alphaConvertVar(Arg2,Var,Arg4).

alphaConvertCondition(role(Arg1,Arg2,Sym,Dir),Var,Ptr-Ptr,role(Arg3,Arg4,Sym,Dir)):- !,
   alphaConvertVar(Arg1,Var,Arg3),
   alphaConvertVar(Arg2,Var,Arg4).

alphaConvertCondition(named(X,Sym,Type,Sense),Var,Ptr-Ptr,named(Y,Sym,Type,Sense)):- !,
   alphaConvertVar(X,Var,Y).

alphaConvertCondition(card(X,Sym1,T),Vars,Ptr-Ptr,card(Y,Sym2,T)):- !,
   alphaConvertVar(X,Vars,Y),
   alphaConvertVar(Sym1,Vars,Sym2).

alphaConvertCondition(timex(X,Sym),Vars,Ptr-Ptr,timex(Y,Sym)):- !,
   alphaConvertVar(X,Vars,Y).

alphaConvertCondition(eq(X1,X2),Vars,Ptr-Ptr,eq(Y1,Y2)):- !,
   alphaConvertVar(X1,Vars,Y1),
   alphaConvertVar(X2,Vars,Y2).

alphaConvertCondition(U,_,_,_):- !,
   warning('Unknown condition: ~p',[U]), fail.
