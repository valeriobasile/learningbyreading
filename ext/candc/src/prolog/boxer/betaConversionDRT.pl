
:- module(betaConversionDRT,[betaConvert/2]).

:- use_module(boxer(alphaConversionDRT),[alphaConvertDRS/2]).
:- use_module(boxer(mergeDRT),[mergeDrs/2]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).


/* ========================================================================
   Beta-Conversion (introducing stack)
======================================================================== */

betaConvert(X,Y):- betaConvert(X,Y,[]), !.

betaConvert(X,X):- warning('beta-conversion failed for ~p',[X]).


/* ========================================================================
   Beta-Conversion (core stuff)
======================================================================== */

betaConvert(X,Y,[]):- var(X), !, Y=X.

betaConvert(X,Y,L):-  var(X), !, wrapArguments(L,X,Y).

betaConvert(app(Functor,Argument),Result,Stack):- 
   nonvar(Functor), !,
   alphaConvertDRS(Functor,Converted),
   betaConvert(Argument,ConArg),               %%% much faster this way!
   betaConvert(Converted,Result,[ConArg|Stack]).

betaConvert(lam(X,Formula),Result,[X|Stack]):- !,
   betaConvert(Formula,Result,Stack).

betaConvert(lam(X,Formula),lam(X,Result),[]):- !,
   betaConvert(Formula,Result).

betaConvert(merge(B1,B2),Result,[]):- !,
   betaConvert(B1,K1),
   betaConvert(B2,K2),
   mergeDrs(merge(K1,K2),Result).

betaConvert(sdrs([],C),sdrs([],C),[]):- !.

betaConvert(sdrs([B1|L1],C1),Result,[]):- !,
   betaConvert(B1,B2),
   betaConvert(sdrs(L1,C1),sdrs(L2,C2)),
   mergeDrs(sdrs([B2|L2],C2),Result).

betaConvert(lab(X,B1),lab(X,B2),[]):- !,
   betaConvert(B1,B2).

betaConvert(sub(B1,B3),sub(B2,B4),[]):- !,
   betaConvert(B1,B2),
   betaConvert(B3,B4).

betaConvert(B1:drs(D1,C1),B1:drs(D2,C3),[]):- 
   option('--elimeq',true),
   select(_:[]:eq(X1,Y1),C1,C2),
   select(B2:[]:Y2,D1,D2), B1==B2, Y1==Y2, !, X1=Y1,
   betaConvertConditions(C2,C3).

betaConvert(B:drs(D,C1),B:drs(D,C2),[]):- !, 
   betaConvertConditions(C1,C2).

betaConvert(alfa(T,B1,B2),Result,[]):- !, 
   betaConvert(B1,K1), 
   betaConvert(B2,K2),
   mergeDrs(alfa(T,K1,K2),Result).

betaConvert(app(B1,B2),app(K1,K2),[]):- !, 
   betaConvert(B1,K1), 
   betaConvert(B2,K2).

betaConvert(X,Y,L):- !,  
   wrapArguments(L,X,Y).


/* ========================================================================
   Beta-Convert DRS Conditions
======================================================================== */

betaConvertConditions([],[]).

betaConvertConditions([B:I:C1|L1],[B:I:C2|L2]):- !,
   betaConvertCond(C1,C2),
   betaConvertConditions(L1,L2).


/* ========================================================================
   Beta-Convert DRS Condition
======================================================================== */

betaConvertCond(not(A1),not(B1)):- !, betaConvert(A1,B1).
betaConvertCond(pos(A1),pos(B1)):- !, betaConvert(A1,B1).
betaConvertCond(nec(A1),nec(B1)):- !, betaConvert(A1,B1).
betaConvertCond(prop(X,A1),prop(X,B1)):- !, betaConvert(A1,B1).
betaConvertCond(or(A1,A2),or(B1,B2)):- !, betaConvert(A1,B1), betaConvert(A2,B2).
betaConvertCond(imp(A1,A2),imp(B1,B2)):- !, betaConvert(A1,B1), betaConvert(A2,B2).
betaConvertCond(duplex(T,A1,X,A2),duplex(T,B1,X,B2)):- !, betaConvert(A1,B1), betaConvert(A2,B2).
betaConvertCond(C,C).


/* ========================================================================
   Wrapping arguments from stack
======================================================================== */

wrapArguments([],X,X).

wrapArguments([A|L],X,Y):- wrapArguments(L,app(X,A),Y).


