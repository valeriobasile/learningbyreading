
:- module(transform,[preprocess/6,
                     topsem/2,      % +Der, -Sem 
                     topatt/2,      % +Der, -Attributes
                     topstr/2,      % +Der, -String
                     topcat/2]).    % +Der, -Category

:- use_module(library(lists),[append/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[error/2,warning/2]).
:- use_module(boxer(morpha),[morpha/2]).
:- use_module(boxer(slashes)).


/* -------------------------------------------------------------------------
   Pre-Processing of CCG derivation to ensure correct format
------------------------------------------------------------------------- */

preprocess(SID,X,Y,Tags,Start,End):-
   setTokID(SID,Start,TokID),
   trans(X,TokID,Y,End,Tags), !.

preprocess(SID,_,_,_,_,_):-
   error('unable to preprocess derivation ~p',[SID]), !, fail.


/* -------------------------------------------------------------------------
   Funny (C&C wrongly analysed cases of N coordination)
------------------------------------------------------------------------- */

trans(fa(n,X,funny(n,Conj,fa(n,Y,Z))),N1,X2,N3,Tags):- !,
   trans(fa(n,ba(n/n,X,conj((n/n)\(n/n),n/n,Conj,Y)),Z),N1,X2,N3,Tags).
   
trans(funny(_,_,X1),N1,X2,N3,Tags):- !,
   warning('the funny combinatory rule causes skipping token ~p',[N1]),
   N2 is N1 + 1, %% assuming we skip one word (i.e. 'and')
   trans(X1,N2,X2,N3,Tags).


/* -------------------------------------------------------------------------
   Punctuation typechange rules
------------------------------------------------------------------------- */

trans(rtc(C,X1,Pu1),N1,ba(C,nil,Att,Str,X2,X3),N3,Tags1-Tags3):- 
   Pu1 =.. [t,_|Cs], !,
   trans(X1,N1,X2,N2,Tags1-Tags2),
   topcat(X2,Cat),
   topatt(X2,Att),
   Pu2 =.. [t,C\Cat|Cs],
   trans(Pu2,N2,X3,N3,Tags2-Tags3),
   strings(X2,X3,Str).

trans(ltc(C,Pu1,X1),N1,fa(C,nil,Att,Str,X2,X3),N3,Tags1-Tags3):-
   trans(Pu1,N1,Pu2,N2,Tags1-Tags2),
   Pu2 = t(_,Tok2,Sem2,Att2,I), !,
   trans(X1,N2,X3,N3,Tags2-Tags3),
   topcat(X3,Cat),
   topatt(X3,Att),
   X2 = t(C/Cat,Tok2,Sem2,Att2,I),
   strings(X3,X2,Str).


/* -------------------------------------------------------------------------
   Punctuation rules
------------------------------------------------------------------------- */

trans(rp(Cat,X1,Y0),N1,X2,N3,Tags):-
   Y0 =.. [t,_|L], !, Y1 =.. [t,Cat\Cat|L],
   trans(ba(Cat,X1,Y1),N1,X2,N3,Tags).

trans(lp(Cat,X0,Y1),N1,X2,N3,Tags):-
   X0 =.. [t,_|L], !, X1 =.. [t,Cat/Cat|L],
   trans(fa(Cat,X1,Y1),N1,X2,N3,Tags).


/* -------------------------------------------------------------------------
   Application
------------------------------------------------------------------------- */

trans(fa(_,X1,Y1),  N1,  fa(C1,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   topcat(X2,C1/C2),
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   topcat(Y2,C2),
   strings(X2,Y2,Str),
   headAtt(X2,Y2,Att).

trans(ba(_,X1,Y1),  N1,  ba(C2,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   topcat(X2,C1),
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   topcat(Y2,C2\C1),
   strings(X2,Y2,Str),
   headAtt(Y2,X2,Att).


/* -------------------------------------------------------------------------
   Composition
------------------------------------------------------------------------- */

trans(fc(_,X1,Y1),  N1,  fc(C1/C3,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   topcat(X2,C1/C2),
   topcat(Y2,C2/C3),
   strings(X2,Y2,Str),
   headAtt(X2,Y2,Att).

trans(bc(_,X1,Y1),  N1,  bc(C3\C1,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   topcat(X2,C2\C1),
   topcat(Y2,C3\C2),
   strings(X2,Y2,Str),  
   headAtt(Y2,X2,Att).

/* -------------------------------------------------------------------------
   Generalised Composition
------------------------------------------------------------------------- */

trans(gfc(C,N,X1,Y1),  N1,  gfc(C,N,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(X2,Y2,Att).

trans(gbc(C,N,X1,Y1),  N1,  gbc(C,N,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(Y2,X2,Att).


/* -------------------------------------------------------------------------
   Crossed Composition
------------------------------------------------------------------------- */

trans(bxc(_,X1,Y1),  N1,  bxc(C3/C1,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   topcat(X2,C2/C1),
   topcat(Y2,C3\C2),
   strings(X2,Y2,Str),  
   headAtt(X2,Y2,Att).

trans(fxc(C,X1,Y1), N1, fxc(C,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(Y2,X2,Att).


/* -------------------------------------------------------------------------
   Generalised Crossed Composition
------------------------------------------------------------------------- */

trans(gfxc(C,N,X1,Y1), N1, gfxc(C,N,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(X2,Y2,Att).

trans(gbxc(C,N,X1,Y1), N1, gbxc(C,N,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(Y2,X2,Att).


/* -------------------------------------------------------------------------
   Conjuction (Coordination)
------------------------------------------------------------------------- */

%trans(conj(np:nb\np:nb,np:nb,X1,Y1), N1, conj(np\np,np,nil,Att,X2,Y2), N3, Tags1-Tags3):- 
%   X1 =.. [t,conj|Cs], !,
%   X3 =.. [t,conj:np|Cs], 
%   trans(X3,N1,X2,N2,Tags1-Tags2), 
%   trans(Y1,N2,Y2,N3,Tags2-Tags3),
%   topatt(Y2,Att).

trans(conj(np\np,np,X1,Y1), N1, conj(np\np,np,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- 
   X1 =.. [t,comma   |Cs], !,       % replace apposition comma 
   X3 =.. [t,conj:app|Cs],          % by category conj:app
   trans(X3,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),
   topatt(Y2,Att).

trans(conj(Cat\Cat,Cat,X1,Y1), N1, conj(NewCat\NewCat,NewCat,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !,
   adjustFeatures(Cat,NewCat),
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   X2 =.. [_,conj:NewCat|_], 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),
   topatt(Y2,Att).

trans(conj(_C1,_C2,Y1,Z1), N1, conj(C\C,C,nil,Att,Str,Y2,Z2), N3, Tags1-Tags3):- !, 
   trans(Y1,N1,Y2,N2,Tags1-Tags2), 
   trans(Z1,N2,Z2,N3,Tags2-Tags3),
   strings(Y2,Z2,Str),
   topcat(Z2,C),
   topatt(Z2,Att).

trans(coord(_C,X1,Y1,Z1), N1, coord(C,nil,Att,Str,X2,Y2,Z2), N4, Tags1-Tags4):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3), 
   trans(Z1,N3,Z2,N4,Tags3-Tags4),
   topstr(X2,S1),
   topstr(Y2,S2), append(S1,S2,S3),
   topstr(Z2,S4), append(S3,S4,Str),
   topcat(Z2,C),
   topatt(Z2,Att).


/* -------------------------------------------------------------------------
   Unary Rules: Type Changing
------------------------------------------------------------------------- */

trans(lx(C,D,X),N1,T,N2,Tags):- !, trans(tc(C,D,X),N1,T,N2,Tags).

trans(tc(C1,_,X1), N1, tc(C3,C2,nil,Att,Str,X2), N2, Tags):- !, 
   adjustFeatures(C1,C3),
   trans(X1,N1,X2,N2,Tags),
   topcat(X2,C2),
   topstr(X2,Str),
   topatt(X2,Att).


/* -------------------------------------------------------------------------
   Unary Rules: Type Raising
------------------------------------------------------------------------- */

trans(tr(C1/(C1\C2),X1), N1, ftr(C1/(C1\C2),C2,nil,Att,Str,X2), N2, Tags):- !, 
   trans(X1,N1,X2,N2,Tags),
   topcat(X2,C2),
   topstr(X2,Str),
   topatt(X2,Att).

trans(tr(C1\(C1/C2),X1), N1, btr(C1\(C1/C2),C2,nil,Att,Str,X2), N2, Tags):- !, 
   trans(X1,N1,X2,N2,Tags),
   topcat(X2,C2),
   topstr(X2,Str),
   topatt(X2,Att).


/* -------------------------------------------------------------------------
   Substitution
------------------------------------------------------------------------- */

trans(fs(C,X1,Y1),  N1,  fs(C,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(X2,Y2,Att).

trans(bs(C,X1,Y1),  N1,  bs(C,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(Y2,X2,Att).

trans(fxs(C,X1,Y1), N1, fxs(C,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2), 
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(X2,Y2,Att).

trans(bxs(C,X1,Y1), N1, bxs(C,nil,Att,Str,X2,Y2), N3, Tags1-Tags3):- !, 
   trans(X1,N1,X2,N2,Tags1-Tags2),  
   trans(Y1,N2,Y2,N3,Tags2-Tags3),
   strings(X2,Y2,Str),  
   headAtt(Y2,X2,Att).


/* -------------------------------------------------------------------------
   Token (repair rules -- systematically wrong output of C&C parser)
------------------------------------------------------------------------- */

trans(t(A,B,people,C,D,E),N,Tok,M,Tags):- 
   option('--x',true), !, % SemEval-2014
   trans(t(A,B,person,C,D,E),N,Tok,M,Tags).

trans(t(A,B,C,'NNP',D,E),N,Tok,M,Tags):- 
   option('--x',true), !, % SemEval-2014
   trans(t(A,B,C,'NN',D,E),N,Tok,M,Tags).

trans(t(Cat,'\'t',_,'VB',S,Ne),N,Tok,M,Tags):- !,
   trans(t(Cat,'\'t','RB',[lemma:not,sense:S,namex:Ne]),N,Tok,M,Tags).

trans(t(Cat,'\'t',_,'VB',Att),N,Tok,M,Tags):- !,
   trans(t(Cat,'\'t','RB',Att),N,Tok,M,Tags).

trans(t(A,B,please,'VB',D,E),N,Tok,M,Tags):- !,
   trans(t(A,B,please,'RB',D,E),N,Tok,M,Tags).


/* -------------------------------------------------------------------------
   Token (wrapper rules -- dealing with old output of C&C parser)
------------------------------------------------------------------------- */

trans(t(Cat,Token,Pos),N,Tok,M,Tags):- !,
   trans(t(Cat,Token,Pos,[]),N,Tok,M,Tags).

trans(t(Cat,Token,Lem,Pos,S1,Ne),N,Tok,M,Tags):- !,
   context(S1,S2),
   trans(t(Cat,Token,Pos,[lemma:Lem,namex:Ne|S2]),N,Tok,M,Tags).


/* -------------------------------------------------------------------------
   Token
------------------------------------------------------------------------- */

trans(t(Cat1,Tok,Pos,Tags),N,t(Cat2,Tok,nil,RevTags,N),M,T1-T2):-
   adjustFeatures(Cat1,Cat2),
   tags(T2,N,[tok:Tok,pos:Pos|Tags],T1),
   morpha([pos:Pos|Tags],RevTags),
   M is N + 1.

% old input version (t/6 terms)
%trans(t(Cat1,Tok,Lem,Pos,S1,Ne),N,t(Cat2,Tok,nil,[pos:Pos,lemma:Lem,namex:Ne|S2],N),M,T1-T2):-
%   context(S1,S2),
%   adjustFeatures(Cat1,Cat2),
%   tags(T2,N,[tok:Tok,pos:Pos,lemma:Lem,namex:Ne|S2],T1),  
%   M is N + 1.
 

/* =========================================================================
   String Formation
========================================================================= */

strings(D1,D2,W):- topstr(D1,W1), topstr(D2,W2), append(W1,W2,W).


/* =========================================================================
   External Context Information (for now only Word Sense Disambiguation)
========================================================================= */

context(Number,Sense):- number(Number), !, Sense = [sense:Number].
context(_,Sense):- Sense = [].


/* =========================================================================
   Determine Feature on N
========================================================================= */

featureN('NNPS', nam):- !.
featureN('NNP',  nam):- !.
featureN('CD',   num):- !.
featureN(_,      nom).



/* =========================================================================
   Adjust features (mostly bugs in C&C parser)
========================================================================= */

adjustFeatures(conj/conj, conj:X/conj:X):- !.        

adjustFeatures(conj, conj:_):- !.        

adjustFeatures(comma, conj:_):- !.        

adjustFeatures(semi, conj:_):- !.        

adjustFeatures(s, s:_):- !.             %%% bug in C&C parser

adjustFeatures(s\np, s:dcl\np):- !.     %%% bug in C&C parser

adjustFeatures(s/s:X, s:X/s:X).         %%% bug in C&C parser
adjustFeatures(s/s:X, s:_/s:X):- !.     %%% bug in C&C parser

adjustFeatures( (((s:Y\np)\(s:Y\np))\((s\np)\(s\np)))/((s\np)\(s\np)),Cat):- !,
   Cat = (((s:Y\np)\(s:Y\np))\((s:X\np)\(s:X\np)))/((s:Z\np)\(s:Z\np)).

adjustFeatures(np:_, np):- !.

adjustFeatures(n:_, n):- !.


/* =========================================================================
   Adjust features
========================================================================= */

adjustFeatures(F1/A1,F2/A2):- !,
   adjustFeatures(F1,F2),
   adjustFeatures(A1,A2).

adjustFeatures(F1\A1,F2\A2):- !,
   adjustFeatures(F1,F2),
   adjustFeatures(A1,A2).

adjustFeatures(Cat,Cat).


/* =========================================================================
   Adding Info
========================================================================= */

tags(T,ID,Tags,[ID:Tags|T]).


/* =========================================================================
   Get top categorie, semantics or attributes from a derivation
========================================================================= */

topcat(Der,Cat):- top(Der,Cat,_,_,_), !.
topsem(Der,Sem):- top(Der,_,Sem,_,_), !.
topatt(Der,Att):- top(Der,_,_,Att,_), !.
topstr(Der,Str):- top(Der,_,_,_,Str), !.


/* =========================================================================
   Top categorie, semantics or attributes from a derivation
========================================================================= */

top(fa(C,S,A,W,_,_),C,S,A,W).
top(ba(C,S,A,W,_,_),C,S,A,W).
top(fc(C,S,A,W,_,_),C,S,A,W).
top(bc(C,S,A,W,_,_),C,S,A,W).
top(fxc(C,S,A,W,_,_),C,S,A,W).
top(bxc(C,S,A,W,_,_),C,S,A,W).
top(fs(C,S,A,W,_,_),C,S,A,W).
top(bs(C,S,A,W,_,_),C,S,A,W).
top(fxs(C,S,A,W,_,_),C,S,A,W).
top(bxs(C,S,A,W,_,_),C,S,A,W).
top(gfc(C,_,S,A,W,_,_),C,S,A,W).
top(gbc(C,_,S,A,W,_,_),C,S,A,W). 
top(gfxc(C,_,S,A,W,_,_),C,S,A,W). 
top(gbxc(C,_,S,A,W,_,_),C,S,A,W). 
top(gfc(C,S,A,W,_,_),C,S,A,W).
top(gbc(C,S,A,W,_,_),C,S,A,W). 
top(gfxc(C,S,A,W,_,_),C,S,A,W). 
top(gbxc(C,S,A,W,_,_),C,S,A,W). 
top(ftr(C,_,S,A,W,_),C,S,A,W).
top(btr(C,_,S,A,W,_),C,S,A,W).
top(tc(C,_,S,A,W,_),C,S,A,W).
top(lx(C,S,A,W,_,_),C,S,A,W).
top(t(C,W,S,A,_),C,S,A,[W]).
top(conj(C,_,S,A,W,_,_),C,S,A,W).
top(coord(C,S,A,W,_,_,_),C,S,A,W).


/* -------------------------------------------------------------------------
   Take attributes from head
------------------------------------------------------------------------- */

headAtt(D1,D2,Att):- topcat(D1,C1/C2), C1==C2, !, topatt(D2,Att).
headAtt(D1,D2,Att):- topcat(D1,C1\C2), C1==C2, !, topatt(D2,Att).
headAtt(D0,_ ,Att):- topatt(D0,Att), !.


/* -------------------------------------------------------------------------
   Set Token-ID
------------------------------------------------------------------------- */

setTokID(_,Start,Start):-
   option('--tokid',global), !.

setTokID(SID,_,Start):-
   option('--tokid',local), !,
   Start is (SID*1000)+1.
