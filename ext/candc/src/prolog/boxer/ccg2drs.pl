
:- module(ccg2drs,[ccg2drs/2,base/4,gen/4]).

:- use_module(library(lists),[member/2,select/3,append/3]).

:- use_module(boxer(slashes)).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).
:- use_module(boxer(resolveDRT),[resolveDRS/2]).
%:-use_module(boxer(vpe),[resolveVPE/2]).
:- use_module(boxer(transform),[preprocess/6,topcat/2,topatt/2,topsem/2,topstr/2]).
:- use_module(boxer(relation),[resolve_relations/2]).
:- use_module(boxer(closure),[closure/3]).
:- use_module(boxer(lexicon),[semlex/5]).
:- use_module(boxer(coordination),[coordMacro/2,argCard/2]).
:- use_module(boxer(typechange),[typechange/5]).
:- use_module(boxer(evaluation),[incCompleted/0,incAttempted/0]).
:- use_module(boxer(input),[preferred/2]).
:- use_module(boxer(sdrt),[mergeSDRS/2]).
:- use_module(boxer(drs2fdrs),[instDrs/1,instDrs/2]).
:- use_module(boxer(tuples),[tuples/4]).
:- use_module(boxer(categories),[att/3]).

:- use_module(semlib(drs2tacitus),[drs2tac/4]).
:- use_module(semlib(drs2amr),[drs2amr/4]).
:- use_module(semlib(pdrs2drs),[pdrs2drs/2]).
:- use_module(semlib(drs2fol),[drs2fol/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2]).



/* =========================================================================
   Main Predicate
========================================================================= */

ccg2drs(L,Ders):-  
   option('--semantics',der), !,
   ccg2ders(L,Ders,1). 

ccg2drs([CCG|L],XDRS):-  
   build(CCG,DRS,Tags,1,Index), !,
   incAttempted, incCompleted,
   ccg2drss(L,Tags,DRS,XDRS,Index). 

ccg2drs([CCG|L],XDRS):-
   incAttempted,
   noanalysis(CCG), 
   ccg2drs(L,XDRS).


/* =========================================================================
   Build rest of underspecified Semantic Representations
========================================================================= */

ccg2drss([],Tags-[],PDRS,xdrs(Tags,Sem),_):-
   resolveDRS(PDRS,Tags-[]), !,
   semantics(PDRS,Tags,Sem).

ccg2drss([CCG|L],Tags1-Tags2,PrevDRS,XDRS,Index):-
   build(CCG,DRS,Tags2-Tags3,Index,NewIndex), 
   insertDRS(PrevDRS,DRS,NewDRS), !,
   incAttempted, incCompleted,
   ccg2drss(L,Tags1-Tags3,NewDRS,XDRS,NewIndex). 

ccg2drss([CCG|L],Tags,PDRS,XDRS,Index):-
   incAttempted,
   noanalysis(CCG), 
   ccg2drss(L,Tags,PDRS,XDRS,Index). 


/* =========================================================================
   Build syntax-semantics derivations
========================================================================= */

ccg2ders([],[],_):- !.

ccg2ders([C|L],[Der|Ders],Index):-
   ccg2der(C,Der,Index,NewIndex), !,
   incAttempted,
   incCompleted,
   ccg2ders(L,Ders,NewIndex). 

ccg2ders([C|L],Ders,Index):-
   incAttempted,
   noanalysis(C), 
   ccg2ders(L,Ders,Index). 


/* =========================================================================
   Build syntax-semantics derivation
========================================================================= */

ccg2der(N,der(N,Der),Start,End):-
   preferred(N,CCG0),
   preprocess(N,CCG0,CCG1,_Tags,Start,End),
   resolve_relations(CCG1,CCG2),
   interpretDer(CCG2,Der).

interpretDer(CCG,Copy):- 
   interpret(CCG,Der), 
   copy_term(Der,Copy), !.

interpretDer(CCG,CCG).


/* =========================================================================
   Insert DRS
========================================================================= */

insertDRS(merge(B1,B2),New,Merge):-
   option('--theory',drt), !, 
   insertDRS(B2,New,B3), 
   Merge = merge(B1,B3).

insertDRS(Old,New,Merge):- 
   option('--theory',drt), !, 
   Merge = merge(Old,New).

insertDRS(Old,New,SDRS):- 
   option('--theory',sdrt), !, 
   Rel = continuation,
%  Rel = elaboration,
   mergeSDRS(smerge(Old,New,Rel,[]),SDRS).


/* =========================================================================
   Build one DRS for derivation N
========================================================================= */

build(N,UDRS,Tags,Start,End):-
   preferred(N,CCG0),
   preprocess(N,CCG0,CCG1,Tags,Start,End),
   resolve_relations(CCG1,CCG2),
   interpret(CCG2,Der), 
   topsem(Der,Sem),
   topcat(Der,Cat),
%  topstr(Der,Words), write(Words), nl,
   closure(Cat,Sem,Closed),
   betaConvert(Closed,UDRS), !.


/* =========================================================================
   Analysis failed for derivation N
========================================================================= */

noanalysis(N):-
   preferred(N,_), !,
   warning('no semantics for sentence ~p',[N]).
 
noanalysis(N):-
   warning('no syntax for sentence ~p',[N]).


/* =========================================================================
   Make Equivalence Classes (reference resolution)
========================================================================= */

addEquivClass(I,X,E1-E3):-
   select(X:C1,E1,E2), !,
   append(I,C1,C2),
   E3 = [X:C2|E2].

addEquivClass(I,X,E1-E2):-
   E2 = [X:I|E1].


/* =========================================================================
   Equivalence Classes (reference resolution)
========================================================================= */

equivalenceClasses(sdrs([],_),E-E):- !.

equivalenceClasses(sdrs([B|L],C),E1-E3):- !,
   equivalenceClasses(B,E1-E2),
   equivalenceClasses(sdrs(L,C),E2-E3).

equivalenceClasses(merge(B1,B2),E1-E3):- !,
   equivalenceClasses(B1,E1-E2),
   equivalenceClasses(B2,E2-E3).

equivalenceClasses(lab(_,B),E1-E2):- !,
   equivalenceClasses(B,E1-E2).

equivalenceClasses(sub(B1,B2),E1-E3):- !,
   equivalenceClasses(B1,E1-E2),
   equivalenceClasses(B2,E2-E3).

equivalenceClasses(_:drs([],[]),E-E):- !.

equivalenceClasses(B:drs([],[_:I:pred(X,_,_,_)|C]),E1-E3):- !,
   addEquivClass(I,X,E1-E2),
   equivalenceClasses(B:drs([],C),E2-E3).

equivalenceClasses(B:drs([],[_:I:named(X,_,_,_)|C]),E1-E3):- !,
   addEquivClass(I,X,E1-E2),
   equivalenceClasses(B:drs([],C),E2-E3).

equivalenceClasses(B:drs([],[_:I:timex(X,_)|C]),E1-E3):- !,
   addEquivClass(I,X,E1-E2),
   equivalenceClasses(B:drs([],C),E2-E3).

equivalenceClasses(B:drs([],[_:I:card(X,_,_)|C]),E1-E3):- !,
   addEquivClass(I,X,E1-E2),
   equivalenceClasses(B:drs([],C),E2-E3).

equivalenceClasses(B:drs([],[_:_:rel(_,_,_,_)|C]),E1-E2):- !,
   equivalenceClasses(B:drs([],C),E1-E2).

equivalenceClasses(B:drs([],[_:_:role(_,_,_,_)|C]),E1-E2):- !,
   equivalenceClasses(B:drs([],C),E1-E2).

equivalenceClasses(B:drs([],[_:_:eq(_,_)|C]),E1-E2):- !,
   equivalenceClasses(B:drs([],C),E1-E2).

equivalenceClasses(B:drs([],[_:_:not(DRS)|C]),E1-E3):- !,
   equivalenceClasses(DRS,E1-E2),
   equivalenceClasses(B:drs([],C),E2-E3).

equivalenceClasses(B:drs([],[_:_:pos(DRS)|C]),E1-E3):- !,
   equivalenceClasses(DRS,E1-E2),
   equivalenceClasses(B:drs([],C),E2-E3).

equivalenceClasses(B:drs([],[_:_:nec(DRS)|C]),E1-E3):- !,
   equivalenceClasses(DRS,E1-E2),
   equivalenceClasses(B:drs([],C),E2-E3).

equivalenceClasses(B:drs([],[_:_:prop(_,DRS)|C]),E1-E3):- !,
   equivalenceClasses(DRS,E1-E2),
   equivalenceClasses(B:drs([],C),E2-E3).

equivalenceClasses(B:drs([],[_:_:imp(DRS1,DRS2)|C]),E1-E4):- !,
   equivalenceClasses(DRS1,E1-E2),
   equivalenceClasses(DRS2,E2-E3),
   equivalenceClasses(B:drs([],C),E3-E4).

equivalenceClasses(B:drs([],[_:_:or(DRS1,DRS2)|C]),E1-E4):- !,
   equivalenceClasses(DRS1,E1-E2),
   equivalenceClasses(DRS2,E2-E3),
   equivalenceClasses(B:drs([],C),E3-E4).

equivalenceClasses(B:drs([],[_:_:duplex(_,DRS1,_,DRS2)|C]),E1-E4):- !,
   equivalenceClasses(DRS1,E1-E2),
   equivalenceClasses(DRS2,E2-E3),
   equivalenceClasses(B:drs([],C),E3-E4).

equivalenceClasses(B:drs([],[C|Conds]),E1-E2):- !,
   warning('condition not known in equivalenceClasses/2: ~p',[C]),
   equivalenceClasses(B:drs([],Conds),E1-E2).

equivalenceClasses(B:drs([_:I:X|D],C),E1-E3):- !,
   addEquivClass(I,X,E1-E2),
   equivalenceClasses(B:drs(D,C),E2-E3).

equivalenceClasses(B,E-E):- 
   warning('expression not known in equivalenceClasses/2: ~p',[B]).


/* =========================================================================
   Clean up Equivalence Classes (reference resolution)
========================================================================= */

cleanEquivClasses([],[]).

cleanEquivClasses([_:[]|L1],L2):- !,
   cleanEquivClasses(L1,L2).

cleanEquivClasses([_:[_]|L1],L2):- !,
   cleanEquivClasses(L1,L2).

cleanEquivClasses([X:C1|L1],[X:C2|L2]):- !,
   sort(C1,C2),
   cleanEquivClasses(L1,L2).


/* =========================================================================
   Produce Reference Resolution
========================================================================= */

writeECs([],_):- nl.
writeECs([I:X|L],T):- 
   write(I), write(': ['), writeEC(X,T), write('] '),
   writeECs(L,T).

writeEC([],_).
writeEC([X|L],T):- 
   member(X:F,T), member(tok:Tok,F), !,
   write(Tok), 
   (L=[];L=[_|_],write(',')),
   writeEC(L,T).
writeEC([_|L],T):- 
   writeEC(L,T).


evaluate(Es,_Tags):-
%  write('equivalent: '), writeECs(Es,Tags),
   setof(ant(A,B),R^(resolveDRT:antecedent([A|R],B)),L), !,
   evaluate(L,Es,0,0,0).

evaluate(_,_).

evaluate([],_,Correct,_TotalAttempted,Total):- 
   Correct < Total, !,
   warning('not all gold standard antecedents found, but only: ~p/~p',[Correct,Total]),
   true.
%   format('precision: ~p/~p~n',[Correct,TotalAttempted]),
%   format('recall: ~p/~p~n',[Correct,Total]).


evaluate([],_,_Correct,_TotalAttempted,_Total):- !,
   true.
%   format('precision: ~p/~p~n',[Correct,TotalAttempted]),
%   format('recall: ~p/~p~n',[Correct,Total]).

evaluate([ant(A,B)|L],Es,C1,TA1,TT1):-
   member(_:E,Es), 
   member(A,E),
   member(B,E), !,
   C2 is C1 + 1,
   TA2 is TA1 + 1,
   TT2 is TT1 + 1,
   evaluate(L,Es,C2,TA2,TT2).   

evaluate([ant(A,_)|L],Es,C,TA1,TT1):-
   member(_:E,Es), 
   member(A,E), !,
   TA2 is TA1 + 1,
   TT2 is TT1 + 1,
   evaluate(L,Es,C,TA2,TT2).   

evaluate([_|L],Es,C,TA,TT1):-
   TT2 is TT1 + 1,
   evaluate(L,Es,C,TA,TT2).   


/* =========================================================================
   Produce Semantic Representation
========================================================================= */

semantics(X,Tags,Y):-
   option('--instantiate',true), 
   option('--semantics',  pdrs), 
   option('--resolve',    true), !, 
   instDrs(X), Y=X,
   equivalenceClasses(Y,[]-E1),
   cleanEquivClasses(E1,E2), 
   evaluate(E2,Tags).

semantics(X,_,Y):-
   option('--instantiate',true), 
   option('--semantics',pdrs), !, 
   instDrs(X), Y=X.

semantics(X,_,Y):-
   option('--semantics',pdrs), !, 
   Y=X.

semantics(X,Tags,Y):-
   option('--semantics',drg), !, 
   instDrs(X,N), tuples(Tags,X,N,Y).

semantics(A,_,B):-
   option('--instantiate',true), option('--semantics',drs), !, 
   instDrs(A), pdrs2drs(A,B).

semantics(A,_,B):-
   option('--semantics',drs), !, 
   pdrs2drs(A,B).

semantics(A,_,C):-
   option('--semantics',fol), !, 
   pdrs2drs(A,B), drs2fol(B,C).

semantics(A,Tags,C):-
   option('--semantics',tacitus), !, 
   instDrs(A,N), pdrs2drs(A,B), drs2tac(B,Tags,N,C).

semantics(A,Tags,C):-
   option('--semantics',amr), !, 
%   instDrs(A,N), pdrs2drs(A,B), drs2amr(B,Tags,N,C).
   instDrs(A,N), drs2amr(A,Tags,N,C).


/* -------------------------------------------------------------------------
   Forward Application
------------------------------------------------------------------------- */

interpret(fa(Cat,_,Att,W,F1,A1),fa(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=app(F2,A2).      


/* -------------------------------------------------------------------------
   Backward Application
------------------------------------------------------------------------- */

interpret(ba(Cat,_,Att,W,A1,F1),ba(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   Sem=app(F2,A2).


/* -------------------------------------------------------------------------
   Forward Composition
------------------------------------------------------------------------- */

interpret(fc(Cat,_,Att,W,F1,A1),fc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Composition
------------------------------------------------------------------------- */

interpret(bc(Cat,_,Att,W,A1,F1),bc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Cross Composition
------------------------------------------------------------------------- */

interpret(fxc(Cat,_,Att,W,F1,A1),fxc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Cross Composition
------------------------------------------------------------------------- */

interpret(bxc(Cat,_,Att,W,A1,F1),bxc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Substitution
------------------------------------------------------------------------- */

interpret(fs(Cat,_,Att,W,F1,A1),fs(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Substitution
------------------------------------------------------------------------- */

interpret(bs(Cat,_,Att,W,A1,F1),bs(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(F2,F2),
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Cross Substitution
------------------------------------------------------------------------- */

interpret(fxs(Cat,_,Att,W,F1,A1),fxs(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Cross Substitution
------------------------------------------------------------------------- */

interpret(bxs(Cat,_,Att,W,A1,F1),bxs(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Generalised Forward Composition
------------------------------------------------------------------------- */

interpret(gfc(Cat,N,_,Att,W,F1,A1),gfc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   gen(N,F2,A2,Sem).

interpret(gfc(Cat,S,A,W,F1,A1),Der):-
   topcat(F1,S1/S2),
   topcat(A1,ACat),
   base(ACat,S2/S3,Dollar,N),
   base(Cat,S1/S3,Dollar,N), !,
   interpret(gfc(Cat,N,S,A,W,F1,A1),Der).   


/* -------------------------------------------------------------------------
   Generalised Backward Composition
------------------------------------------------------------------------- */

interpret(gbc(Cat,N,_,Att,W,A1,F1),gbc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   gen(N,F2,A2,Sem).

interpret(gbc(Cat,S,A,A1,W,F1),Der):- 
   topcat(F1,S1\S2),
   topcat(A1,ACat),
   base(ACat,S2\S3,Dollar,N),
   base(Cat,S1\S3,Dollar,N), !,
   interpret(gbc(Cat,N,S,A,A1,W,F1),Der).


/* -------------------------------------------------------------------------
   Generalised Forward Cross Composition
------------------------------------------------------------------------- */

interpret(gfxc(Cat,N,_,Att,W,F1,A1),gfxc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   gen(N,F2,A2,Sem).

interpret(gfxc(Cat,S,A,W,F1,A1),Der):-
   topcat(F1,S1/S2),
   topcat(A1,ACat),
   base(ACat,S2\S3,Dollar,N),
   base(Cat,S1\S3,Dollar,N), !,
   interpret(gfxc(Cat,N,S,A,W,F1,A1),Der).


/* -------------------------------------------------------------------------
   Generalised Backward Cross Composition
------------------------------------------------------------------------- */

interpret(gbxc(Cat,N,_,Att,W,A1,F1),gbxc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   gen(N,F2,A2,Sem).

interpret(gbxc(Cat,S,A,W,A1,F1),Der):- 
   topcat(F1,S1\S2),
   topcat(A1,ACat),
   base(ACat,S2/S3,Dollar,N),
   base(Cat,S1/S3,Dollar,N), !,
   interp(gbxc(Cat,N,S,A,W,A1,F1),Der).

/* -------------------------------------------------------------------------
   Token
------------------------------------------------------------------------- */

%interp(t(Cat,_Word,_,Att,I),Sem,nil):-
%   \+ option('--semantics',der),
%   att(Att,lemma,Lemma),
%   downcase_atom(Lemma,Symbol),
%   semlex(Cat,Symbol,[I],Att-_,Sem), !.

interpret(t(Cat,Word,_,Att1,I),t(Cat,Word,Sem,Att2,I)):-
%  option('--semantics',der), 
   att(Att1,lemma,Lemma),
   downcase_atom(Lemma,Symbol),
   semlex(Cat,Symbol,[I],Att1-Att2,Sem), !.


/* -------------------------------------------------------------------------
   Type Changing Rules
------------------------------------------------------------------------- */

interpret(tc(NewCat,OldCat,_,Att,W,A1),tc(NewCat,OldCat,A3,Att,W,D)):- !,
   interpret(A1,D), topsem(D,A2),
   typechange(OldCat,A2,Att,NewCat,A3).


/* -------------------------------------------------------------------------
   Type Raising
------------------------------------------------------------------------- */

interpret(ftr(NewCat,OldCat,_,Att,W,A1),ftr(NewCat,OldCat,Sem,Att,W,D)):- !,
   interpret(A1,D), topsem(D,A2),
   Sem = lam(X,app(X,A2)).

interpret(btr(NewCat,OldCat,_,Att,W,A1),btr(NewCat,OldCat,Sem,Att,W,D)):- !,
   interpret(A1,D), topsem(D,A2),
   Sem = lam(X,app(X,A2)).


/* -------------------------------------------------------------------------
   Coordination (a la Steedman)
------------------------------------------------------------------------- */

interpret(coord(Cat,_,Att,W,L1,C1,R1),coord(Cat,Sem,Att,W,D1,D2,D3)):- !,
   argCard(Cat,N),
   coordMacro(N,Coord),
   interpret(L1,D1), topsem(D1,L2),
   interpret(C1,D2), topsem(D2,C2),
   interpret(R1,D3), topsem(D3,R2),
   Sem = app(app(app(Coord,C2),R2),L2).


/* -------------------------------------------------------------------------
   Apposition (a la Hockenmaier)
------------------------------------------------------------------------- */

interpret(conj(Cat,np,_,Att,W,C1,R1),conj(Cat,np,Sem,Att,W,D1,D2)):-
   topcat(C1,conj:app), 
   topcat(R1,np), 
   interpret(C1,D1), topsem(D1,C2),
   interpret(R1,D2), !, topsem(D2,R2),
   Sem = app(C2,R2).


/* -------------------------------------------------------------------------
   Dedicated coordination
------------------------------------------------------------------------- */

interpret(conj(Cat,CCat,_,Att,W,C1,R1),conj(Cat,CCat,Sem,Att,W,D1,D2)):-  
   topcat(C1,conj:CCat), 
   topcat(R1,CCat), 
   interpret(C1,D1), topsem(D1,C2),
   interpret(R1,D2), !, topsem(D2,R2),
   Sem = app(C2,R2).


/* -------------------------------------------------------------------------
   Coordination (a la Hockenmaier)
------------------------------------------------------------------------- */

interpret(conj(Cat,CCat,_,Att,W,C1,R1),conj(Cat,CCat,Sem,Att,W,D1,D2)):- !,
   argCard(CCat,N),
   coordMacro(N,Coord),
   interpret(C1,D1), topsem(D1,C2),            % conjunctor
   interpret(R1,D2), topsem(D2,R2),            % right conjunct
   Sem = app(app(Coord,C2),R2).


/* -------------------------------------------------------------------------
   Warning Messages
------------------------------------------------------------------------- */

interpret(Input,_,_):-
   Input = t(Cat,Word,_,Att,Index),
   error('no lexical semantics for cat ~p (token: ~p), (attributes: ~p) (index: ~p)',[Cat,Word,Att,Index]), 
   !, fail.

interpret(Input,_,_):-
   error('no interpretation rule for ~p',[Input]), 
   !, fail.


/* =========================================================================
   Semantics for Generalised Rules
========================================================================= */

gen(1,F,A,lam(X1,app(F,app(A,X1)))).
gen(2,F,A,lam(X1,lam(X2,app(F,app(app(A,X1),X2))))).
gen(3,F,A,lam(X1,lam(X2,lam(X3,app(F,app(app(app(A,X1),X2),X3)))))).
gen(4,F,A,lam(X1,lam(X2,lam(X3,lam(X4,app(F,app(app(app(app(A,X1),X2),X3),X4))))))).

    
/*=============================================================
   Base Categories
=============================================================*/

base(Cat,Cat,[],1):- !.

base(Cat/Riht,Base,[riht(Riht)|A],N):- !, 
   base(Cat,Base,A,M), N is M + 1.

base(Cat\Left,Base,[left(Left)|A],N):- !, 
   base(Cat,Base,A,M), N is M + 1.


/* =========================================================================
   Wrapper to choose lemma or word

semlex(Cat,Word,_Lemma,Index,Att1-Att2,Sem):-
   att(Att1,pos,Pos), member(Pos,['NNP','NNPS']), !,
   downcase_atom(Word,Sym),
   semlex(Cat,Sym,Index,Att1-Att2,Sem).

semlex(Cat,_Word,Lemma,Index,Att1-Att2,Sem):-
   downcase_atom(Lemma,Sym),
   semlex(Cat,Sym,Index,Att1-Att2,Sem).
========================================================================= */
