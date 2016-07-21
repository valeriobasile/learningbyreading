
/*========================================================================
   File Search Paths
========================================================================*/

file_search_path(semlib,     'src/prolog/lib').
file_search_path(nutcracker, 'src/prolog/nutcracker').
file_search_path(boxer,      'src/prolog/boxer').
file_search_path(knowledge,  'src/prolog/boxer/knowledge').

/*========================================================================
   Load other libraries
========================================================================*/

:- use_module(library(lists),[select/3,append/3]).
:- use_module(boxer(slashes)).
:- use_module(boxer(printDrs),[printDrs/2]).
:- use_module(nutcracker(callInference),[callTPandMB/7]).
:- use_module(semlib(options),[option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).

:- dynamic chart/5.

/* ----------------------------------------------------------------------------------
   Simple Lexicon
---------------------------------------------------------------------------------- */

lex(every,  (s/(s\np))/n, (x1-C)-((((x1-B)-drs([],[imp(drs([[]:x1],[pos(C)]),B)]))-D)-D) ).

lex(two,    (s/(s\np))/n, (x1-C)-((((x1-B)-drs([[]:x2],[pred(x2,two,n,1),imp(drs([[]:x1],[rel(x1,x2,member,1)]),C),imp(drs([[]:x1],[rel(x1,x2,member,1)]),B)]))-D)-D) ).
lex(three,  (s/(s\np))/n, (x1-C)-((((x1-B)-drs([[]:x2],[pred(x2,three,n,1),imp(drs([[]:x1],[rel(x1,x2,member,1)]),C),imp(drs([[]:x1],[rel(x1,x2,member,1)]),B)]))-D)-D) ).
lex(four,   (s/(s\np))/n, (x1-C)-((((x1-B)-drs([[]:x2],[pred(x2,four,n,1),imp(drs([[]:x1],[rel(x1,x2,member,1)]),C),imp(drs([[]:x1],[rel(x1,x2,member,1)]),B)]))-D)-D) ).

lex(a,      np/n, (x1-C)-((x1-B)-drs([[]:x1],[pos(C),pos(B)]))           ).

lex(big,    n/n,  (x1-B)-(x1-drs([],[pred(x1,big,a,0),pos(B)]))                ).

lex(man,    n,    x1-drs([],[pred(x1,man,n,0)])                                ).
lex(men,    n,    x1-drs([],[pred(x1,man,n,0)])                                ).
lex(woman,  n,    x1-drs([],[pred(x1,woman,n,0)])                              ).

lex(someone, np,   (x1-B)-drs([[]:x1],[pred(x1,person,n,1),pos(B)])  ).

lex(herself, np,   (x1-B)-drs([],[pred(x1,female,n,0),pos(B)])  ).

lex(mia,    np,   (x1-B)-drs([[]:x1],[named(x1,mia,per,0),pos(B)])  ).
lex(lou,    np,   (x1-B)-drs([[]:x1],[named(x1,lou,per,0),pos(B)])  ).
lex(andy,   np,   (x1-B)-drs([[]:x1],[named(x1,andy,per,0),pos(B)]) ).

lex(mia,    s/(s\np),   ((x1-B)-drs([[]:x1],[named(x1,mia,per,0),pos(B)])-C)-C      ).

lex(everyone, s/(s\np),   ((x1-B)-drs([],[imp(drs([[]:x1],[pred(x1,person,n,1)]),B)])-C)-C      ).

lex(slowly, (s\np)\(s\np), ((x2-drs([],[pred(x2,slow,a,0),pos(C)]))-B)-((x2-C)-B) ).
lex(slowly, s\s, ((x2-drs([],[pred(x2,slow,a,0),pos(C)]))-B)-((x2-C)-B) ).

lex(smokes, s\np, (((x1-drs([[]:x2],[pred(x2,smoke,v,0),rel(x2,x1,theme,0),pos(B)]))-C)-((x2-B)-C))         ).
lex(smoke,  s\np, (((x1-drs([[]:x2],[pred(x2,smoke,v,0),rel(x2,x1,theme,0),pos(B)]))-C)-((x2-B)-C))         ).

lex(saw,    (s\np)/np, ((x1-drs([],[rel(x2,x1,theme,0),pos(D)]))-B)-(((x1-drs([[]:x2],[pred(x2,see,v,0),rel(x2,x1,agent,0),pos(B)]))-C)-((x2-D)-C)) ).

lex(and,    (np/np)\np, ((x1-drs([],[rel(x1,x2,member,1)]))-B1)-(((x1-drs([],[rel(x1,x2,member,1)]))-B2)-((x1-B)-drs([[]:x2],[pos(B1),pos(B2),imp(drs([[]:x1],[rel(x1,x2,member,1)]),B)])))).

lex('.',    t\s,   ((x2-drs([],[pred(x2,event,v,0)]))-B)-B  ).

/* ----------------------------------------------------------------------------------
   Combinatory Rules
---------------------------------------------------------------------------------- */

combine(A,B,C,D,E,fa(D,E)):- fa(A,B,C).
combine(A,B,C,D,E,ba(D,E)):- ba(A,B,C).
combine(A,B,C,D,E,fc(D,E)):- fc(A,B,C).

/* ----------------------------------------------------------------------------------
   CCG
---------------------------------------------------------------------------------- */

fa((A/B):X-Y, B:X, A:Y).
ba(A:X, (B\A):X-Y, B:Y).
fc((A/B):X-Y, (B/C):Z-X, (A/C):Z-Y).


/* ----------------------------------------------------------------------------------
   Examples
---------------------------------------------------------------------------------- */

example( 1, [mia,smokes,'.']).
example( 2, [two,men,smoke,'.']).
example( 3, [every,man,smokes,'.']).
example( 4, [a,woman,smokes,'.']).
example( 5, [mia,saw,a,man,'.']).
example( 6, [every,big,man,smokes,'.']).
example( 7, [every,big,man,saw,a,big,man,'.']).
example( 8, [two,men,saw,a,woman,'.']).
example( 9, [three,men,saw,a,woman,'.']).
example(10, [four,men,saw,a,woman,'.']).

example(20, [lou,and,andy,smoke,'.']).
example(21, [lou,and,andy,saw,a,woman,'.']).

example(31, [two,men,smoke,'.']).
example(32, [a,man,smokes,'.']).
example(33, [three,men,smoke,'.']).

example(41, [mia,smokes,slowly,'.']).
example(42, [a,man,smokes,slowly,'.']).
example(43, [every,man,smokes,slowly,'.']).
example(44, [mia,saw,a,man,slowly,'.']).
example(45, [every,woman,saw,a,man,slowly,'.']).

example(101,[smokes,mia,'.']).
example(102,[mia,saw,every,man,'.']).


/* ----------------------------------------------------------------------------------
   Test
---------------------------------------------------------------------------------- */

example(Example):-
   setDefaultOptions(nutcracker), 
   example(Example,Sentence),
   retractall(chart(_,_,_,_,_)),
   initChart(Sentence,0,N),
   parse(1,1,N,_), !.

test(Sentence):-
   setDefaultOptions(nutcracker), 
   retractall(chart(_,_,_,_,_)),
   initChart(Sentence,0,N),
   parse(1,1,N,_), !.

/* ----------------------------------------------------------------------------------
   RTE
---------------------------------------------------------------------------------- */

rte(E1,E2):-
   setDefaultOptions(nutcracker), 
   setOption(nutcracker,'--tp',vampire),
   setOption(nutcracker,'--mb',paradox3),
   example(E1,S1),
   retractall(chart(_,_,_,_,_)),
   initChart(S1,0,N1),
   parse(1,1,N1,F1),
   example(E2,S2),
   retractall(chart(_,_,_,_,_)),
   initChart(S2,0,N2),
   parse(1,1,N2,F2),
   inference(not(imp(F1,F2))).


/* ----------------------------------------------------------------------------------
   Chart Init
---------------------------------------------------------------------------------- */

initChart([],X,X).

initChart([Token|L],X,N):-
   Y is X + 1,
   lex(Token,Cat,Sem),
   assert(chart(X,Y,Cat,Sem,lex(Cat,Token))),
   initChart(L,Y,N).

/* ----------------------------------------------------------------------------------
   Parsing (CYK)
---------------------------------------------------------------------------------- */

parse(Start,K,End,FOL):- 
   K > End, 
   chart(Start,End,Cat,Sem,Der), !,
 write(chart(Start,End,Cat,Sem,Der)),nl,
   output(Cat,Sem,Der,FOL).

parse(M,K,L,F):-
   L is M+K, !,
   parse(M,L),
   KInc is K+1,
   parse(0,KInc,L,F).

parse(M,K,L,F):-                    
   N is M+K,                      
   parse(M,N),
   MInc is M+1,
   parse(MInc,K,L,F).


/* ----------------------------------------------------------------------------------
   Output
---------------------------------------------------------------------------------- */

output(Cat,Sem,Der,FOL):-
   write(cat:Cat),nl,
   write(sem:Sem),nl,
   reduce(Sem,Drs), !,
   printDrs(user_output,Drs),
   write(der:Der),nl,
   drs2fol(Drs,FOL),
   inference(FOL).

/* ----------------------------------------------------------------------------------
   Combine
---------------------------------------------------------------------------------- */

parse(M,L):-
   chart(M,X,Cat1,Sem1,Der1),
   chart(X,L,Cat2,Sem2,Der2),
   combine(Cat1:Sem1,Cat2:Sem2,Cat3:Sem3,Der1,Der2,Der3), !,
   assert(chart(M,L,Cat3,Sem3,Der3)).

parse(_,_).


/* ----------------------------------------------------------------------------------
   Inference
---------------------------------------------------------------------------------- */
   
inference(FOL):-
   MaxDomSize=20,

   A10  = all(X,all(Y,imp(member(X,Y),and(individual(X),collection(Y))))),
   A11  = all(B,imp(two(B),and(some(A,firstmember(A,B)),some(A,secondmember(A,B))))),
   A12  = all(B,imp(three(B),and(some(A,firstmember(A,B)),and(some(A,secondmember(A,B)),some(A,thirdmember(A,B)))))),
   A13  = all(B,imp(four(B),and(some(A,firstmember(A,B)),and(some(A,secondmember(A,B)),and(some(A,thirdmember(A,B)),some(A,fourthmember(A,B))))))),

   A14  = all(X,all(Y,imp(firstmember(X,Y),member(X,Y)))),
   A15  = all(X,all(Y,imp(secondmember(X,Y),member(X,Y)))),
   A16  = all(X,all(Y,imp(thirdmember(X,Y),member(X,Y)))),
   A17  = all(X,all(Y,imp(fourthmember(X,Y),member(X,Y)))),

%   A15  = all(X,imp(and(individual(X),some(Y,and(individual(Y),not(eq(X,Y))))),some(Y,and(two(Y),member(X,Y))))),
%   A15  = all(X,all(Y,imp(and(individual(X),and(individual(Y),not(eq(X,Y)))),some(Z,and(two(Z),and(member(X,Z),member(Y,Z))))))),
%   A16  = all(X,all(Y,imp(and(two(Y),member(X,Y)),or(firstmember(X,Y),secondmember(X,Y))))),

%   A1 = and(A10,and(A11,and(A12,and(A13,and(A14,and(A15,and(A16,and(A17,and(A18,A19))))))))),
   A1 = and(A10,and(A11,and(A12,and(A13,and(A14,and(A15,and(A16,A17))))))),

   A21  = all(X,imp(individual(X),not(collection(X)))),
   A22  = all(X,imp(individual(X),not(event(X)))),
   A23  = all(X,imp(collection(X),not(event(X)))),
   A24  = all(X,imp(man(X),not(woman(X)))),
   A25  = all(X,imp(man(X),human(X))),
   A26  = all(X,imp(woman(X),human(X))),
   A27  = all(X,imp(human(X),individual(X))),
   A28  = all(X,imp(smoke(X),event(X))),
   A29  = all(X,imp(see(X),event(X))),
   A2 = and(A21,and(A22,and(A23,and(A24,and(A25,and(A26,and(A27,and(A28,A29)))))))),

   A31 = all(X,imp(two(X),collection(X))),
   A32 = all(X,imp(three(X),collection(X))),
   A33 = all(X,imp(four(X),collection(X))),
   A3 = and(A31,and(A32,A33)),

   A40  = all(X,imp(two(X),not(some(Y,and(firstmember(Y,X),secondmember(Y,X)))))),
   A41  = all(X,imp(three(X),not(some(Y,and(firstmember(Y,X),thirdmember(Y,X)))))),
   A42  = all(X,imp(three(X),not(some(Y,and(secondmember(Y,X),thirdmember(Y,X)))))),
   A43  = all(X,imp(three(X),not(some(Y,and(firstmember(Y,X),secondmember(Y,X)))))),
   A44  = all(X,imp(four(X),not(some(Y,and(firstmember(Y,X),secondmember(Y,X)))))),   
   A45  = all(X,imp(four(X),not(some(Y,and(firstmember(Y,X),thirdmember(Y,X)))))),
   A46  = all(X,imp(four(X),not(some(Y,and(firstmember(Y,X),fourthmember(Y,X)))))),
   A47  = all(X,imp(four(X),not(some(Y,and(secondmember(Y,X),thirdmember(Y,X)))))),
   A48  = all(X,imp(four(X),not(some(Y,and(secondmember(Y,X),fourthmember(Y,X)))))),
   A49  = all(X,imp(four(X),not(some(Y,and(thirdmember(Y,X),fourthmember(Y,X)))))),
   A4 = and(A40,and(A41,and(A42,and(A43,and(A44,and(A45,and(A46,and(A47,and(A48,A49))))))))),

   A51  = all(X,imp(lou(X),not(andy(X)))),
   A52  = all(X,imp(lou(X),man(X))),
   A53  = all(X,imp(andy(X),man(X))),
   A54  = all(X,imp(mia(X),woman(X))),
   A5 = and(A51,and(A52,and(A53,A54))),

   A60  = all(X,imp(and(individual(X),some(Y,and(individual(Y),not(eq(Y,X))))),some(Y,and(two(Y),member(X,Y))))),
   A61  = all(X,imp(and(individual(X),some(Y,and(two(Y),not(member(X,Y))))),some(Y,and(three(Y),member(X,Y))))),
   A62  = all(X,imp(and(individual(X),some(Y,and(three(Y),not(member(X,Y))))),some(Y,and(four(Y),member(X,Y))))),

   A63  = all(X,imp(two(X),all(Y,imp(member(Y,X),or(firstmember(Y,X),secondmember(Y,X)))))),
   A64  = all(X,imp(three(X),all(Y,imp(member(Y,X),or(firstmember(Y,X),or(secondmember(Y,X),thirdmember(Y,X))))))),
   A65  = all(X,imp(four(X),all(Y,imp(member(Y,X),or(firstmember(Y,X),or(secondmember(Y,X),or(thirdmember(Y,X),fourthmember(Y,X)))))))),

   A66  = all(X,all(Y,imp(some(C,and(firstmember(X,C),firstmember(Y,C))),eq(X,Y)))),
   A67  = all(X,all(Y,imp(some(C,and(secondmember(X,C),secondmember(Y,C))),eq(X,Y)))),
   A68  = all(X,all(Y,imp(some(C,and(thirdmember(X,C),thirdmember(Y,C))),eq(X,Y)))),
   A69  = all(X,all(Y,imp(some(C,and(fourthmember(X,C),fourthmember(Y,C))),eq(X,Y)))),

   A6 = and(A60,and(A61,and(A62,and(A63,and(A64,and(A65,and(A66,and(A67,and(A68,A69))))))))),

   BK = and(A1,and(A2,and(A3,and(A4,and(A5,A6))))),
   F = and(BK,FOL),
   callTPandMB([],not(F),F,1,MaxDomSize,Model,Engine),

   write(engine:Engine),nl,
   printModel(Model,user_output),nl.


/* ----------------------------------------------------------------------------------
   POS reduction
---------------------------------------------------------------------------------- */

reduce(drs(D,Conds),Reduced):-
   select(pos(drs([],C1)),Conds,C2), !,
   append(C1,C2,C3),
   reduce(drs(D,C3),Reduced).

reduce(drs(D,C1),drs(D,C2)):- !,
   reduce(C1,C2).

reduce([],[]):- !.

reduce([pos(B1)|L1],[pos(B2)|L2]):- !,
  reduce(B1,B2),
  reduce(L1,L2).

reduce([imp(A1,B1)|L1],[imp(A2,B2)|L2]):- !,
  reduce(A1,A2),
  reduce(B1,B2),
  reduce(L1,L2).

reduce([or(A1,B1)|L1],[or(A2,B2)|L2]):- !,
  reduce(A1,A2),
  reduce(B1,B2),
  reduce(L1,L2).

reduce([C|L1],[C|L2]):- !,
  reduce(L1,L2).


/* ----------------------------------------------------------------------------------
   drs2fol
---------------------------------------------------------------------------------- */

drs2fol(drs([],[Cond]),Formula):- !, cond2fol(Cond,Formula).

drs2fol(drs([],[Cond1,Cond2|Conds]),and(Formula1,Formula2)):- !,
   cond2fol(Cond1,Formula1), drs2fol(drs([],[Cond2|Conds]),Formula2).

drs2fol(drs([_:X|Referents],Conds),some(V,Formula)):-
   ref2var(X,V), 
   drs2fol(drs(Referents,Conds),Formula).


/* ========================================================================
   Referents to variable
=========================================================================*/

ref2var(x1,'X'):- !.
ref2var(x2,'Y'):- !.


/* ========================================================================
   Translate DRS-Conditions into FOL formulas 
=========================================================================*/

cond2fol(_:C,F):- !,
   cond2fol(C,F).

cond2fol(not(Drs),not(Formula)):- !,
   drs2fol(Drs,Formula).
 
cond2fol(pos(Drs),Formula):- !,
   drs2fol(Drs,Formula).

cond2fol(or(Drs1,Drs2),or(Formula1,Formula2)):- !,
   drs2fol(Drs1,Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(imp(drs(D,C),B),Formula):- !,
   cond2fol(not(drs(D,[not(B)|C])),Formula).

cond2fol(named(X,N1,Type,Sense),F):-
   symbol(Type,N1,Sense,N2), !,
   ref2var(X,V),
   F=..[N2,V].

cond2fol(eq(X,Y),eq(V1,V2)):- 
   ref2var(X,V1),
   ref2var(Y,V2).

cond2fol(pred(X,Sym1,Type,Sense),F):- 
   symbol(Type,Sym1,Sense,Sym2), !,
   ref2var(X,V), F=..[Sym2,V].

cond2fol(rel(X,Y,Sym1,Sense),F):- 
   symbol(r,Sym1,Sense,Sym2), !,
   ref2var(X,V1), ref2var(Y,V2),  F=..[Sym2,V1,V2].


/*========================================================================
   Ensure F is a number or atom
========================================================================*/

symbol(Type,F1,0,F2):- !, symbol(Type,F1,1,F2).
symbol(_Type,F1,_Sense,F2):- F1=F2.


/* =======================================================================
   Print Model
========================================================================*/

printModel(model(D,[]),Stream):- !,
   format(Stream,'model(~p, [])',[D]).

printModel(model(D,[F]),Stream):- !,
   format(Stream,'model(~p,~n  [~p])',[D,F]).

printModel(model(D,[X,Y|F]),Stream):- !,
   sort([X,Y|F],[First|Sorted]),
   format(Stream,'model(~p,~n  [~p,~n',[D,First]),
   printModel(Sorted,Stream).

printModel([Last],Stream):- !,
   format(Stream,'   ~p])',[Last]).

printModel([X|L],Stream):- !,
   format(Stream,'   ~p,~n',[X]),
   printModel(L,Stream).

printModel(Model,Stream):-
   write(Stream,Model).

