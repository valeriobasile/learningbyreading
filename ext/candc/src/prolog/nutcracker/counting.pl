
:- module(counting,[countingAxioms/2]).

/*========================================================================
    Load Modules
========================================================================*/

:- use_module(semlib(options),[option/2]).


/* =======================================================================
   Get Background Knowledge (Counting)
========================================================================*/

countingAxioms(F,F):-
   option('--plural',false), !.

countingAxioms(F,BK):-
   option('--plural',true), !,

   BK = [A0,A1,A2,A3,A4,A5,A6,A7,A8|F],

   A0 = all(X,all(Y,imp(subset(X,Y),all(Z,imp(member(Z,X),member(Z,Y)))))),

   A1 = all(X,all(Y,imp(member(X,Y),and(singular(X),collection(Y))))),
    
   A2 = all(X,all(Y,imp(subset(X,Y),and(collection(X),collection(Y))))),

   A3 = all(X,all(Y,all(Z,imp(and(subset(X,Y),subset(Y,Z)),subset(X,Z))))),

   A4 = all(X,imp(singular(X),not(collection(X)))),

   A5 = all(X,imp(one(X),not(two(X)))),

   A6 = all(X,imp(collection(X),or(one(X),two(X)))),

   A7 = all(X,imp(singular(X),some(Z,and(collection(Z),member(X,Z))))),

   A8 = all(X,all(Y,imp(and(and(collection(X),one(X)),
                            and(and(collection(Y),one(Y)),
                                and(not(eq(X,Y)),
                                    and(not(subset(X,Y)),not(subset(Y,X)))))),
                        some(Z,and(collection(Z),
                                   and(subset(X,Z),
                                       and(two(Z),
                                           and(subset(Y,Z),
                                               all(U,imp(member(U,Z),or(member(U,X),member(U,Y)))))))))))).

/*

   A6 = all(X,imp(two(X),some(Y1,some(Y2,and(member(Y1,X),and(member(Y2,X),and(not(eq(Y1,Y2)),all(Z,imp(member(Z,X),or(eq(Z,Y1),eq(Z,Y2))))))))))).

   A2 = all(X,imp(at_least_2(X),
                  some(Y1,and(member(Y1,X),
                              some(Y2,and(member(Y2,X),not(eq(Y1,Y2)))))))),

   A3 = all(X,imp(at_least_3(X),
                  some(Y,and(and(at_least_2(Y),subset(Y,X)),
                             some(Z,and(member(Z,X),not(member(Z,Y)))))))),

   A4 = all(X,imp(at_least_4(X),
                  some(Y,and(and(at_least_3(Y),subset(Y,X)),
                             some(Z,and(member(Z,X),not(member(Z,Y)))))))),

   A5 = all(X,imp(at_least_5(X),
                  some(Y,and(and(at_least_4(Y),subset(Y,X)),
                             some(Z,and(member(Z,X),not(member(Z,Y)))))))),

   A6 = all(X,imp(at_least_6(X),
                  some(Y,and(and(at_least_5(Y),subset(Y,X)),
                             some(Z,and(member(Z,X),not(member(Z,Y)))))))),

   A7 = all(X,imp(at_least_7(X),
                  some(Y,and(and(at_least_6(Y),subset(Y,X)),
                             some(Z,and(member(Z,X),not(member(Z,Y)))))))).
*/


