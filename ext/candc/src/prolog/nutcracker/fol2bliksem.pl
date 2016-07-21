
:- module(fol2bliksem,[fol2bliksem/2,
                       fol2bliksem/3]).

:- use_module(nutcracker(fol2tptp),[printArgs/2]).


/* ========================================================================
   Translates formula to otter syntax on Stream
======================================================================== */

fol2bliksem(Formula,Stream):-
   format(Stream,'~nAuto.~n~n',[]),
   printBliksemFormula(Stream,Formula).

fol2bliksem(Axioms,Formula,Stream):-
   format(Stream,'~nAuto.~n~n',[]),
   printBliksemFormulas(Axioms,Stream),
   printBliksemFormula(Stream,Formula).


/* ========================================================================
   Print a Bliksem formula (introducing tab)
======================================================================== */

printBliksemFormula(Stream,F):-
   \+ \+ ( numbervars(F,0,_),
           printBliksem(F,5,Stream) ),
   format(Stream,'.~n',[]).


/* ========================================================================
   Print a list of Bliksem formulas
======================================================================== */

printBliksemFormulas([],_):- !.

printBliksemFormulas([F|L],Stream):-
   printBliksemFormula(Stream,F),
   printBliksemFormulas(L,Stream).


/* ========================================================================
   Print Bliksem formulas
======================================================================== */

printBliksem(some(X,Formula),Tab,Stream):- !,
   write(Stream,'(< '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' >'),
   printBliksem(Formula,Tab,Stream),
   write(Stream,')').

printBliksem(all(X,Formula),Tab,Stream):- !,
   write(Stream,'([ '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' ]'),
   printBliksem(Formula,Tab,Stream),
   write(Stream,')').

printBliksem(and(Phi,Psi),Tab,Stream):- !,
   write(Stream,'('),
   printBliksem(Phi,Tab,Stream), 
   format(Stream,' & ~n',[]),
   tab(Stream,Tab),
   NewTab is Tab + 5,
   printBliksem(Psi,NewTab,Stream),
   write(Stream,')').

printBliksem(or(Phi,Psi),Tab,Stream):- !,
   write(Stream,'('),
   printBliksem(Phi,Tab,Stream),
   write(Stream,' | '),
   printBliksem(Psi,Tab,Stream),
   write(Stream,')').

printBliksem(imp(Phi,Psi),Tab,Stream):- !,
   write(Stream,'('),  
   printBliksem(Phi,Tab,Stream),
   write(Stream,' -> '),
   printBliksem(Psi,Tab,Stream),
   write(Stream,')').

printBliksem(bimp(Phi,Psi),Tab,Stream):- !,
   write(Stream,'('),  
   printBliksem(Phi,Tab,Stream),
   write(Stream,' <-> '),
   printBliksem(Psi,Tab,Stream),
   write(Stream,')').

printBliksem(not(Phi),Tab,Stream):- !,
   write(Stream,'!'),
   printBliksem(Phi,Tab,Stream).

printBliksem(eq(X,Y),_,Stream):- !,
   write(Stream,'( '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' = '),
   write_term(Stream,Y,[numbervars(true)]),
   write(Stream,' )').

printBliksem(Pred,_,Stream):-
   nonvar(Pred),
   Pred =.. [Sym|Args],
   write(Stream,Sym),
   write(Stream,'('),
   printArgs(Args,Stream),
   write(Stream,')').
