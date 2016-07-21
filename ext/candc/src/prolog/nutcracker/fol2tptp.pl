
:- module(fol2tptp,[fol2tptp/2,
                    fol2tptp/3,
                    fol2tptpNew/2,
                    fol2tptpNew/3,
                    fol2tptpOld/2,
                    fol2tptpOld/3,
                    printArgs/2]).

:- use_module(semlib(errors),[warning/2]).


/* ========================================================================
   Select TPTP syntax style (default: New style)
======================================================================== */

fol2tptp(Conjecture,Stream):- fol2tptpNew(Conjecture,Stream).
fol2tptp(Axioms,Conjecture,Stream):- fol2tptpNew(Axioms,Conjecture,Stream).


/* ========================================================================
   Translates formula to TPTP syntax on Stream (new style TPTP)
======================================================================== */

fol2tptpNew(Formula,Stream):- 
   write(Stream,'fof(nutcracker,conjecture,'),
   \+ \+ ( numbervars(Formula,0,_),printTPTP(Formula,Stream) ),
   write(Stream,').'),
   nl(Stream).


/* ========================================================================
   Translates axioms+formula to TPTP syntax on Stream (new style TPTP)
======================================================================== */

fol2tptpNew([],Formula,Stream):- !,
   fol2tptpNew(Formula,Stream).

fol2tptpNew([Axiom|L],Formula,Stream):- 
   write(Stream,'fof(nutcracker,axiom,'),
   \+ \+ ( numbervars(Axiom,0,_),printTPTP(Axiom,Stream) ),
   write(Stream,').'), nl(Stream),
   fol2tptpNew(L,Formula,Stream).


/* ========================================================================
   Translates formula to TPTP syntax on Stream (old style TPTP)
======================================================================== */

fol2tptpOld(Formula,Stream):-
   write(Stream,'input_formula(nutcracker,conjecture,'),
   \+ \+ ( numbervars(Formula,0,_),printTPTP(Formula,Stream) ),
   write(Stream,').'),
   nl(Stream).


/* ========================================================================
   Translates axioms+formula to TPTP syntax on Stream (old style TPTP)
======================================================================== */

fol2tptpOld([],Formula,Stream):- !,
   fol2tptpOld(Formula,Stream).

fol2tptpOld([Axiom|L],Formula,Stream):- 
   write(Stream,'input_formula(nutcracker,axiom,'),
   \+ \+ ( numbervars(Axiom,0,_),printTPTP(Axiom,Stream) ),
   write(Stream,').'), nl(Stream),
   fol2tptpOld(L,Formula,Stream).


/* ========================================================================
   Print TPTP formulas
======================================================================== */

printTPTP(some(X,Formula),Stream):- !,
   write(Stream,'(? ['),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,']: '),
   printTPTP(Formula,Stream),
   write(Stream,')').

printTPTP(all(X,Formula),Stream):- !,
   write(Stream,'(! ['),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,']: '),
   printTPTP(Formula,Stream),
   write(Stream,')').

printTPTP(and(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTPTP(Phi,Stream), 
   write(Stream,' & '), 
   printTPTP(Psi,Stream), 
   write(Stream,')').

printTPTP(or(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTPTP(Phi,Stream), 
   write(Stream,' | '),
   printTPTP(Psi,Stream), 
   write(Stream,')').

printTPTP(imp(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTPTP(Phi,Stream), 
   write(Stream,' => '),
   printTPTP(Psi,Stream), 
   write(Stream,')').

printTPTP(bimp(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTPTP(Phi,Stream), 
   write(Stream,' <=> '),
   printTPTP(Psi,Stream), 
   write(Stream,')').

printTPTP(not(Phi),Stream):- !,
   write(Stream,'~ '),
   printTPTP(Phi,Stream).

printTPTP(eq(X,Y),Stream):- !,
   write_term(Stream,X=Y,[numbervars(true)]).

printTPTP(Pred,Stream):-
   nonvar(Pred),
   Pred =.. [Sym|Args],
   write(Stream,Sym),
   write(Stream,'('),
   printArgs(Args,Stream),
   write(Stream,')').


/* ========================================================================
   Print arguments
======================================================================== */

printArgs([X],Stream):- !,
   printArg(Stream,X).

printArgs([X|L],Stream):- 
   printArg(Stream,X),
   write(Stream,','),
   printArgs(L,Stream).


/* ========================================================================
   Print argument
======================================================================== */

printArg(Stream,X):- 
   functor(X,'$VAR',1), !,
   write_term(Stream,X,[numbervars(true)]).

printArg(Stream,X):- 
   warning('term expexted, found formula: ~p',X),
   write_term(Stream,X,[numbervars(true)]).
