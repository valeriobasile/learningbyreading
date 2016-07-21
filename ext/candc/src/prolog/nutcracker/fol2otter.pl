
:- module(fol2otter,[fol2otter/2,
                     fol2otter/3,
                     fol2mace/2,
                     fol2mace/3]).

:- use_module(nutcracker(fol2tptp),[printArgs/2]).


/*========================================================================
   Translates formula to Otter syntax on Stream
========================================================================*/

fol2otter(Formula,Stream):-
   headerOtter(Stream),
   printOtterFormula(Formula,Stream),
   footerOtter(Stream).

fol2otter(Axioms,Formula,Stream):-
   headerOtter(Stream),
   printOtterFormulas(Axioms,Stream),
   printOtterFormula(Formula,Stream),
   footerOtter(Stream).


/*========================================================================
   Header and Footer OTTER
========================================================================*/

headerOtter(Stream):- 
   format(Stream,'set(auto).~n~n',[]),
   format(Stream,'assign(max_seconds,100).~n~n',[]),
   format(Stream,'clear(print_proofs).~n~n',[]),
   format(Stream,'set(prolog_style_variables).~n~n',[]),
   format(Stream,'formula_list(usable).~n~n',[]).

footerOtter(Stream):-
   format(Stream,'~nend_of_list.~n',[]).


/*========================================================================
   Translates formula to MACE syntax on Stream
========================================================================*/

fol2mace(Formula,Stream):- 
   headerMace(Stream),
   printOtterFormula(Formula,Stream),
   footerMace(Stream).

fol2mace(Axioms,Formula,Stream):- 
   headerMace(Stream),
   printOtterFormulas(Axioms,Stream),
   printOtterFormula(Formula,Stream),
   footerMace(Stream).


/*========================================================================
   Header and footer MACE
========================================================================*/

headerMace(Stream):-
   format(Stream,'set(auto).~n~n',[]),
   format(Stream,'clear(print_proofs).~n~n',[]),
   format(Stream,'set(prolog_style_variables).~n~n',[]),
   format(Stream,'formula_list(usable).~n~n',[]).

footerMace(Stream):-
   format(Stream,'~nend_of_list.~n',[]).


/*========================================================================
   Print an Otter formula (introducing tab)
========================================================================*/

printOtterFormula(F,Stream):-
   \+ \+ ( numbervars(F,0,_),
           printOtter(F,5,Stream) ),
   format(Stream,'.~n',[]).


/*========================================================================
   Print a set of Otter formulas
========================================================================*/

printOtterFormulas([],_).

printOtterFormulas([F|L],Stream):-
   printOtterFormula(F,Stream),
   printOtterFormulas(L,Stream).


/*========================================================================
   Print Otter formulas
========================================================================*/

printOtter(some(X,Formula),Tab,Stream):- !, 
   write(Stream,'(exists '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' '),
   printOtter(Formula,Tab,Stream),
   write(Stream,')').

printOtter(all(X,Formula),Tab,Stream):- !,
   write(Stream,'(all '),
   write_term(Stream,X,[numbervars(true)]),   
   write(Stream,' '),
   printOtter(Formula,Tab,Stream),
   write(Stream,')').

printOtter(and(Phi,Psi),Tab,Stream):- !,
   write(Stream,'('),
   printOtter(Phi,Tab,Stream), 
   format(Stream,' & ~n',[]),
   tab(Stream,Tab),
   NewTab is Tab + 5,
   printOtter(Psi,NewTab,Stream),
   write(Stream,')').

printOtter(or(Phi,Psi),Tab,Stream):- !,
   write(Stream,'('),
   printOtter(Phi,Tab,Stream),
   write(Stream,' | '),
   printOtter(Psi,Tab,Stream),
   write(Stream,')').

printOtter(imp(Phi,Psi),Tab,Stream):- !,
   write(Stream,'('),  
   printOtter(Phi,Tab,Stream),
   write(Stream,' -> '),
   printOtter(Psi,Tab,Stream),
   write(Stream,')').

printOtter(bimp(Phi,Psi),Tab,Stream):- !,
   write(Stream,'('),  
   printOtter(Phi,Tab,Stream),
   write(Stream,' <-> '),
   printOtter(Psi,Tab,Stream),
   write(Stream,')').

printOtter(not(Phi),Tab,Stream):- !,
   write(Stream,'-('),
   printOtter(Phi,Tab,Stream),
   write(Stream,')').

printOtter(eq(X,Y),_,Stream):- !,
   write(Stream,'('),  
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' = '),
   write_term(Stream,Y,[numbervars(true)]),
   write(Stream,')').

printOtter(Pred,_,Stream):-
   nonvar(Pred),
   Pred =.. [Sym|Args],
   write(Stream,Sym),
   write(Stream,'('),
   printArgs(Args,Stream),
   write(Stream,')').

