
:- module(drs2tex,[drs2tex/2]).

:- use_module(semlib(drs2fol),[symbol/4]).


/*========================================================================
   Latex DRSs
========================================================================*/

drs2tex(smerge(B1,B2),Stream):- !, 
   drs2tex(merge(B1,B2),Stream).

drs2tex(alfa(_,B1,B2),Stream):- !, 
   write(Stream,'('),
   drs2tex(B1,Stream),
   write(Stream,'$\\alpha$'),
   drs2tex(B2,Stream),
   write(Stream,')').

drs2tex(merge(B1,B2),Stream):- !, 
   write(Stream,'('),
   drs2tex(B1,Stream),
   write(Stream,';'),
   drs2tex(B2,Stream),
   write(Stream,')').

drs2tex(drs(D,C),Stream):- !,
   write(Stream,'\\drs{'),
   refs2tex(D,Stream),
   write(Stream,'{'),
   conds2tex(C,Stream),
   write(Stream,'}').


/*========================================================================
   Tex DRS-referents
========================================================================*/

refs2tex([],Stream):- !,
   write(Stream,'}').

refs2tex([_:C],Stream):- !,
   write(Stream,C),
   write(Stream,'}').

refs2tex([_:C|L],Stream):-
   write(Stream,C),
   write(Stream,' '),
   refs2tex(L,Stream).


/*========================================================================
   Tex DRS-Conditions
========================================================================*/

conds2tex([],_):- !.

conds2tex([_:C],Stream):- !,
   cond2tex(C,Stream,_), 
   write(Stream,'\\\\[-3pt]').

conds2tex([_:C|L],Stream):-
   cond2tex(C,Stream,N), 
   format(Stream,'\\\\[~ppt]~n',[N]),
   conds2tex(L,Stream).


/*========================================================================
   Tex DRS-Condition
========================================================================*/

cond2tex(not(Drs),Stream,9):- !,
   write(Stream,'$\\lnot$'),
   drs2tex(Drs,Stream).

cond2tex(nec(Drs),Stream,9):- !,
   write(Stream,'$\\Diamond$'),
   drs2tex(Drs,Stream).

cond2tex(pos(Drs),Stream,9):- !,
   write(Stream,'$\\Box$'),
   drs2tex(Drs,Stream).
 
cond2tex(prop(X,Drs),Stream,9):- !,
   write(Stream,X),
   write(Stream,':'),
   drs2tex(Drs,Stream).

cond2tex(or(Drs1,Drs2),Stream,9):- !,
   drs2tex(Drs1,Stream),
   write(Stream,'$\\lor$'),
   drs2tex(Drs2,Stream).

cond2tex(imp(Drs1,Drs2),Stream,9):- !,
   drs2tex(Drs1,Stream),
   write(Stream,'$\\Rightarrow$'),
   drs2tex(Drs2,Stream).

cond2tex(whq(Drs1,Drs2),Stream,9):- !,
   drs2tex(Drs1,Stream),
   write(Stream,'?'),
   drs2tex(Drs2,Stream).

cond2tex(duplex(_,Drs1,_,Drs2),Stream,N):- !, 
   cond2tex(whq(Drs1,Drs2),Stream,N).

cond2tex(card(X,C,_),Stream,1):- !,
   write(Stream,'$|$'),
   write(Stream,X),
   write(Stream,'$|$ = '),
   write(Stream,C).

cond2tex(named(X,C,T,_),Stream,1):- !,
   write(Stream,'named('),
   write(Stream,X),
   write(Stream,','),
   write(Stream,C),
   write(Stream,','),
   write(Stream,T),
   write(Stream,')').

cond2tex(timex(X,D1),Stream,1):- !,
   sym2tex(D1,t,_Sense,D2),
   write(Stream,D2),
   write(Stream,'('),
   write(Stream,X),
   write(Stream,')').

cond2tex(eq(X,Y),Stream,1):-  !,
   write(Stream,X),
   write(Stream,' = '),
   write(Stream,Y).

cond2tex(pred(X,Sym1,Type,Sense),Stream,1):- 
   sym2tex(Sym1,Type,Sense,Sym2), !,
   write(Stream,Sym2),
   write(Stream,'('),
   write(Stream,X),
   write(Stream,')').

cond2tex(rel(X,Y,Sym1,Sense),Stream,1):- 
   sym2tex(Sym1,r,Sense,Sym2), !,
   write(Stream,Sym2),
   write(Stream,'('),
   write(Stream,X),
   write(Stream,','),
   write(Stream,Y),
   write(Stream,')').

cond2tex(role(X,Y,Sym1,1),Stream,1):- 
   sym2tex(Sym1,r,1,Sym2), !,
   write(Stream,Sym2),
   write(Stream,'('),
   write(Stream,X),
   write(Stream,','),
   write(Stream,Y),
   write(Stream,')').

cond2tex(role(X,Y,Sym1,-1),Stream,1):- 
   sym2tex(Sym1,r,1,Sym2), !,
   write(Stream,Sym2),
   write(Stream,'('),
   write(Stream,Y),
   write(Stream,','),
   write(Stream,X),
   write(Stream,')').


/*========================================================================
   Tex non-logical sym2texs
========================================================================*/

sym2tex(Sym1,Type,Sense,Sym3):-
   symbol(Type,Sym1,Sense,Sym2),
   name(Sym2,Codes2),
   underscore(Codes2,Codes3),
   name(Sym3,Codes3).


/*========================================================================
   Deal with underscores
========================================================================*/

underscore([],[]).

underscore([95|L1],[92,95|L2]):- !,
   underscore(L1,L2).

underscore([X|L1],[X|L2]):- !,
   underscore(L1,L2).
