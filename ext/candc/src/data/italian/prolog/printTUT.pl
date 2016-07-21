
:- module(printTUT,[printTUT/2,lenTUT/2]).

/* --------------------------------------------------------------------------
   Tab Length
-------------------------------------------------------------------------- */

tablength(4).


/* --------------------------------------------------------------------------
   Print Tree
-------------------------------------------------------------------------- */

printTUT(X,Stream):- printTUT(X,0,Stream), nl(Stream).

printTUT(X,_Tab,Stream):- atom(X), !, tab(Stream,1), write(Stream,X).

printTUT(_Type:X,_Tab,Stream):- atom(X), !, tab(Stream,1), write(Stream,X).

printTUT(Type:tree(A),Tab,Stream):- !,
   nl(Stream), tab(Stream,Tab), write(Stream,Type),
   write(Stream,'('),write(Stream,A),
   write(Stream,')').

printTUT(Type:tree(A,B),Tab,Stream):- !,
   nl(Stream), tab(Stream,Tab),
   write(Stream,Type),write(Stream,'('),write(Stream,A),
   tablength(Length),
   NewTab is Tab + Length,
   printTUTs([B],NewTab,Stream),
   write(Stream,')').

printTUT(Type:Tree,Tab,Stream):- 
   Tree =.. [tree,A|Rest], !,
   nl(Stream),  
   tab(Stream,Tab),write(Stream,Type),write(Stream,'('),write(Stream,A),
   tablength(Length),
   NewTab is Tab + Length,
   printTUTs(Rest,NewTab,Stream),
   write(Stream,')').

printTUT(Type:Tree,Tab,Stream):- !,
   nl(Stream), tab(Stream,Tab), write(Stream,Type:Tree).


/* --------------------------------------------------------------------------
   Print Branches
-------------------------------------------------------------------------- */

printTUTs([],_,_):- !.
printTUTs([X|L],Tab,Stream):- printTUT(X,Tab,Stream), !, printTUTs(L,Tab,Stream).


/* --------------------------------------------------------------------------
   Length Tree
-------------------------------------------------------------------------- */

lenTUT(X,N):- lenTUT(X,0,N).

lenTUT(X,N1,N2):- atom(X), !, token(X,N1,N2).  

lenTUT(_:X,N,N):- atom(X), !.

lenTUT(_:tree(_),N,N):- !.

lenTUT(_:tree(_,B),N1,N2):- !, lenTUTs([B],N1,N2).

lenTUT(_:Tree,N1,N2):- Tree =.. [tree,_|Rest], !, lenTUTs(Rest,N1,N2).

lenTUT(_,N,N).


/* --------------------------------------------------------------------------
   Length of Branches
-------------------------------------------------------------------------- */

lenTUTs([],N,N):- !.
lenTUTs([X|L],N1,N3):- lenTUT(X,N1,N2), !, lenTUTs(L,N2,N3).

/* --------------------------------------------------------------------------
   Deciding whether it is a token
-------------------------------------------------------------------------- */

token(Atom,N1,N2):- 
   atom(Atom),
   atom_chars(Atom,Lex), 
   append(Chars,['/'|_],Lex),
   \+ member('.',Chars), !,
   N2 is N1 + 1.

token(_,N,N).
