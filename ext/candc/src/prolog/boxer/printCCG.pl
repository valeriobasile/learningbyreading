
:- module(printCCG,[printCCG/2]).

:- use_module(boxer(slashes)).
:- use_module(library(lists),[reverse/2]).


/* ----------------------------------------------------------------------
   Main Predicate
---------------------------------------------------------------------- */

printCCG(CCG,Stream):-
   ccg2lines(CCG,_,Lines), !,
   reverse(Lines,Reversed),
   printLines(Reversed,Stream).

printCCG(CCG,Stream):-
   write(Stream,CCG), nl(Stream).


/* ----------------------------------------------------------------------
   Print Lines (the CCG derivation)
---------------------------------------------------------------------- */

printLines([],Stream):- 
   nl(Stream).

printLines([X|L],Stream):- 
   tab(Stream,1), write(Stream,X), nl(Stream), 
   printLines(L,Stream).


/* ----------------------------------------------------------------------
   Compute each line
---------------------------------------------------------------------- */

ccg2lines(n(X),Len,Lines):- !,
   ccg2lines(X,Len,Lines).

ccg2lines(lf(Cat,_,_,Tok),Len,[Line1,Line2,Line3]):- !,
   ccg2lines(tok(Cat,Tok),Len,[Line1,Line2,Line3]).

ccg2lines(t(Cat,Tok,_,_,_),Len,[Line1,Line2,Line3]):- !,
   ccg2lines(tok(Cat,Tok),Len,[Line1,Line2,Line3]).

ccg2lines(tok(Cat,Tok),Len,[Line1,Line2,Line3]):- !,
   cat2atom(Cat,CatAtom),
   atom_length(CatAtom,CatLen),
   atom_length(Tok,TokLen),
   Len is max(7,max(CatLen,TokLen)),
   cat(CatAtom,Len,Line1),
   rule(lex,Len,Line2),
   cat(Tok,Len,Line3).

ccg2lines(Rule,Max,[Line1,Line2|L3]):-
   ( Rule = tc(N,_,_,_,_,Tree), RuleName = '*'
   ; Rule = ftr(N,_,_,_,_,Tree), RuleName = '>T'
   ; Rule = btr(N,_,_,_,_,Tree), RuleName = '<T' ), !,
   cat2atom(N,Atom),
   atom_length(Atom,CatLen),
   ccg2lines(Tree,TreeLen,L2),
   fillRight(L2,TreeLen,CatLen,Max,L3),
   cat(Atom,Max,Line1),
   rule(RuleName,Max,Line2).

ccg2lines(Rule,Max,[Line1,Line2|L5]):-
   binRuleName(Rule,N,Left,Right,Name), !,
   cat2atom(N,Atom),
   atom_length(Atom,CatLen),
   ccg2lines(Left,MaxLeft,L2),
   ccg2lines(Right,MaxRight,L3),
   combLines(MaxLeft,L2,MaxRight,L3,TreeLen,L4),
   fillRight(L4,TreeLen,CatLen,Max,L5),
   cat(Atom,Max,Line1),
   rule(Name,Max,Line2).

ccg2lines(Rule,Max,[Line1,Line2|L7]):-
   Rule = coord(N,_,_,_,Left,Middle,Right), !,
   RuleName = coord,
   cat2atom(N,Atom),
   atom_length(Atom,CatLen),
   ccg2lines(Left,MaxLeft,L2),
   ccg2lines(Middle,MaxMiddle,L3),
   combLines(MaxLeft,L2,MaxMiddle,L3,MaxTemp,L4),
   ccg2lines(Right,MaxRight,L5),
   combLines(MaxTemp,L4,MaxRight,L5,TreeLen,L6),
   fillRight(L6,TreeLen,CatLen,Max,L7),
   cat(Atom,Max,Line1),
   rule(RuleName,Max,Line2).


/* ----------------------------------------------------------------------
   Combine Lines
---------------------------------------------------------------------- */

combLines(MaxLeft,Left,MaxRight,Right,Max,New):-
   Max is MaxLeft + MaxRight + 1,
   fill(' ',MaxLeft,SpaceLeft),
   fill(' ',MaxRight,SpaceRight),
   combLines(Left,SpaceLeft,Right,SpaceRight,New).

combLines([],_,[],_,[]):- !.

combLines([],Space1,L2,Space2,L3):- !,
   combLines([Space1],Space1,L2,Space2,L3).

combLines(L1,Space1,[],Space2,L3):- !,
   combLines(L1,Space1,[Space2],Space2,L3).

combLines([X1|L1],Space1,[X2|L2],Space2,[X3|L3]):-
   atom_concat(X1,' ',Temp),
   atom_concat(Temp,X2,X3),
   combLines(L1,Space1,L2,Space2,L3).


/* ----------------------------------------------------------------------
   Filling out spaces to the right (sometimes needed)
---------------------------------------------------------------------- */

fillRight(L,TreeLen,CatLen,TreeLen,L):- 
   TreeLen > CatLen, !.

fillRight(L,TreeLen,CatLen,TreeLen,L):- 
   TreeLen = CatLen, !.

fillRight(L1,TreeLen,CatLen,CatLen,L2):- 
   TreeLen < CatLen, !,
   Len is CatLen-TreeLen,
   fill(' ',Len,Fill),
   fillRight(L1,Fill,L2).

fillRight([],_,[]).

fillRight([X1|L1],Fill,[X2|L2]):-
   atom_concat(X1,Fill,X2),
   fillRight(L1,Fill,L2).


/* ----------------------------------------------------------------------
   Format a rule
---------------------------------------------------------------------- */

rule(Rule,Max,Line):-
   atom_length(Rule,Len),
   FillLen is ((Max - Len) - 2),
   fill('-',FillLen,Fill),
   atom_concat(Fill,'[',Line1),
   atom_concat(Line1,Rule,Line2),
   atom_concat(Line2,']',Line), !.


/* ----------------------------------------------------------------------
   Format a cat
---------------------------------------------------------------------- */

cat(Cat,Max,Line):-
   atom_length(Cat,Len),
   FillLen is Max - Len,
   fill(' ',FillLen,Fill),
   atom_concat(Cat,Fill,Line), !.


/* ----------------------------------------------------------------------
   Fill a line with a character
---------------------------------------------------------------------- */

fill(Atom,Len,Result):-
   atom_codes(Atom,[Code]),
   fill2(Code,Codes,Len),
   atom_codes(Result,Codes), !.

fill2(_,[],Len):- Len < 0, !.

fill2(_,[],0):- !.

fill2(X,[X|L],N):-
   M is N - 1,
   fill2(X,L,M).


/* ----------------------------------------------------------------------
   Convert a CCG cat to an atom
---------------------------------------------------------------------- */

cat2atom(Cat,Atom):- cat2atom(Cat,Atom,top).

cat2atom(conj:_,Atom,Level):- !, cat2atom(conj,Atom,Level).

cat2atom(X:F,Cat,_):- 
   atom(X), atom(F), !,
   atom_concat(':',F,Temp),
   atom_concat(X,Temp,Cat), !.

cat2atom(X,X,_):- atom(X), !.

cat2atom(X/Y,Atom,top):- !,
   cat2atom(X,F,notop),
   cat2atom(Y,A,notop),
   atom_concat(F,'/',Temp),
   atom_concat(Temp,A,Atom).

cat2atom(X/Y,Atom,notop):- !,
   cat2atom(X,F,notop),
   cat2atom(Y,A,notop),
   atom_concat('(',F,Temp1),
   atom_concat(Temp1,'/',Temp2),
   atom_concat(Temp2,A,Temp3),
   atom_concat(Temp3,')',Atom).

cat2atom(X\Y,Atom,top):- !,
   cat2atom(X,F,notop),
   cat2atom(Y,A,notop),
   atom_concat(F,'\\',Temp),
   atom_concat(Temp,A,Atom).

cat2atom(X\Y,Atom,notop):-
   cat2atom(X,F,notop),
   cat2atom(Y,A,notop),
   atom_concat('(',F,Temp1),
   atom_concat(Temp1,'\\',Temp2),
   atom_concat(Temp2,A,Temp3),
   atom_concat(Temp3,')',Atom).


/* ----------------------------------------------------------------------
   Binary Rule Name
---------------------------------------------------------------------- */

binRuleName(ba(N,_,_,_,Left,Right),N,Left,Right,'<').
binRuleName(fa(N,_,_,_,Left,Right),N,Left,Right,'>').
binRuleName(fc(N,_,_,_,Left,Right),N,Left,Right,'>B').
binRuleName(bc(N,_,_,_,Left,Right),N,Left,Right,'<B').
binRuleName(fs(N,_,_,_,Left,Right),N,Left,Right,'>S').
binRuleName(bs(N,_,_,_,Left,Right),N,Left,Right,'<S').
binRuleName(fxc(N,_,_,_,Left,Right),N,Left,Right,'>Bx').
binRuleName(bxc(N,_,_,_,Left,Right),N,Left,Right,'<Bx').
binRuleName(fxs(N,_,_,_,Left,Right),N,Left,Right,'>Sx').
binRuleName(bxs(N,_,_,_,Left,Right),N,Left,Right,'<Sx').
binRuleName(gfc(N,_,_,_,_,Left,Right),N,Left,Right,gfc).
binRuleName(gbc(N,_,_,_,_,Left,Right),N,Left,Right,gbc).
binRuleName(gfxc(N,_,_,_,_,Left,Right),N,Left,Right,gfxc).
binRuleName(gbxc(N,_,_,_,_,Left,Right),N,Left,Right,gbxc).
binRuleName(conj(N,_,_,_,_,Left,Right),N,Left,Right,conj).
