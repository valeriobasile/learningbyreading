
:- module(printCCG,[writeCCG/4,
                    printCCG/2,
                    printTokCat/2]).

:- use_module(slashes).
:- use_module(library(lists),[reverse/2,member/2]).


/* ----------------------------------------------------------------------
   Main Predicate (simple)
---------------------------------------------------------------------- */

writeCCG(CCG,Name,No,Stream):-  
   format(Stream,'ccg(~p,   %%% ~q~n',[No,Name]),
   writeDer(CCG,5,Stream),
   write(Stream,').'), nl(Stream), 
   nl(Stream).


/* ----------------------------------------------------------------------
   Derivation
---------------------------------------------------------------------- */

writeDer(t(Cat,Pos,_Ind,Tok),Tab,Stream):- !,
   tab(Stream,Tab),
   downcase_atom(Tok,Stem),
   write(Stream,'t('), 
   print(Stream,Cat), 
   format(Stream,',~q,~q,~q)',[Tok,Stem,Pos]).

writeDer(Der,Tab,Stream):-
   Der =.. [Rule,Cat1,Cat2,A], 
   member(Rule,[tc]), !,
   tab(Stream,Tab), 
   write(Stream,Rule), write(Stream,'('),
   print(Stream,Cat1), write(Stream,', '),
   print(Stream,Cat2), write(Stream,','), nl(Stream),
   NewTab is Tab + 5,
   writeDer(A,NewTab,Stream),
   write(Stream,')').

writeDer(Der,Tab,Stream):-
   Der =.. [Rule,Cat,A1,A2], !,
   tab(Stream,Tab), 
   write(Stream,Rule), write(Stream,'('), 
   print(Stream,Cat), write(Stream,','), nl(Stream),
   NewTab is Tab + 5,
   writeDer(A1,NewTab,Stream),
   write(Stream,','), nl(Stream),
   writeDer(A2,NewTab,Stream),
   write(Stream,')').

writeDer(Der,Tab,Stream):-
   Der =.. [Rule,Cat,A],
   atom(Rule), !,
   tab(Stream,Tab), 
   write(Stream,Rule), write(Stream,'('), 
   print(Stream,Cat), write(Stream,','), nl(Stream),
   NewTab is Tab + 5,
   writeDer(A,NewTab,Stream),
   write(Stream,')').

writeDer(Der,Tab,Stream):-
   tab(Stream,Tab),
   write(Stream,Der),
   nl(Stream).


/* ----------------------------------------------------------------------
   Main Predicate (pretty)
---------------------------------------------------------------------- */

printCCG(CCG,Stream):-
   ccg2lines(CCG,_,Lines), !,
   reverse(Lines,Reversed), 
   nl(Stream),
   printLines(Reversed,Stream).

printCCG(CCG,Stream):-
   writeCCG(CCG,Stream).

printTokCat(CCG,Stream):-
   printCat(CCG,Stream),
   nl(Stream).

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

ccg2lines(t(Cat,_,_,Tok),Len,[Line1,Line2,Line3]):- !,
   ccg2lines(tok(Cat,Tok),Len,[Line1,Line2,Line3]).

ccg2lines(t(_,Cat,Tok,_,_,_),Len,[Line1,Line2,Line3]):- !,
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
   ( Rule = tc(N,_,Tree), RuleName = tc
   ; Rule = tr(N,Tree), RuleName = tr ), !,
   cat2atom(N,Atom),
   atom_length(Atom,CatLen),
   ccg2lines(Tree,TreeLen,L2),
   fillRight(L2,TreeLen,CatLen,Max,L3),
   cat(Atom,Max,Line1),
   rule(RuleName,Max,Line2).

ccg2lines(Rule,Max,[Line1,Line2|L5]):-
   ( Rule = ba(N,Left,Right),     !, RuleName = ba
   ; Rule = fa(N,Left,Right),     !, RuleName = fa
   ; Rule = fc(N,Left,Right),     !, RuleName = fc 
   ; Rule = bc(N,Left,Right),     !, RuleName = bc
   ; Rule = fs(N,Left,Right),     !, RuleName = fs
   ; Rule = bs(N,Left,Right),     !, RuleName = bs
   ; Rule = fxc(N,Left,Right),    !, RuleName = fxc
   ; Rule = bxc(N,Left,Right),    !, RuleName = bxc
   ; Rule = fxs(N,Left,Right),    !, RuleName = fxs
   ; Rule = bxs(N,Left,Right),    !, RuleName = bxs
   ; Rule = gfc(N,_,Left,Right),  !, RuleName = gfc
   ; Rule = gfc(N,Left,Right),    !, RuleName = gfc
   ; Rule = gbc(N,_,Left,Right),  !, RuleName = gbc
   ; Rule = gbc(N,Left,Right),    !, RuleName = gbc
   ; Rule = gfxc(N,_,Left,Right), !, RuleName = gfxc
   ; Rule = gfxc(N,Left,Right),   !, RuleName = gfxc
   ; Rule = gbxc(N,_,Left,Right), !, RuleName = gbxc
   ; Rule = gbxc(N,Left,Right),   !, RuleName = gbxc
   ; Rule = lp(N,Left,Right),     !, RuleName = lp
   ; Rule = rp(N,Left,Right),     !, RuleName = rp
   ; Rule = ltc(N,Left,Right),    !, RuleName = ltc
   ; Rule = rtc(N,Left,Right),    !, RuleName = rtc
   ; Rule = conj(N,_,Left,Right), !, RuleName = conj ), !,
   cat2atom(N,Atom),
   atom_length(Atom,CatLen),
   ccg2lines(Left,MaxLeft,L2),
   ccg2lines(Right,MaxRight,L3),
   combLines(MaxLeft,L2,MaxRight,L3,TreeLen,L4),
   fillRight(L4,TreeLen,CatLen,Max,L5),
   cat(Atom,Max,Line1),
   rule(RuleName,Max,Line2).

ccg2lines(Rule,Max,[Line1,Line2|L7]):-
   Rule = coord(N,Left,Middle,Right), !,
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

%combLines([A,B,C],Space1,[D,E,F,G|L2],Space2,L3):- !,
%   combLines([Space1,A,B,C],Space1,[D,E,F,G|L2],Space2,L3).

%combLines([D,E,F,G|L1],Space1,[A,B,C],Space2,L3):- !,
%   combLines([D,E,F,G|L1],Space1,[Space2,A,B,C],Space2,L3).

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

cat2atom(Cat,Atom):- 
   cat2atom(Cat,Atom,top).

cat2atom(Var,Cat,_):-
   var(Var), !, Cat = 'UNK'.

cat2atom(X:F,Cat,_):- 
   atom(X), atom(F), !,
   atom_concat(':',F,Temp),
   atom_concat(X,Temp,Cat), !.

cat2atom(X:F,Cat,_):- 
   atom(X), var(F), !, F= 'UNK',
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
   Print Cat
---------------------------------------------------------------------- */

printCat(t(Cat,Pos,_Ind,Tok),Stream):- !,
%   cat2atom(Cat,CatAtom),
%   format(Stream,'LEX ~p ~p ~p ~p~n',[Tok,Pos,Ind,CatAtom]).
   printCat(tok(Cat,Pos,Tok),Stream).

printCat(tok(Cat,Pos,Tok),Stream):- !,
   cat2atom(Cat,CatAtom),
   format(Stream,'LEX ~p ~p ~p~n',[Tok,Pos,CatAtom]).

printCat(Rule,Stream):-
   ( Rule = tc(_,_,Tree)
   ; Rule = tr(_,Tree) ), !,
   printCat(Tree,Stream).

printCat(Rule,Stream):-
   ( Rule = ba(N,Left,Right),     !, RuleName = ba
   ; Rule = fa(N,Left,Right),     !, RuleName = fa
   ; Rule = fc(N,Left,Right),     !, RuleName = fc 
   ; Rule = bc(N,Left,Right),     !, RuleName = bc
   ; Rule = fs(N,Left,Right),     !, RuleName = fs
   ; Rule = bs(N,Left,Right),     !, RuleName = bs
   ; Rule = fxc(N,Left,Right),    !, RuleName = fxc
   ; Rule = bxc(N,Left,Right),    !, RuleName = bxc
   ; Rule = fxs(N,Left,Right),    !, RuleName = fxs
   ; Rule = bxs(N,Left,Right),    !, RuleName = bxs
   ; Rule = gfc(N,_,Left,Right),  !, RuleName = gfc
   ; Rule = gfc(N,Left,Right),    !, RuleName = gfc
   ; Rule = gbc(N,_,Left,Right),  !, RuleName = gbc
   ; Rule = gbc(N,Left,Right),    !, RuleName = gbc
   ; Rule = gfxc(N,_,Left,Right), !, RuleName = gfxc
   ; Rule = gfxc(N,Left,Right),   !, RuleName = gfxc
   ; Rule = gbxc(N,_,Left,Right), !, RuleName = gbxc
   ; Rule = gbxc(N,Left,Right),   !, RuleName = gbxc
   ; Rule = lp(N,Left,Right),     !, RuleName = lp
   ; Rule = rp(N,Left,Right),     !, RuleName = rp
   ; Rule = ltc(N,Left,Right),    !, RuleName = ltc
   ; Rule = rtc(N,Left,Right),    !, RuleName = rtc
   ; Rule = conj(N,_,Left,Right), !, RuleName = conj ), !,
   printCat(Left,Stream),
   printCat(Right,Stream).

printCat(Rule,Stream):-
   Rule = coord(_,Left,Middle,Right), !,
   printCat(Left,Stream),
   printCat(Middle,Stream),
   printCat(Right,Stream).

printCat(Rule,Stream):-
   write(Stream,Rule),nl(Stream).
