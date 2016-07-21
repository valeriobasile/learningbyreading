
:- module(binarise,[binarise/3]).

:- use_module(library(lists),[member/2,select/3,append/3]).
:- use_module(printError,[error/2,error/3]).

/* -----------------------------------------------------------------------
   Binarising: put branches in a list
----------------------------------------------------------------------- */

binarise(Tree,Name,Bin):- 
   Tree =.. [tree,Node|Branches], !,
   binarise(t(Node,Branches),Name,Bin).


/* -----------------------------------------------------------------------
   Binarising: definite prepositions 
------------------------------------------------------------------------ */

binarise(t(Cat,[h:T1,a:T2]),B,C):-
   T1 = tree('PREP',Tok1),
   T2 = tree(DPARG, h:tree('Dbar', h:tree('ART~DE', Tok2), a:Tree)),
   constituent(DPARG,'DP-ARG'),
   atom_codes(Tok1,Codes1), atom_codes(Tok2,Codes2),
   append(_,[47|Codes],Codes1), append(_,[47|Codes],Codes2), !,
   binarise(t(Cat,[h:tree('DEFPREP',Tok1),a:Tree]),B,C).


/* -----------------------------------------------------------------------
   Binarising: repair of embedded and relative clauses
------------------------------------------------------------------------ */

binarise(t(Cat,[h:T1,a:T2]),B,C):-
   Cat = 'CONJP-OBJ',
   T1 = tree('CONJ',_),
   T2 =.. [tree,'S-ARG'|_], !,
   binarise(t(Cat,[m:T1,h:T2]),B,C).

%binarise(t(Cat,[a:T1,h:T2]),B,C):-
%   Cat = 'S-RMOD+RELCL',
%   T1 = tree('NP-SUBJ',_),
%   T2 =.. [tree,'Sbar'|_], !,
%   binarise(t(Cat,[h:T1,a:T2]),B,C).


/* -----------------------------------------------------------------------
   Binarising: repair of double-headed preposition 
------------------------------------------------------------------------ */

binarise(t(Cat,[h:T1,h:T2]),B,C):- 
   T1 =.. [tree,'PREP'|_], 
   T2 =.. [tree,'PP-CONTIN+PREP'|_], !,
   binarise(t(Cat,[h:T1,a:T2]),B,C).

/* -----------------------------------------------------------------------
   Binarising: repair of name/noun compounds
------------------------------------------------------------------------ */

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,h:T2],L1,Before,After),
   T1 =.. [tree,'Nbar'|_],
   T2 =.. [tree,'NP-CONTIN+DENOM'|_], !,
   sublist([h:t(Cat,[m:T1,h:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,h:T2],L1,Before,After),
   T1 =.. [tree,'Nbar'|_],
   T2 =.. [tree,'NOU~CA-CONTIN'|_], !,
   sublist([h:t(Cat,[m:T1,h:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

/* -----------------------------------------------------------------------
   Binarising: repair of numerical compounds
------------------------------------------------------------------------ */

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,h:T2],L1,Before,After),
   T1 =.. [tree,'NUMR'|_],
   T2 =.. [tree,'DP-CONTIN+NUM'|_], !,
   sublist([h:t(Cat,[a:T1,h:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,h:T2],L1,Before,After),
   T1 =.. [tree,'NUMR'|_],
   T2 =.. [tree,NUMRC|_], 
   constituent(NUMRC,'NUMR-CONTIN+NUM'), !,
   sublist([h:t(Cat,[a:T1,h:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,h:T2],L1,Before,After),
   T1 =.. [tree,'NUMR'|_],
   T2 =.. [tree,'Dbar-CONTIN+NUM'|_], !,
   sublist([h:t(Cat,[a:T1,h:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

/* -----------------------------------------------------------------------
   Binarising: repair of comparative
------------------------------------------------------------------------ */

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,h:T2],L1,Before,After),
   T1 =.. [tree,'ADVB-COORDANTEC+COMPAR'|_],
   T2 =.. [tree,'ADJ~QU'|_], !,
   sublist([h:t(Cat,[h:T1,a:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

/* -----------------------------------------------------------------------
   Binarising: repair of percentage expressions
------------------------------------------------------------------------ */

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,a:T2,h:T3],L1,Before,After),
   T1 =.. [tree,'NUMR'|_],
   T2 =.. [tree,'SPECIAL-ARG-PERCENT'|_], 
   T3 =.. [tree,'PP-ARG-PARTITIVE'|_], !,
   sublist([h:t(Cat,[h:T1,a:T2,a:T3])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,a:T2,h:T3],L1,Before,After),
   T1 =.. [tree,'NUMR'|_],
   T2 =.. [tree,'PP-ARG-PERCENT'|_], 
   T3 =.. [tree,'PP-ARG-PARTITIVE'|_], !,
   sublist([h:t(Cat,[h:T1,a:T2,a:T3])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,a:T2,a:T3,h:T4],L1,Before,After),
   T1 =.. [tree,'NUMR'|_],
   T2 =.. [tree,'SPECIAL-ARG-PERCENT'|_], 
   T3 =.. [tree,'PP-ARG'|_], 
   T4 =.. [tree,'PP-ARG-PARTITIVE'|_], !,
   sublist([h:t(Cat,[h:T1,a:T2,a:T3,a:T4])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

/* -----------------------------------------------------------------------
   Binarising: repair of sentences with VP as subject
------------------------------------------------------------------------ */

binarise(t(Cat,L1),B,C):- 
   sublist([h:T1,h:T2],L1,Before,After),
   T1 =.. [tree,VPSUBJ|_], constituent(VPSUBJ,'VP-SUBJ'),
   T2 =.. [tree,'Sbar'|_], !,
   sublist([h:t(Cat,[a:T1,h:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

/* -----------------------------------------------------------------------
   Binarising: possessives (ALB-19)
------------------------------------------------------------------------ */

binarise(t(Cat,L1),B,C):- 
   sublist([a:T1,a:T2],L1,Before,After),
   T1 =.. [tree,'ADJ~PO'|_],
   T2 =.. [tree,'NOU~CS-ARG'|_], !,
   sublist([h:t(Cat,[h:T1,a:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

/* -----------------------------------------------------------------------
   Binarising: DP modifiers (ALB-33, ALB-381)
------------------------------------------------------------------------ */

binarise(t(Cat,L1),B,C):- 
   sublist([m:T1,a:T2],L1,Before,After),
   T1 =.. [tree,'ADVP-RMOD'|_],
   T2 =.. [tree,'Dbar'|_], !,
   sublist([h:t(Cat,[m:T1,h:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

binarise(t(Cat,L1),B,C):- 
   sublist([m:T1,a:T2],L1,Before,After),
   T1 =.. [tree,'PRDT-RMOD'|_],
   T2 =.. [tree,'Dbar'|_], !,
   sublist([h:t(Cat,[m:T1,h:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

binarise(t(Cat,L1),B,C):- 
   sublist([a:T1,a:T2],L1,Before,After),
   T1 =.. [tree,ADJ|_], constituent(ADJ,'ADJ'),
   T2 =.. [tree,NOU|_], constituent(NOU,'NOU'), !,
   sublist([h:t(Cat,[h:T1,a:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).

/* -----------------------------------------------------------------------
   Binarising: embedded clauses (CHIAM-47, V-19)
------------------------------------------------------------------------ */

binarise(t(Cat,L1),B,C):- 
   Cat = 'Vbar',
   sublist([a:T1,m:T2],L1,Before,After),
   T1 =.. [tree,VMA|_], constituent(VMA,'VMA~'),
   T2 =.. [tree,CON|_], constituent(CON,'CONJP-OBJ'), !,
   sublist([h:t(Cat,[h:T1,a:T2])],L2,Before,After),
   binarise(t(Cat,L2),B,C).


/* ------------------------------------------------------
   Binarising: headless monsters
------------------------------------------------------ */

binarise(t(Cat,Branches),Name,_):-
   Branches = [_,_|_], 
   \+ member(h:_,Branches),
   error('headless monster in ~p (~p)~n',[Name,Cat]), 
   printBranches(Branches),
   !, fail.


/* ------------------------------------------------------
   Binarising: two headed monsters
------------------------------------------------------ */

binarise(t(Cat,Branches),Name,_):- 
   select(h:_,Branches,Rest),
   member(h:_,Rest), 
   error('two-headed monster in ~p (~p)~n',[Name,Cat]), 
   printBranches(Branches),
   !, fail.


/* ------------------------------------------------------
   Binarising: traces
------------------------------------------------------ */

binarise(t(Cat,[]),_,trace(S,Trace)):- 
   atom_chars(Cat,Chars),
   append(Pre,['*','['|List],Chars), !,
   atom_chars(Trace,['['|List]),
   atom_chars(S,Pre).

binarise(t(Node,_,[h:tree('Nbar',h:tree(_,Word))]),_,trace(Node,'[]')):- 
   atom(Word),
   atom_chars(Word,Chars),
   append(_,['*','[',']'],Chars), !.


/* ------------------------------------------------------
   Binarising: leaf nodes
------------------------------------------------------ */

binarise(t(Cat,[Word]),_,leaf(NewCat,Number,Token)):-
   atom(Word),
   atom_codes(Word,Codes),
   append(NumCodes,[47|TokenCodes],Codes), !, 
   number_codes(Number,NumCodes),
   atom_codes(Token,TokenCodes),
   ( atom_codes(Cat,CatCodes), append(NewCatCodes,[42|_],CatCodes), !, atom_codes(NewCat,NewCatCodes) ; NewCat = Cat ).


/* ------------------------------------------------------
   Binarising: unary branching
------------------------------------------------------ */

binarise(t(N,[T:A]),Name,tree(N,T:B)):- !,
   binarise(A,Name,B).


/* ------------------------------------------------------
   Binarising: binary branching
------------------------------------------------------ */

binarise(t(N,[T1:A1,T2:A2]),Name,tree(N,T1:B1,T2:B2)):- !,
   binarise(A1,Name,B1),
   binarise(A2,Name,B2).


/* ------------------------------------------------------
   Binarising: aux verbs
------------------------------------------------------ */

binarise(t(Node,B1),Name,Bin):- 
   Node = 'Vbar',
   sublist([m:T1,h:T2],B1,Before,After),
   T1 =.. [tree,'VP'|_], T2 =.. [tree,Verb|_], 
   member(Verb,['VMA~GE','VMA~PA','VMO~PA']), !,
   sublist([h:t(Node,[m:T1,h:T2])],B2,Before,After),
   binarise(t(Node,B2),Name,Bin).


/* ------------------------------------------------------
   Binarising: punctuation (itemisation)
------------------------------------------------------ */

binarise(t(Node,Branches),Name,Bin):-
   Branches = [Mod1,Mod2|Rest],
   Mod1 = m:T1, T1 =.. [tree,'DP-RMOD-LISTPOS'|_],
   Mod2 = m:T2, T2 =.. [tree,'PUNCT-SEPARATOR'|_], !,
   Tree = t(Node,[m:tree('LISTPOS',a:T1,h:T2)|Rest]),
   binarise(Tree,Name,Bin).


/* ------------------------------------------------------
   Binarising: punctuation (separators)
------------------------------------------------------ */

binarise(t(Node,Branches),Name,Bin):-
   Mod = m:tree('PUNCT-SEPARATOR',_),
   member(Mod,Branches),
   append([a:Tree3,Mod],Rest,Branches), !,
   Tree = t(Node,[a:tree(Node,h:Tree3,Mod)|Rest]),
   binarise(Tree,Name,Bin).


/* ------------------------------------------------------
   Binarising: punctuation (brackets, quotes)
------------------------------------------------------ */

binarise(t(Node,Branches),Name,Bin):- 
   member(Open, ['PUNCT-OPEN+QUOTES', 'PUNCT-OPEN+PARENTHETICAL']),
   member(Close,['PUNCT-CLOSE+QUOTES','PUNCT-CLOSE+PARENTHETICAL']),
   TFi = m:tree(Open,Fi),
   TLa = m:tree(Close,La), 
   append(Before,[TFi|Temp],Branches),
   append(Mid,[TLa|After],Temp), !,
   NTFi = m:tree(Open,Fi),
   NTLa = m:tree(Close,La), 
   New = h:tree(Node,h:tree('PUNCT',NTFi,h:t(Node,Mid)),NTLa),
   sublist([New],NewBranches,Before,After),
   binarise(t(Node,NewBranches),Name,Bin).


/* ------------------------------------------------------
   Binarising: ternary (or more) branching
------------------------------------------------------ */

binarise(t(Node,B1),Name,Bin):- 
   member(Pattern,[[h:_,a:_],[a:_,h:_],[h:_,m:_],[m:_,h:_]]),
   sublist(          Pattern,  B1,Before,After), !,
   sublist([h:t(Node,Pattern)],B2,Before,After),
   binarise(t(Node,B2),Name,Bin).


/* ===========================================================
   SubList
=========================================================== */

sublist(Sub,List,Before,After):-
   append(Temp,After,List), 
   append(Before,Sub,Temp),!.


/* ===========================================================
   Print branches
=========================================================== */

printBranches([]):-
   error('.............~n',[]).

printBranches([X|L]):-
   error('    ~p~n',[X]),
   printBranches(L).


/* ===================================================================
   Constituent checking
=================================================================== */

constituent(Cons,Goal):- 
   atom(Cons), atom(Goal),
   atom_chars(Goal,GoalChars),
   atom_chars(Cons,ConsChars),
   append(GoalChars,_,ConsChars), !.
