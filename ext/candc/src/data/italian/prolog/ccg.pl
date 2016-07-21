:- module(ccg,[ccg/2]).

:- use_module(slashes).
:- use_module(printError,[error/2,error/3]).
:- use_module(library(lists),[member/2,append/3]).

:- dynamic run_mode/1.

/* ----------------------------------------------------------
   Wrapper
---------------------------------------------------------- */

ccg(HAM:Tree,CCG):- 
   RunMode = 0,
   ccg(HAM:Tree,CCG,RunMode).

ccg(_,_,N):-
   max_run_mode(N), 
   !, fail.

ccg(HAM:Tree,CCG,RunMode):- 
   setRunMode(RunMode),
   Stack = [],      %%% stack empty at start
   ccg(Tree,HAM,Stack,CCG), !.

ccg(Tree,CCG,N):-
   RunMode is N + 1, 
   ccg(Tree,CCG,RunMode).

/* ----------------------------------------------------------
   Running Modes
---------------------------------------------------------- */

run_mode(0).
max_run_mode(3).

setRunMode(M):-
   retract(run_mode(_)), 
   assert(run_mode(M)).


/* ----------------------------------------------------------
   Debugging
---------------------------------------------------------- */

%debugging(Type,Node,Result):- write(Type), write(' '), write(Node), write(' '), write(Result), nl.
debugging(_,_,_).


/* ----------------------------------------------------------
   Top Cat
---------------------------------------------------------- */

topcat(t(C1,_,_,_),C2):- !, C2=C1.
topcat(ba(C1,_,_),C2):- !, C2=C1.
topcat(fa(C1,_,_),C2):- !, C2=C1.
topcat(CCG,_):- !, error('topcat not defined for ~p~n',[CCG]), fail.


/* ----------------------------------------------------------
   Modifier
---------------------------------------------------------- */

modifier(Tree,Stack,CCG):- ccg(Tree,m,Stack,CCG), !.


/* ----------------------------------------------------------
   Argument
---------------------------------------------------------- */

argument(tree('S-INDCOMPL',a:trace('NP-SUBJ',_),h:T),CCG):- 
   ccg(T,h,[argleft(np)],CCG).

argument(Tree,CCG):- 
   ccg(Tree,a,[],CCG).


/* ----------------------------------------------------------
   Apposition (cancelled for now)

ccg(tree(NP,_:Tree),HAM,np\np,tc(np\np,np,tc(np,n,New))):- apposition(NP), ccg(Tree,HAM,n,New), !.
ccg(tree(NP,_:Tree),HAM,np\np,tc(np\np,np,New)):-          apposition(NP), ccg(Tree,HAM,np,New), !.
ccg(tree(NP,_:Tree),HAM,n\n,tc(n\n,np,New)):-              apposition(NP), ccg(Tree,HAM,np,New), !.
ccg(tree(NP,_:Tree),HAM,n\n,tc(n\n,np,tc(np,n,New))):-     apposition(NP), ccg(Tree,HAM,n,New), !.
---------------------------------------------------------- */


/* ----------------------------------------------------------
   Unary Rule
---------------------------------------------------------- */

ccg(tree(_, _:Tree),HAM,Stack,CCG):- !, ccg(Tree,HAM,Stack,CCG).

/* ----------------------------------------------------------
   Unary Rule: (introduce type-changing rule) -- cancelled for now

ccg(tree(NP,_:Tree),a,np,tc(np,n,New)):- constituent(NP,'NP'), ccg(Tree,a,n,New).
ccg(tree(DP,_:Tree),a,np,tc(np,n,New)):- constituent(DP,'DP'), ccg(Tree,a,n,New).
---------------------------------------------------------- */


/* ----------------------------------------------------------
   Combinatorial Rules: ignore traces
---------------------------------------------------------- */

ccg(tree(_, Trace, _:T),HAM,Stack,CCG):- atrace(Trace), !, ccg(T,HAM,Stack,CCG).
ccg(tree(_, _:T, Trace),HAM,Stack,CCG):- atrace(Trace), !, ccg(T,HAM,Stack,CCG).

%ccg(tree(_Node, _:tree(_,_:trace(_,_)), HAM:T),_,Stack,CCG):- 
%   \+ member(Node,['S-RMOD+RELCL+REDUC',
%                   'S-RMODPRED+SUBJ',
%                   'S-RMODPRED+OBJ']), 
%   !,
%   ccg(T,HAM,Stack,CCG).
%
%ccg(tree(_, HAM:T, _:tree(_,_:trace(_,_))),_,Stack,CCG):- !, ccg(T,HAM,Stack,CCG).


/* ===================================================================
   Combinatorial Rules: HEAD ARG
=================================================================== */

/* -------------------------------------------------------------------
   HEAD ARG: Coordination

ccg(tree(Node,h:T1,a:T2),_,Cat\Cat,fa(Cat\Cat,C1,C2)):-
   Node = 'COORD', !,
   ccg(T2,a,Cat,C2), 
   debugging('ha (coordination)',Node,C2),
   ccg(T1,h,(Cat\Cat)/Cat,C1), !.
------------------------------------------------------------------- */

/* -------------------------------------------------------------------
   HEAD ARG: VP with fxc

ccg(tree(VP,h:T1,a:T2),_,(s:F1\np)\np,fxc((s:F1\np)\np,C1,C2)):- 
   constituent(VP,'VP'), 
   ccg(T2,a,(s:F2\np)\np,C2), 
   debugging('ha (VP with forward crossing)',VP,C2),
   ccg(T1,h,(s:F1\np)/(s:F2\np),C1), !.
------------------------------------------------------------------- */


/* -------------------------------------------------------------------
   HEAD ARG: conjunction
------------------------------------------------------------------- */

ccg(tree(Node,h:T1,a:T2),h,Stack,fa(Arg\Arg,C1,C2)):- 
   run_mode(0),
   T1 = leaf(COORD,_,_Tok), member(COORD,['CONJ','PUNCT-COORD']), !,
   ccg(T2,a,[],C2), 
   topcat(C2,Arg),
   debugging('ha conj h0 (arg)',Node,C2),
   ccg(T1,h,[argrigh(Arg)|Stack],C1), 
   topcat(C1,(Arg\Arg)/Arg),
   debugging('ha conj h0 (head)',Node,C1).

ccg(tree(Node,h:T1,a:T2),h,Stack,fa(Fun,C1,C2)):- 
   run_mode(1),
   T1 = leaf(COORD,_,_Tok), member(COORD,['CONJ','PUNCT-COORD']), !,
   ccg(T2,a,[],C2), 
   topcat(C2,Arg),
   debugging('ha conj h1 (arg)',Node,C2),
   ccg(T1,h,[argrigh(Arg)|Stack],C1), 
   topcat(C1,Fun/Arg), 
   ( Fun = (Arg\Arg) 
   ; Fun = (Mod\Mod)
   ; Fun = (Mod/Mod)
   ; Fun = (Mod1\Mod2), nonvar(Mod1), nonvar(Mod2) ),
   debugging('ha conj h1 (head)',Node,C1).

ccg(tree(Node,h:T1,a:T2),m,Stack,fa(Fun,C1,C2)):- 
   run_mode(0),
   T1 = leaf(COORD,_,_Tok), member(COORD,['CONJ','PUNCT-COORD']), 
   !,
   ccg(T2,a,[],C2), 
   topcat(C2,Arg),
   debugging('ha conj m0 (arg)',Node,C2),
   ccg(T1,h,[argrigh(Arg)|Stack],C1), 
   topcat(C1,Fun/Arg), 
   ( Fun = (Arg\Arg), nonvar(Arg)
   ; Fun = (Mod\Mod), nonvar(Mod)
   ; Fun = (Mod/Mod), nonvar(Mod)
   ; Fun = ((Mod1\Mod1)\Mod), nonvar(Mod), nonvar(Mod1) ),
   debugging('ha conj m0 (head)',Node,C1).

/* -------------------------------------------------------------------
   HEAD ARG: logical subject (passive construction)
------------------------------------------------------------------- */

ccg(tree('Vbar',h:T1,a:T2),_,Stack,CCG):- 
   T2 =.. [tree,LGS|_], 
   constituent(LGS,'NP-LGS'), !,
   ccg(T1,h,[argrigh('NP-LGS')|Stack],CCG).

/* -------------------------------------------------------------------
   HEAD ARG: general case
------------------------------------------------------------------- */

ccg(tree(Node,h:T1,a:T2),_,Stack,fa(Fun,C1,C2)):- !,
%   ccg(T2,a,[],C2), 
   argument(T2,C2), 
   topcat(C2,Arg),
   debugging('ha (arg)',Node,C2),
   ccg(T1,h,[argrigh(Arg)|Stack],C1), 
   topcat(C1,Fun/Arg), 
   debugging('ha (head)',Node,C1).


/* ===================================================================
   Combinatorial Rules: ARG HEAD
=================================================================== */

/* -------------------------------------------------------------------
   ARG HEAD: relative clause
------------------------------------------------------------------- */

ccg(tree(Node,a:T1,h:T2),_,Stack,fa(Fun,C1,C2)):- 
   Node ='S-RMOD+RELCL', 
   T1 = tree(NP, h:tree('Nbar', h:leaf('PRO~RE', _, 'che'))),
   constituent(NP,'NP-SUBJ'),   T2 =.. [tree,'Sbar'|_], !,
   ccg(T2,h,[argleft(np)],C2), 
   topcat(C2,Arg),
   debugging('ha relcl (arg)',Node,C2),
   ccg(T1,h,[argrigh(Arg)|Stack],C1), 
   topcat(C1,Fun/Arg), 
   debugging('ha relcl (head)',Node,C1).


/* -------------------------------------------------------------------
   ARG HEAD: reduced relative clauses (argument contains trace)

ccg(tree('S-RMOD+RELCL+REDUC',a:_,h:T2),_,n\n,tc(n\n,s:X\np,C2)):- !,
   ccg(T2,h,s:X\np,C2).

ccg(tree('S-RMODPRED+SUBJ',a:_,h:T2),_,X\X,tc(X\X,s:F\np,C2)):- !,
   ccg(T2,h,s:F\np,C2).

ccg(tree('S-RMODPRED+OBJ',a:_,h:T2),_,X\X,tc(X\X,s:F\np,C2)):- !,
   ccg(T2,h,s:F\np,C2).
------------------------------------------------------------------- */


/* -------------------------------------------------------------------
   ARG HEAD: Vbar
   This is meant for question words, but it is not restrictive enough!

ccg(tree(Vbar,a:T1,h:T2),_,Cat1,fa(Cat1,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   ccg(T2,h,Cat2\np,C2), 
   debugging('ah (question word)',Vbar,C2),
   ccg(T1,a,Cat1/(Cat2\np),C1), !.
------------------------------------------------------------------- */

/* -------------------------------------------------------------------
   ARG HEAD: should be modifier  -- probably bug (ex: loro fallimento)
   Asked Cristina by email on Aug 18 2009

ccg(tree(Nbar,a:T1,h:T2),_,Cat1,fa(Cat1,C1,C2)):- 
   Nbar = 'Nbar',
   constituent(Nbar,'Nbar'), !, 
   ccg(T2,a,Cat2,C2),
   debugging('ah (funny possessive)',Nbar,C2),
   ccg(T1,h,Cat1/Cat2,C1).
------------------------------------------------------------------- */



/* -------------------------------------------------------------------
   ARG HEAD: coordination

ccg(tree(Node,a:T1,h:T2),_,Arg,ba(Arg,C1,C2)):- 
   ccg(T2,h,Arg\Arg,C2), 
   debugging('ah (coordination)',Node,C2),
   ccg(T1,a,Arg,C1), !.
------------------------------------------------------------------- */

/* -------------------------------------------------------------------
   ARG HEAD: sentence apposition

ccg(tree('S-APPOSITION',a:T1,h:T2),_,s:X\s:X,tc(s:X\s:X,Cat,ba(Cat,C1,C2))):- 
   ccg(T2,h,Cat\Arg,C2),
   debugging('ah (S-Apposition)','S-APPOSITION',C2),
   ccg(T1,a,Arg,C1), !.
------------------------------------------------------------------- */

/* -------------------------------------------------------------------
   ARG HEAD: conjunction (does not always succeed)
------------------------------------------------------------------- */

ccg(tree(Node,a:T1,h:T2),_,Stack,ba(Arg/Arg,C1,C2)):- 
   T2 = leaf(COORD,_,_), member(COORD,['CONJ','PUNCT-COORD']), 
   ccg(T1,a,[],C1), topcat(C1,Arg),
   debugging('ah conj (arg)',Node,C1),
   ccg(T2,h,[argleft(Arg)|Stack],C2), 
   topcat(C2,(Arg/Arg)\Arg), !,
   debugging('ah conj (head)',Node,C2).

/* -------------------------------------------------------------------
   ARG HEAD: general case
------------------------------------------------------------------- */

ccg(tree(Node,a:T1,h:T2),_,Stack,ba(Fun,C1,C2)):- !,
   ccg(T1,a,[],C1), topcat(C1,Arg),
   debugging('ah (arg)',Node,C1),
   ccg(T2,h,[argleft(Arg)|Stack],C2), 
   topcat(C2,Fun\Arg), 
   debugging('ah (head)',Node,C2).


/* ===================================================================
   Combinatorial Rules: HEAD MOD
=================================================================== */

/* -------------------------------------------------------------------
   HEAD MOD: redo punctutation (end of sentence)
------------------------------------------------------------------- */

ccg(tree(Node,h:T1,m:T2),HAM,Stack,CCG):- 
   T2 = leaf('PUNCT-END',_,_), !,
   ccg(tree(Node,a:T1,h:T2),HAM,Stack,CCG).


/* -------------------------------------------------------------------
   HEAD MOD: Vbar (generation)

ccg(tree(Vbar,h:T1,m:T2),_,Cat\Arg,bc(Cat\Arg,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   member(Cat,[s:_,s:_\np,s:_/np]), 
   ccg(T1,h,Cat\Arg,C1),
   ccg(T2,m,Cat\Cat,C2), !.

ccg(tree(Vbar,h:T1,m:T2),_,Cat/Arg,bxc(Cat/Arg,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   member(Cat,[s:_,s:_\np,s:_/np]), 
   ccg(T1,h,Cat/Arg,C1),
   ccg(T2,m,Cat\Cat,C2), !.

ccg(tree(Vbar,h:T1,m:T2),_,(Cat/Arg1)/Arg2,gbxc((Cat/Arg1)/Arg2,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   member(Cat,[s:_,s:_\np,s:_/np]), 
   ccg(T1,h,(Cat/Arg1)/Arg2,C1),
   ccg(T2,m,Cat\Cat,C2), !.
------------------------------------------------------------------- */

/* -------------------------------------------------------------------
   HEAD MOD: general case
------------------------------------------------------------------- */

ccg(tree(Node,h:T1,m:T2),_,Stack,ba(Fun,C1,C2)):- !,
   ccg(T1,h,Stack,C1), topcat(C1,Cat),
   debugging('hm (head)',Node,C1),
   modifier(T2,[modleft(Cat)],C2), 
   debugging('hm (mod)',Node,C2), 
   topcat(C2,Fun\Cat).

/* ===================================================================
   Combinatorial Rules: MOD HEAD
=================================================================== */

/* --------------------------------------------------------------------
   MOD HEAD: AUX + AUX + VP (MOD acts as functor) (eg ALB-23)
-------------------------------------------------------------------- */

ccg(tree(Node,m:T1,h:T4),_,Stack,fa(Fun2,C2,fa(Fun3,C3,C4))):- 
   Node = 'Vbar',
   T1 = tree('VP',h:tree(Node,m:T2,h:T3)),
   T2 = tree('VP',h:tree(Node,h:leaf(Aux1,_,_))), aux_verb(Aux1,_), 
   T3 = leaf(Aux2,_,_), aux_verb(Aux2,_), 
   ccg(T4,h,Stack,C4), topcat(C4,CatArg),
   ccg(T3,h,Stack,t(Fun3,B,C,D)), 
   C3 = t(Fun3/CatArg,B,C,D),
   ccg(T2,h,Stack,t(Fun2,E,F,G)), !,
   C2 = t(Fun2/Fun3,E,F,G).

/* --------------------------------------------------------------------
   MOD HEAD: AUX + VP (needed because MOD acts as functor)
-------------------------------------------------------------------- */

ccg(tree(Node,m:T1,h:T2),_,Stack,fa(CatFun,C1,C2)):- 
   Node = 'Vbar',
   T1 = tree('VP',h:tree(Node,h:leaf(Pos,_,_))), 
   aux_verb(Pos,_), 
   ccg(T2,h,Stack,C2), topcat(C2,CatArg),
   ccg(T1,h,Stack,t(CatFun,B,C,D)), !,
   C1 = t(CatFun/CatArg,B,C,D).

/* ----------------------------------------------------------
   MOD HEAD: AUX + TV (needed because MOD acts as functor)

ccg(tree(Vbar,m:T1,h:T2),_,Cat1\Arg,fxc(Cat1\Arg,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   T1 = tree('VP',_), 
   ccg(T1,m,Cat1/Cat2,C1),
   debugging('mh (Aux + TV, case 1)',Vbar,C1), 
   ccg(T2,h,Cat2\Arg,C2), !.

ccg(tree(Vbar,m:T1,h:T2),_,Cat1/Arg,fc(Cat1/Arg,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   T1 = tree('VP',_), 
   ccg(T1,m,Cat1/Cat2,C1),
   debugging('mh (Aux + TV, case 2)',Vbar,C1), 
   ccg(T2,h,Cat2/Arg,C2), !.
---------------------------------------------------------- */


/*
ccg(tree(Vbar,m:T1,h:T2),_,(Cat1\Arg1)\Arg2,gfxc((Cat1\Arg1)\Arg2,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   member(Cat1,[s:_,s:_\np,s:_/np]), 
   member(Cat2,[s:_,s:_\np,s:_/np]), 
   ccg(T1,m,Cat1/Cat2,C1),
   ccg(T2,h,(Cat2\Arg1)\Arg2,C2), !.

ccg(tree(Vbar,m:T1,h:T2),_,(Cat1\Arg1)/Arg2,gfxc((Cat1\Arg1)/Arg2,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   member(Cat1,[s:_,s:_\np,s:_/np]),  
   member(Cat2,[s:_,s:_\np,s:_/np]), 
   ccg(T1,m,Cat1/Cat2,C1),
   ccg(T2,h,(Cat2\Arg1)/Arg2,C2), !.

ccg(tree(Vbar,m:T1,h:T2),_,(Cat1/Arg1)/Arg2,gfc((Cat1/Arg1)/Arg2,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   member(Cat1,[s:_,s:_\np,s:_/np]),
   member(Cat2,[s:_,s:_\np,s:_/np]), 
   ccg(T1,m,Cat1/Cat2,C1),
   ccg(T2,h,(Cat2/Arg1)/Arg2,C2), !.

ccg(tree(Vbar,m:T1,h:T2),_,(Cat1/Arg1)\Arg2,gfc((Cat1/Arg1)\Arg2,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   member(Cat1,[s:_,s:_\np,s:_/np]), 
   member(Cat2,[s:_,s:_\np,s:_/np]), 
   ccg(T1,m,Cat1/Cat2,C1),
   ccg(T2,h,(Cat2/Arg1)\Arg2,C2), !.

ccg(tree(Vbar,m:T1,h:T2),_,((Cat1\Arg1)\Arg2)/Arg3,gfxc(((Cat1\Arg1)\Arg2)/Arg3,C1,C2)):- 
   constituent(Vbar,'Vbar'), 
   member(Cat1,[s:_,s:_\np,s:_/np]), 
   member(Cat2,[s:_,s:_\np,s:_/np]), 
   ccg(T1,m,Cat1/Cat2,C1),
   ccg(T2,h,((Cat2\Arg1)\Arg2)/Arg3,C2), !.
*/

/* -------------------------------------------------------------------
   MOD HEAD: general case
------------------------------------------------------------------- */

ccg(tree(Node,m:T1,h:T2),_,Stack,fa(Fun,C1,C2)):- !,
   ccg(T2,h,Stack,C2), topcat(C2,Cat),
   debugging('mh (head)',Node,C2),
   modifier(T1,[modrigh(Cat)],C1),
   debugging('mh (mod)',Node,C1),
   topcat(C1,Fun/Cat).


/* ----------------------------------------------------------------------------------
   Leaf Nodes: MOD (quotes and parentheticals)
---------------------------------------------------------------------------------- */

ccg(leaf(Pos,Ind,Tok), m, [modrigh(X)], t(Cat,Pos,Ind,Tok)):- 
   Pos = 'PUNCT-OPEN+QUOTES', !, 
   Cat = (X/p:quo)/X.

ccg(leaf(Pos,Ind,Tok), m, [modrigh(X)], t(Cat,Pos,Ind,Tok)):-  
   Pos = 'PUNCT-OPEN+PARENTHETICAL', !, 
   Cat = (X/p:par)/X.

ccg(leaf(Pos,Ind,Tok), m, [modleft(X/p:F)], t(Cat,Pos,Ind,Tok)):- 
   Pos = 'PUNCT-CLOSE+QUOTES', !, 
   Cat = X\(X/p:F).

ccg(leaf(Pos,Ind,Tok), m, [modleft(X/p:F)], t(Cat,Pos,Ind,Tok)):- 
   Pos = 'PUNCT-CLOSE+PARENTHETICAL', !, 
   Cat = X\(X/p:F).

/* ----------------------------------------------------------------------------------
   Leaf Nodes: MOD (general case)
---------------------------------------------------------------------------------- */

ccg(leaf(Pos,Ind,Tok), m, [modrigh(X)], t(Cat,Pos,Ind,Tok)):- !, Cat = (X/X).
ccg(leaf(Pos,Ind,Tok), m, [modleft(X)], t(Cat,Pos,Ind,Tok)):- !, Cat = (X\X).


/* ----------------------------------------------------------
   Leaf Nodes: HEAD (general case)
---------------------------------------------------------- */

ccg(leaf(Pos,Ind,Tok), h, Stack, t(Cat,Pos,Ind,Tok) ):- 
   pos2cat(Pos,Base,Stack), 
   build(Stack,Base,Cat).


/* ----------------------------------------------------------
   Leaf Nodes: HEAD (error message)
---------------------------------------------------------- */

ccg(leaf(Pos,Ind,Tok), h, Stack, t(Cat,Pos,Ind,Tok) ):- 
   \+ pos2cat(Pos,_,Stack), 
   Cat = missing, 
   write('PROBLEM missing in pos2cat':Pos:Stack:Cat), nl.


/* ----------------------------------------------------------
   Leaf Nodes: ARG
---------------------------------------------------------- */

ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- noun(Pos), !, member(Cat,[n,np]).
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- pro(Pos), !, Cat = np.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- ordpro(Pos), !, Cat = n.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- adj(Pos), Cat = (s:adj\np).
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- adj(Pos), Cat = (n/n).
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- adj(Pos), !, Cat = (n\n).
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- intpro(Pos), !, Cat = np.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- relpro(Pos), !, Cat = np.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- locpro(Pos), !, Cat = np.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- pospro(Pos), !, Cat = np.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- art(Pos), !, Cat = (np/n).
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- main_verb(Pos,F), !, Cat = (s:F\np).

ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'CONJ', !, Cat = _.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'ADVB', !, Cat = pp.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'ADVB-COORDANTEC+COMPAR', !, Cat = pp.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'PREP', !, Cat = (pp/np).

ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'ADJ~PO', !, member(Cat,[n/n,np/n]).
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'PROSUBJ', !, Cat = np.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'NUMR', !, member(Cat,[n,np]).
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'DATE', !, Cat = n.
ccg(leaf(Pos,Ind,Tok), a, [], t(Cat,Pos,Ind,Tok)):- Pos = 'SPECIAL-ARG-PERCENT', !, Cat = n.

ccg(Leaf, a, Cat, _):- Leaf = leaf(_,_,_), write('PROBLEM':a:Leaf:Cat),nl, fail.


/* ------------------------------------------------------------------------
   Mapping from POS to basic CCG categories [possible arguments]
------------------------------------------------------------------------ */

pos2cat(Pos, s:pss,    S):- main_verb(Pos,_),    member(argrigh('NP-LGS'),S).
pos2cat(Pos, s:F,      S):- main_verb(Pos,F), \+ member(argrigh('NP-LGS'),S).
pos2cat(Pos, s:F,      _):- modal_verb(Pos,F).
pos2cat(Pos, s:F,      _):- aux_verb(Pos,F).
pos2cat(Pos, s:adj\np, _):- adj(Pos).

pos2cat(Pos, np,       _):- art(Pos).
pos2cat(Pos, np,       _):- pro(Pos).
pos2cat(Pos, np,       _):- locpro(Pos).
pos2cat(Pos, np,       _):- intpro(Pos).
pos2cat(Pos, n,        _):- ordpro(Pos).
pos2cat(Pos, np,       _):- relpro(Pos).
pos2cat(Pos, np,       _):- pospro(Pos).
pos2cat(Pos, n,        _):- pospro(Pos).
pos2cat(Pos, n,        _):- noun(Pos).
pos2cat(Pos, np,       _):- noun(Pos).

pos2cat('PREP',    pp,   _).
pos2cat('PREP',    X\X,  _).
pos2cat('PREP',    X/X,  _).
pos2cat('DEFPREP', pp,   _).
pos2cat('DEFPREP', X\X,  _).
pos2cat('DEFPREP', X/X,  _).

pos2cat('DATE',            n,   _).
pos2cat('NUMR',            n,   _).
pos2cat('NUMR-INTRAMD',    np,  _).
pos2cat('NUMR',            np,  _).
pos2cat('NUMR-CONTIN+NUM', n,   _).
pos2cat('ADJ~PO',          np,  _).    % possessive
pos2cat('ADJ~PO',          n,   _).    % possessive

pos2cat('ADVB',                   n,  _).     % rare; not always noun
pos2cat('ADVB-INTRAMD',           pp, _).     % only occurs once (bug in TUT?)
pos2cat('ADVB-COORDANTEC+COMPAR', pp, _).

pos2cat('PHRAS',           s:int, _).
pos2cat('PUNCT-END',       t,     _).
pos2cat('PRDT',            np,    _).

pos2cat('PUNCT-COORD',     _,  _).
pos2cat('PUNCT-SEPARATOR', _,  _).
pos2cat('CONJ',            _,  _).


/* ------------------------------------------------------------------------
   Determine base of functor category
------------------------------------------------------------------------ */

build([],Cat,Cat):- !.
build([modleft(X)],_,(X\X)):- !.
build([modrigh(X)],_,(X/X)):- !.
build([argleft(X)|L],In,(Out\X)):- !, build(L,In,Out).
build([argrigh('NP-LGS')|L],In,(Out)):- !, build(L,In,Out).
build([argrigh(X)|L],In,(Out/X)):- !, build(L,In,Out).


/* ----------------------------------
   Articles
---------------------------------- */

art('ART~DE').   % definite
art('ART~IN').   % indefinite

art('ADJ~DE').       % definite
art('ADJ~DI').       % demonstrative
art('ADJ~DI2').      % other
art('ADJ~DI2SUBJ').  % other
art('ADJ~IN').       % indef
art('ADJ~IR').       % quali
%art('ADJ~PO').       % possessive


/* ----------------------------------
   Apposition
---------------------------------- */

apposition('NP-APPOSITION').
apposition('NP-APPOSITION-DENOM').
apposition('DP-APPOSITION').


/* ----------------------------------
   Interrogatives
---------------------------------- */

whq('Dove').
whq('Come').
whq('Perche\'').

/* ----------------------------------
   Verbs
---------------------------------- */

verb(X):- main_verb(X,_).
verb(X):- modal_verb(X,_).
verb(X):- aux_verb(X,_).
verb(X):- causitive_verb(X,_).

main_verb('VMA~RA', dcl).    % historical past
main_verb('VMA~RE', dcl).    % present
main_verb('VMA~FU', dcl).    % future
main_verb('VMA~CO', dcl).    % conditional
main_verb('VMA~CG', dcl).    % conjunctive
main_verb('VMA~GE', ger).    % gerund
main_verb('VMA~PA', pap).    % past participle
main_verb('VMA~PE', prp).    % present participle
main_verb('VMA~IM', dcl).    % imperfect
main_verb('VMA~IN', inf).    % infinitive
main_verb('VMA~IP', imp).    % imperative

causitive_verb('VCA~RE', dcl).    % causitive

modal_verb('VMO~RA', dcl).    % historical past
modal_verb('VMO~RE', dcl).    % present
modal_verb('VMO~FU', dcl).    % future
modal_verb('VMO~CO', dcl).    % conditional
modal_verb('VMO~CG', dcl).    % conjunctive
modal_verb('VMO~GE', ger).    % gerund
modal_verb('VMO~PA', pap).    % past participle
modal_verb('VMO~PE', prp).    % present participle
modal_verb('VMO~IM', dcl).    % imperfect
modal_verb('VMO~IN', inf).    % infinitive
modal_verb('VMO~IP', imp).    % imperative

aux_verb('VAU~RA', dcl).    % historical past
aux_verb('VAU~RE', dcl).    % present
aux_verb('VAU~FU', dcl).    % future
aux_verb('VAU~CO', dcl).    % conditional
aux_verb('VAU~CG', dcl).    % conjunctive
aux_verb('VAU~GE', ger).    % gerund
aux_verb('VAU~PA', pap).    % past participle
aux_verb('VAU~PE', prp).    % present participle
aux_verb('VAU~IM', dcl).    % imperfect
aux_verb('VAU~IN', inf).    % infinitive
aux_verb('VAU~IP', imp).    % imperative


/* ----------------------------------
   Nouns
---------------------------------- */

noun(Noun):- constituent(Noun,'NOU~').


/* ----------------------------------
   Adjectives
---------------------------------- */

adj('ADJ~QU').   % qualifying
%adj('ADJ~PO').   % possessive
adj('ADJ~DI').   % demonstrative
adj('ADJ~DI2').  % demonstrative
adj('ADJ~DI2-COORD2ND+BASE').
adj('ADJ~DI2-COORD2ND').
adj('ADJ~OR').   % ordinal
adj('ADJ~EX').   % che
adj('ADJ~QU-ARG').   % qualifying -- bug?


/* ----------------------------------
   Pronouns
---------------------------------- */

pro('PRO-ARG').
pro('PRO~RI').   % reflexive impersonal
pro('PRO~PE').   % personal
pro('PRO~ID').
pro('PRO~DE').

ordpro('PRO~OR').  % ordinal
locpro('PRO~LO').  % locative pronoun
pospro('PRO~PO').  % possessive
relpro('PRO~RE').  % relative pronoun
intpro('PRO~IN').  % interrogative


/* ===================================================================
   Constituent checking
=================================================================== */

constituent(trace(Cons,_),Goal):- !,
   constituent(Cons,Goal).

constituent(ref(Cons,_),Goal):- !,
   constituent(Cons,Goal).

constituent(Cons,Goal):- 
   atom(Cons), atom(Goal),
   atom_chars(Goal,GoalChars),
   atom_chars(Cons,ConsChars),
   append(GoalChars,_,ConsChars), !.


/* ===================================================================
   Detecting Traces
=================================================================== */

atrace(_:trace(_,_)):- !.
atrace(_:tree(_,Tree)):- atrace(Tree).


