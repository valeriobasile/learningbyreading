
:- module(printTree,[printTree/3,printBin/2]).


/* --------------------------------------------------------------------------
   Main Predicate
-------------------------------------------------------------------------- */

printBin(Tree,Stream):- 
   printTree(Tree,[],Stream),
   nl(Stream).


/* --------------------------------------------------------------------------
   Tab Length
-------------------------------------------------------------------------- */

tablength(4).


/* --------------------------------------------------------------------------
   Print Tree
-------------------------------------------------------------------------- */

printTree(single(A),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,A), nl(Stream).

printTree(leaf(A,B),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,A:B), nl(Stream).

printTree(leaf(A,B,C),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,A:B:C), nl(Stream).

printTree(Type:tree(A,B),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,Type:A), nl(Stream),
   tablength(TL),
   printTree(B,[t(' ',TL)|Tabs],Stream).

printTree(Type:tree(A,B,C),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,Type:A), nl(Stream),
   tablength(TL),
   printTree(B,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(C,[t(' ',TL)|Tabs],Stream).

printTree(Type:tree(A,B,C,D),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,Type:A), nl(Stream),
   tablength(TL),
   printTree(B,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(C,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(D,[t(' ',TL)|Tabs],Stream).

printTree(Type:tree(A,B,C,D,E),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,Type:A), write(' [4]'), nl(Stream),
   tablength(TL),
   printTree(B,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(C,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(D,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(E,[t(' ',TL)|Tabs],Stream).

printTree(Type:tree(A,B,C,D,E,F),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,Type:A), write(' [5]'), nl(Stream),
   tablength(TL),
   printTree(B,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(C,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(D,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(E,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(F,[t(' ',TL)|Tabs],Stream).

printTree(Type:tree(A,B,C,D,E,F,G),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,Type:A), write(' [6]'), nl(Stream),
   tablength(TL),
   printTree(B,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(C,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(D,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(E,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(F,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(G,[t(' ',TL)|Tabs],Stream).

printTree(Type:tree(A,B,C,D,E,F,G,H),Tabs,Stream):- !,
   line(Tabs,Stream), write(Stream,Type:A), write(' [7]'), nl(Stream),
   tablength(TL),
   printTree(B,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(C,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(D,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(E,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(F,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(G,[t('|',TL)|Tabs],Stream),
   tabs([t('|',TL),t('|',TL)|Tabs],Stream), nl(Stream),
   printTree(H,[t(' ',TL)|Tabs],Stream).

printTree(Unknown,Tabs,Stream):-
   line(Tabs,Stream), 
   write(Stream,Unknown), nl(Stream).


/* --------------------------------------------------------------------------
   Print Line
-------------------------------------------------------------------------- */

line([],_):- !.

line(Tabs,Stream):- 
   ( Tabs = [t(' ',_)|_], 
    Del = '`', !
   ; Del = '|' ),
   tabs(Tabs,Stream), 
   write(Stream,Del), 
   tablength(TL), 
   sub_atom('------------------',0,TL,_,Line), 
   write(Stream,Line).


/* --------------------------------------------------------------------------
   Print Tabs
-------------------------------------------------------------------------- */

tabs([],_):- !.
tabs(L,Stream):- reverse(L,R), tabs2(R,Stream).

tabs2([],_):- !.
tabs2([t(_,_)],_):- !. 
tabs2([t(Del,X)|L],Stream):- write(Stream,Del), tab(Stream,X), tabs2(L,Stream).


