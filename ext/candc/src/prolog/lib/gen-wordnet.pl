
:- ['ext/PrologWordNet/wn_s.pl'].    % synsets, s/6
:- ['ext/PrologWordNet/wn_hyp.pl'].  % hyponyms, hyp/2
:- ['ext/PrologWordNet/wn_ins.pl'].  % instances, ins/2
:- ['ext/PrologWordNet/wn_ant.pl'].  % antonyms, ant/4

:- use_module(library(lists),[member/2]).

/* =========================================================================
   Main
========================================================================= */

main:-
   open('working/wordnet/isa.pl',write,Stream),
   gen(Stream),
   close(Stream).


/* =========================================================================
   Generate relations
========================================================================= */

gen(Stream):-
   member(POS,[n,v,a,r]),                   % Possible Values: n,v,a,r,s
   s(ID,_,Token,POS,Sense,_),        
   synonyms(ID,POS,[]-Syn),
   hypero(ID,POS,Hyp,Type),
   antonyms(ID,POS,Ant),
   print_rel(Stream,Token:Sense,Syn,Hyp,Ant,Type),
   fail.

gen(_).


/* =========================================================================
   Hyperonyms
========================================================================= */

hypero(ID1,n,S,p):-
   ins(ID1,ID2), !,
   synonyms(ID2,n,[]-S).

hypero(ID1,Pos,S,Pos):-
   hyp(ID1,ID2), !,
   synonyms(ID2,Pos,[]-S).

hypero(_,Pos,[],Pos).


/* =========================================================================
   Antonyms
========================================================================= */

antonyms(ID1,a,S):-
   ant(ID1,1,ID2,1), !,
   synonyms(ID2,a,[]-S).

antonyms(_,_,[]).


/* =========================================================================
   Synonyms
========================================================================= */

synonyms(ID,POS,L1-L2):-
   synonyms(ID,1,POS,L1-L2).

synonyms(ID,N,POS,L1-[Word1:Sense|L2]):-
   s(ID,N,Word,POS,Sense,_), !,
   symbol(Word,Word1),
   M is N + 1,
   synonyms(ID,M,POS,L1-L2).

synonyms(_,_,_,L-L).


/* =========================================================================
   Output
========================================================================= */

print_rel(Stream,Concept,Syn,Hyp,Ant,Type):-
   print_syn(Syn,Concept,Type,Stream),
   print_hyp(Hyp,Concept,Type,Stream),
   print_ant(Ant,Concept,Type,Stream).


/* =========================================================================
   Output SYN
========================================================================= */

print_syn([],_,_,_).

print_syn([Sym:Sense|L],Tok:Sense,Type,Stream):- 
   symbol(Tok,Sym), !,
   print_syn(L,Tok:Sense,Type,Stream).

print_syn([X:XS|L],Tok:Sense,Type,Stream):-
   symbol(Tok,Sym),
   format(Stream,'syn~p(~q,~q,~q,~q).~n',[Type,Sym,Sense,X,XS]),
   print_syn(L,Tok:Sense,Type,Stream).


/* =========================================================================
   Output HYP
========================================================================= */

print_hyp([],_,_,_).

print_hyp([Sym:Sense|L],Tok:Sense,Type,Stream):- 
   symbol(Tok,Sym), !,
   print_hyp(L,Tok:Sense,Type,Stream).

print_hyp([X:XS|L],Tok:Sense,Type,Stream):-
   symbol(Tok,Sym),
   format(Stream,'isa~p(~q,~q,~q,~q).~n',[Type,Sym,Sense,X,XS]),
   print_hyp(L,Tok:Sense,Type,Stream).


/* =========================================================================
   Output ANTONYM
========================================================================= */

print_ant([],_,_,_).

print_ant([Sym:Sense|L],Tok:Sense,Type,Stream):- 
   symbol(Tok,Sym), !,
   print_ant(L,Tok:Sense,Type,Stream).

print_ant([X:XS|L],Tok:Sense,Type,Stream):-
   symbol(Tok,Sym),
   format(Stream,'isnota~p(~q,~q,~q,~q).~n',[Type,Sym,Sense,X,XS]),
   print_ant(L,Tok:Sense,Type,Stream).


/* =========================================================================
   Normalize Symbols
========================================================================= */

symbol(F1,F2):- 
   name(F1,A1),
   sym(A1,[A|A2]),
   name(F2,[A|A2]), !.

symbol(F,F).


/* =========================================================================
   Normalize Symbol Characters
========================================================================= */

sym([],[]).

%%% replace blanks by underscores
%%%
sym([32|L1],[95|L2]):- !,
   sym(L1,L2).

%%% full stop
%%%
sym([46|L1],L2):- !,
   sym(L1,L2).

%%% convert to lowercase characters
%%%
sym([X|L1],[Y|L2]):- 
   X > 64, X < 91, !,
   Y is X + 32,
  sym(L1,L2).

sym([X|L1],[X|L2]):- 
  sym(L1,L2).


/* =========================================================================
   Self Starting
========================================================================= */

:- main, halt.

