
:- module(pp,[pp/2]).

:- use_module(slashes).
:- use_module(idioms).
:- use_module(printError,[error/2,error/3]).
:- use_module(library(lists),[append/3]).


/* ===================================================================
   Post-processing: pro-drop (subject and object)
=================================================================== */

pp(ba(X,NP,VP1),tc(X,X\np,VP2)):- 
   NP = t(np,'PRO~PE',_,'*[]'), !,  
   pp(VP1,VP2).

pp(fa(X,VP1,NP),tc(X,X/np,VP2)):- 
   NP = t(np,'PRO~PE',_,'*[]'), !, 
   pp(VP1,VP2).

pp(ba(X,Arg,Fun1),tc(X,X\Cat,Fun2)):- 
   Arg = t(Cat,_,_,Lex), 
   empty(Lex), !,
   pp(Fun1,Fun2).

pp(ba(X,Arg1,Fun),tc(X,Cat,Arg2)):- 
   Fun = t(_\Cat,_,_,Lex),
   empty(Lex), !,
   pp(Arg1,Arg2).

pp(fa(X,Fun1,Arg),tc(X,X/Cat,Fun2)):- 
   Arg = t(Cat,_,_,Lex),
   empty(Lex), !,
   pp(Fun1,Fun2).

pp(fa(X,Fun,Arg1),tc(X,Cat,Arg2)):- 
   Fun = t(_/Cat,_,_,Lex), 
   empty(Lex), !,
   pp(Arg1,Arg2).

/* ===================================================================
   Post-processing: clitics (separation)
=================================================================== */

pp(fa(X,VP1,NP1),fa(X,VP2,NP2)):- 
   VP1 = t(X/np,POSVP,IVP,Token),  
   NP1 = t(np,POSNP,INP,Token), 
   atom_chars(Token,TokenChars),
   clitic(Pro),
   atom_chars(Pro,ProChars),
   append(TempChars,ProChars,TokenChars), !,
   append(TempChars,['~'],VerbChars),
   atom_chars(Verb,VerbChars),
   atom_chars(Clitic,['~'|ProChars]),
   VP2 = t(X/np,POSVP,IVP,Verb),  
   NP2 = t(np,POSNP,INP,Clitic).

pp(ba(X,VP1,NP1),ba(X,VP2,NP2)):- 
   VP1 = t(X,POSVP,IVP,Token),  
   NP1 = t(X\X,POSNP,INP,Token), 
   atom_chars(Token,TokenChars),
   clitic(Pro),
   atom_chars(Pro,ProChars),
   append(TempChars,ProChars,TokenChars), !,
   append(TempChars,['~'],VerbChars),
   atom_chars(Verb,VerbChars),
   atom_chars(Clitic,['~'|ProChars]),
   VP2 = t(X,POSVP,IVP,Verb),  
   NP2 = t(X\X,POSNP,INP,Clitic).

/* ===================================================================
   Post-processing: clitics (deletion)
=================================================================== */

%pp(fa(X,VP,NP),t(X,POS,I,Sym)):- 
%   VP = t(X/np,POS,I,Sym),  
%   NP = t(np,_,_,Sym), !.

%pp(ba(X,VP,NP),t(X,POS,I,Sym)):- 
%   VP = t(X,POS,I,Sym),  
%   NP = t(X\X,_,_,Sym), !.

/* ===================================================================
   Post-processing: composed clitics
=================================================================== */

pp(t(np,POS,I,'glieli'),t(np,POS,I,'~li')):- !.
pp(t(s:X/s:X,POS,I,'glieli'),t(s:X/s:X,POS,I,'gli~')):- !.

pp(t(np,POS,I,'glielo'),t(np,POS,I,'~lo')):- !.
pp(t(s:X/s:X,POS,I,'glielo'),t(s:X/s:X,POS,I,'gli~')):- !.

pp(t(np,POS,I,'averlo'),t(np,POS,I,'~lo')):- !.
pp(t(s:inf/s:pap,POS,I,'averlo'),t(s:inf/s:pap,POS,I,'aver~')):- !.


/* ===================================================================
   Post-processing: punctuation abstraction
=================================================================== */

pp(t((X/p:F)/X,B,C,D),t(('X'/p:F)/'X',B,C,D)):- user:switch(pab,yes), !.
pp(t(X\(X/p:F),B,C,D),t('X'\('X'/p:F),B,C,D)):- user:switch(pab,yes), !.
  
/* ===================================================================
   Post-processing: idioms
=================================================================== */

pp(t(A,B,C,D),Tree2):- 
   idiom(t(A,B,C,D),Tree1), !, Tree2=Tree1.

/* ===================================================================
   Post-processing: general
=================================================================== */

pp(t(A,B,C,D),t(A,B,C,D)):- !.
pp(bxc(Cat,L1,R1),bxc(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(fxc(Cat,L1,R1),fxc(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(ba(Cat,L1,R1),ba(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(fa(Cat,L1,R1),fa(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(bc(Cat,L1,R1),bc(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(fc(Cat,L1,R1),fc(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(gfc(Cat,L1,R1),gfc(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(gbc(Cat,L1,R1),gbc(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(conj(Cat1,Cat2,L1,R1),conj(Cat1,Cat2,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(tc(Cat1,Cat2,T1),tc(Cat1,Cat2,T2)):- !, pp(T1,T2).
pp(gbxc(Cat,L1,R1),gbxc(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(gfxc(Cat,L1,R1),gfxc(Cat,L2,R2)):- !, pp(L1,L2), pp(R1,R2).
pp(CCG,CCG):- error('ERROR (pp/2): ~p~n',[CCG]).


/* ===================================================================
   Empty Nodes
=================================================================== */

empty('*[]'):- !.
empty(Node):- atom_chars(Node,['*','['|_]).


/* ===================================================================
   Clitics
=================================================================== */

clitic(gli).
clitic(lo).
clitic(la).
clitic(le).
clitic(li).
clitic(ci).
clitic(ne).
clitic(si).
clitic(ti).
clitic(vi).
clitic(mi).


