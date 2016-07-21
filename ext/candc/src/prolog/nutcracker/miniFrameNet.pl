
:- module(miniFrameNet,[axiomsFN/2]).

:- use_module(library(lists),[member/2,append/3]).
:- use_module(semlib(options),[option/2]).


/* ------------------------------------------------------------------------
   Main Predicate
------------------------------------------------------------------------ */

axiomsFN(DRS,Axioms):- 
   option('--roles',framenet),
   framenetfile(File),
   exists_file(File), !,
   findFrames(DRS,[]-Frames), 
   selectAxioms(Frames,Axioms).

axiomsFN(_,[]).


/* ------------------------------------------------------------------------
   File with Framenet Axioms
------------------------------------------------------------------------ */

framenetfile('working/framenet/framenet.pl').


/* ------------------------------------------------------------------------
   Find Frames in DRS
------------------------------------------------------------------------ */

findFrames(drs(_,C),F1-F2):- !, findFrames(C,F1-F2).
findFrames(smerge(A,B),F1-F3):- !, findFrames(A,F1-F2), findFrames(B,F2-F3).
findFrames(merge(A,B),F1-F3):- !, findFrames(A,F1-F2), findFrames(B,F2-F3).
findFrames(alfa(_,A,B),F1-F3):- !, findFrames(A,F1-F2), findFrames(B,F2-F3).

findFrames([],F-F):- !.
findFrames([_:pos(A)|C],F1-F3):- !, findFrames(A,F1-F2), findFrames(C,F2-F3).
findFrames([_:nec(A)|C],F1-F3):- !, findFrames(A,F1-F2), findFrames(C,F2-F3).
findFrames([_:not(A)|C],F1-F3):- !, findFrames(A,F1-F2), findFrames(C,F2-F3).
findFrames([_:prop(_,A)|C],F1-F3):- !, findFrames(A,F1-F2), findFrames(C,F2-F3).
findFrames([_:imp(A,B)|C],F1-F4):- !, findFrames(A,F1-F2), findFrames(B,F2-F3), findFrames(C,F3-F4).
findFrames([_:or(A,B)|C],F1-F4):- !, findFrames(A,F1-F2), findFrames(B,F2-F3), findFrames(C,F3-F4).
findFrames([_:whq(A,B)|C],F1-F4):- !, findFrames(A,F1-F2), findFrames(B,F2-F3), findFrames(C,F3-F4).
findFrames([_:whq(_,A,_,B)|C],F1-F4):- !, findFrames(A,F1-F2), findFrames(B,F2-F3), findFrames(C,F3-F4).
findFrames([_:pred(_,F,f,1)|C],F1-F2):- \+ member(F,F1), !, findFrames(C,[F|F1]-F2).
findFrames([_|C],F1-F2):- findFrames(C,F1-F2).


/* ------------------------------------------------------------------------
   Select Axioms
------------------------------------------------------------------------ */

selectAxioms([],[]).

selectAxioms([F|L],Axioms):-
   findall(A,axiom(F,_,A),Axioms1),
   atom_concat(f1,F,Frame), functor(Sym,Frame,1), arg(1,Sym,X),
   Extra = all(X,imp(Sym,n1event(X))),
   append([Extra|Axioms1],Axioms2,Axioms),
   selectAxioms(L,Axioms2).


/* ------------------------------------------------------------------------
   Load FrameNet
------------------------------------------------------------------------ */

loadFN:-
   framenetfile(File),
   exists_file(File),   
   [File].

loadFN.

:- loadFN.

