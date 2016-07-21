
% para.pl, by Johan Bos

:- use_module(library(readutil),[read_line_to_codes/2]).
:- use_module(library(lists),[member/2,append/3]).
:- ['working/ppdb.1.pl'].
:- ['working/ppdb.2.pl'].
:- ['working/ppdb.3.pl'].
:- ['working/ppdb.4.pl'].


/* ========================================================================
   Main
======================================================================== */

main(T,H,Stream):-
   para(T,H,P,S),
   write(S),nl,
   outputP(P,Stream), fail.
  
main(_,_,_).


/* ========================================================================
   Paraphrasing
======================================================================== */

para([X|L1],H,L3,[X]-Par):-
   p1(X,Par),
   ok(Par,L1,H),
   append(Par,L1,L3).

para([X,Y|L1],H,L3,[X,Y]-Par):-
   p2(X,Y,Par),
   ok(Par,L1,H),
   append(Par,L1,L3).

para([X,Y,Z|L1],H,L3,[X,Y,Z]-Par):-
   p3(X,Y,Z,Par),
   ok(Par,L1,H),
   append(Par,L1,L3).

para([X,Y,Z,A|L1],H,L3,[X,Y,Z,A]-Par):-
   p4(X,Y,Z,A,Par),
   ok(Par,L1,H),
   append(Par,L1,L3).

para([X|L1],H,[X|L2],S):-
   para(L1,H,L2,S).


/* ========================================================================
   Sanity check for paraphrase
======================================================================== */

ok(Par,T,H):-
   \+ (member(Word,Par), member(Word,T)),        % no word of the paraphrase should occur already in T
   \+ (member(Word,Par), \+ member(Word,H)), !.  % every word of the paraphrase should occur in H

ok([some],_,_).  % if the paraphrase is a determiner, take it as well.


/* ========================================================================
   Output
======================================================================== */

outputP([X],S):- !, write(S,X), nl(S).
outputP([X|L],S):- write(S,X), write(S,' '), outputP(L,S).


/*------------------------------------------------------------------------
   Read contents of file
------------------------------------------------------------------------*/

checkFile(Dir,F,S):-
   atomic_list_concat([Dir,'/',F],File),
   access_file(File,read),
   open(File,read,Stream),
   read_line_to_codes(Stream,Codes),
   close(Stream),
   atom_codes(Atom,Codes),
   downcase_atom(Atom,Down),
   atomic_list_concat(S,' ',Down).


/*------------------------------------------------------------------------
   Check presence of files t and h
------------------------------------------------------------------------*/

checkFiles([]).

checkFiles([Dir|L]):-
   checkFile(Dir,t,T),
   checkFile(Dir,h,H), !,
   atomic_list_concat([Dir,'/','paraphrases.txt'],File),   
   open(File,write,Stream),
   main(T,H,Stream),
   close(Stream),
   checkFiles(L).

checkFiles([Dir|L]):-
   format('directory ~p does not contain files~n',[Dir]), 
   checkFiles(L).


/*------------------------------------------------------------------------
   Check directory
------------------------------------------------------------------------*/

checkDir([Dir],List):-
   exists_directory(Dir), 
   access_file(Dir,write), 
   atom_concat(Dir,'/*',Wild),
   subdirs(Wild,List), !.

checkDir([_|L],Dirs):- 
   checkDir(L,Dirs).


/*------------------------------------------------------------------------
   Sub Dirs
------------------------------------------------------------------------*/

subdirs(Wild,Dirs):-
   expand_file_name(Wild,List),
   findall(D,( member(D,List),
               exists_directory(D),
               access_file(D,write) ),Dirs), !.



/* =======================================================================
   Definition of start
========================================================================*/

start:-
   current_prolog_flag(argv,Args), 
%   \+ Args = [],
   checkDir(Args,Dirs), 
   \+ Dirs=[],
   checkFiles(Dirs), !,
   halt.

start:- halt.

:- start.
