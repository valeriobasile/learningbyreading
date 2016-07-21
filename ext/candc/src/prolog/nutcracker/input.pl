
:- module(input,[openInput/1,inputDRS/2,lenDRS/2,openModel/3]).

:- use_module(semlib(errors),[error/2,warning/2]).


/*------------------------------------------------------------------------
   Dynamic Predicates
------------------------------------------------------------------------*/

:- dynamic inputDRS/2.

:- multifile     sem/3, id/2.
:- discontiguous sem/3, id/2.
:- dynamic       sem/3, id/2.


/*------------------------------------------------------------------------
   Open Input Files
------------------------------------------------------------------------*/

openInput(Dir):-
   retractall(inputDRS(_,_)),

   retractall(id(_,_)),
   retractall(sem(_,_,_)),
   openInputDrs(Dir,t),

   retractall(id(_,_)),
   retractall(sem(_,_,_)),
   openInputDrs(Dir,h),

   retractall(id(_,_)),
   retractall(sem(_,_,_)),
   openInputDrs(Dir,th).


/*------------------------------------------------------------------------
   Open Input File and assert to database
------------------------------------------------------------------------*/

openInputDrs(Dir,Type):-
   atomic_list_concat([Dir,'/',Type,'.drs'],File),
   checkInput(File),
   loadDRS(DRS), !,
   assert(inputDRS(Type,DRS)).

openInputDrs(_,_).


/*========================================================================
   Read Model from File
========================================================================*/

openModel(Dir,Type,Model):-
   atomic_list_concat([Dir,'/',Type,'.mod'],File),
   atom(File), exists_file(File), access_file(File,read),
   open(File,read,Stream),
   read(Stream,Model), !,
   close(Stream).

openModel(_,_,unknown).


/*========================================================================
   Check Input File
========================================================================*/

checkInput(File):-
   \+ atom(File), !,
   error('file name format of ~p not allowed',[File]),
   fail.

checkInput(File):-
   \+ exists_file(File), !,
   error('file ~p does not exist',[File]),
   fail.

checkInput(File):-
   \+ access_file(File,read), !,
   error('file ~p not readable',[File]),
   fail.

checkInput(File):-
   catch(load_files([File],[autoload(true),encoding(utf8)]),_,fail), !.

checkInput(File):-
   error('file ~p not Prolog readable',[File]),
   !, fail.

/* ------------------------------------------------------------------------
   Identify IDs in the input file
------------------------------------------------------------------------ */

loadDRS(D):-
   id(_,I),
   sem(I,_A,D), !.

loadDRS(_):-
   \+ id(_,_), !,
   error('DRS input file contains no id/2 terms',[]), 
   fail.

loadDRS(_):-
   \+ sem(_,_,_), !,
   error('DRS input file contains no sem/3 terms',[]), 
   fail.


/* ------------------------------------------------------------------------
   Determine length of DRS
------------------------------------------------------------------------ */

lenDRS(alfa(_,_,B),Len):- !, lenDRS(B,Len).
lenDRS(smerge(B,_),Len):- !, lenDRS(B,Len1), Len is Len1 + 1.
lenDRS(_,1).
