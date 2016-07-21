
% drs2xml.pl, by Johan Bos

/*========================================================================
   File Search Paths
========================================================================*/

file_search_path(semlib,     'src/prolog/lib').
file_search_path(boxer,      'src/prolog/boxer').
file_search_path(knowledge,  'src/prolog/boxer/knowledge').
file_search_path(lex,        'src/prolog/boxer/lex').


/*========================================================================
   Load other libraries
========================================================================*/

:- use_module(boxer(xdrs2xml),[xdrs2xml/2]).


/*========================================================================
   Main
========================================================================*/

convert(In,Out):-
   openOutput(Out,Stream),
   openInput(In,Tags,DRS),
   xdrs2xml(xdrs(Tags,DRS),Stream),
   close(Stream).


/*------------------------------------------------------------------------
   Open Input File
------------------------------------------------------------------------*/

openInput(Input,Tags,DRS):-
   atomic(Input), 
   access_file(Input,read), !,
   open(Input,read,Stream),
   read(Stream,sem(_,Tags,DRS)),
   close(Stream).

openInput(Input,_,_):-
   format('cannot read file ~p~n',[Input]),
   halt.


/*------------------------------------------------------------------------
   Open Output File
------------------------------------------------------------------------*/

openOutput(Output,Stream):-
   atomic(Output), 
   \+ Output=user_output, 
   ( access_file(Output,write), !,
     open(Output,write,Stream,[encoding(utf8)])
   ; format('cannot write to specified file ~p~n',[Output]),
     Stream=user_output ), !.

openOutput(user_output).


/* =======================================================================
   Definition of start
========================================================================*/

start:-
   current_prolog_flag(argv,[swipl,_,_,In,Out]), !,
   convert(In,Out),
   halt.

start:-
   current_prolog_flag(argv,[In,Out]), !,
   convert(In,Out),
   halt.

start:- 
   write('usage: swipl -l src/prolog/boxer/drs2xml.pl INPUT OUTPUT'),nl,
   halt.

:- start.

