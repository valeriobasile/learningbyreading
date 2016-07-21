
:- module(printError,[error/2, error/3]).

error(_Format,_Arg):- 
   user:switch(err,no), !.

error(Format,Arg):- 
   error(Format,Arg,user_output).

error(_Format,_Arg,_Stream):-
   user:switch(err,no), !.

error(Format,Arg,Stream):-
   write(Stream,'ERROR: '),
   format(Stream,Format,Arg).
