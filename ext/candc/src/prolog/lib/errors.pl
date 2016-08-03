
:- module(errors,[warning/2,error/2,inform/2,gold/2]).

:- use_module(semlib(options),[option/2]).

warning(S,V):-
   option('--warnings',true), !,
   format(user_error,'\033[33mWARNING: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

warning(_S,_V):-
   option('--warnings',false).

gold(S,V):-
   option('--warnings',true), !,
   format(user_error,'GOLD: ',[]),
   format(user_error,S,V),
   format(user_error,'~n',[]).

gold(_S,_V):-
   option('--warnings',false).

inform(S,V):-
   option('--info',true), !,
   format(user_error,'\033[34mINFO: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

inform(_S,_V):-
   option('--info',false).

error(S,V):-
   format(user_error,'\033[31mERROR: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

