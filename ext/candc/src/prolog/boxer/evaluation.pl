
:- module(evaluation,[initEval/0,
                      incAttempted/0,
                      incCompleted/0,
                      reportEval/0]).


/*========================================================================
    Declare Dynamic Predicates
========================================================================*/

:- dynamic attempted/1, completed/1.


/*========================================================================
    Initialise
========================================================================*/

initEval:-
   retractall(attempted(_)), 
   retractall(completed(_)),
   assert(attempted(0)), 
   assert(completed(0)).


/*========================================================================
   Increase Completed
========================================================================*/

incCompleted:-
   retract(completed(Co1)), 
   Co2 is Co1 + 1, 
   assert(completed(Co2)), !.


/*========================================================================
   Increase Attempted
========================================================================*/

incAttempted:-
   retract(attempted(Co1)), 
   Co2 is Co1 + 1, 
   assert(attempted(Co2)), !.


/*========================================================================
   Report
========================================================================*/

reportEval:-
   attempted(At), At > 0, !,
   completed(Co),
   Percentage is (100*Co/At),
   format(user_error,
          'Attempted: ~p. Completed: ~p (~2f%).~n',
          [At,Co,Percentage]).

reportEval.

