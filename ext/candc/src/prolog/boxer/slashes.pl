
% This module defines operators for slashes 
% in CCG categories. This is an SWI Prolog specific
% to modularise operator definitions. Modules importing
% this module have access to these operators.
  
:- module(slashes,[op(601,xfx,(/)),
                   op(601,xfx,(\))]).

user:portray(A/B):- write('('),print(A),write('/'),print(B),write(')').
user:portray(A\B):- write('('),print(A),write('\\'),print(B),write(')').
