
:- module(options,[parseOptions/2,
                   option/2,
                   setOption/3,
                   showOptions/1, 
                   setDefaultOptions/1]).

:- use_module(semlib(errors),[error/2,warning/2]).
:- use_module(library(lists),[member/2]).


/* =======================================================================
   Global dynamic predicates
========================================================================*/

:- dynamic option/2.
 

/* =======================================================================
   Set Option
========================================================================*/

setOption(P,Option,Value):-
   assertOptions(P,[Option:Value]).


/* =======================================================================
   Check User Options
========================================================================*/

parseOptions(P,Arg):-
   opts(P,Options,Arg,[]), !,
   assertOptions(P,Options),
   dependentOptions(P).


/* =======================================================================
   Options Grammar
========================================================================*/

opts(_,[]) --> [].
opts(P,[O:do|L]) --> opt0(P,O), opts(P,L). 
opts(P,[O:V|L]) --> opt1(P,O), value(V), opts(P,L). 
opts(P,[O:true|L]) --> opt1(P,O), {option(P,O,_,true,_)}, opts(P,L). 
opts(P,[O:V|L]) --> opt2(P,O), value(V), opts(P,L). 
opts(P,[O:V|L]) --> opt3(P,O), integer(V), opts(P,L). 
opts(P,[unknownoption:O|L]) --> opt4(P,O), opts(P,L). 
opts(P,[unknownoption:O|L]) --> opt4(P,O), value(_), opts(P,L). 
opts(P,[unknown:V|L]) --> value(V), opts(P,L). 

opt0(P,O) --> {option(P,O,0,_,_)}, [O].
opt1(P,O) --> {option(P,O,1,_,_)}, [O].
opt2(P,O) --> {option(P,O,-1,_,_)}, [O].
opt3(P,O) --> {option(P,O,-2,_,_)}, [O].
opt4(_,O) --> [O], {atom_chars(O,['-','-'|_])}.

value(V) --> [V], {atom_chars(V,[X,Y|_]),  \+ (X = '-', Y = '-')}.
value(V) --> [V], {atom_chars(V,[_])}.

integer(V) --> [Int], {atom_codes(Int,Codes), isInteger(Codes,0,V)}.


/* =======================================================================
   Check for Integer
========================================================================*/

isInteger([],Int,Int):- !.

isInteger([X|L],Old,Int):-
   X > 47, X < 58, !,
   New is (Old*10)+(X-48),
   isInteger(L,New,Int).


/* =======================================================================
   Dependent Options
========================================================================*/

dependentOptions(P):-
   findall(ifthen(A,B,C,D),dep(P,A,B,C,D),L),
   dependentOptions(L,P).

dependentOptions([],_):- !.

dependentOptions([ifthen(A,B,C,D)|L],P):-
   option(A,B), !, 
   setOption(P,C,D),
   dependentOptions(L,P).

dependentOptions([_|L],P):-
   dependentOptions(L,P).



/* =======================================================================
   Assert Options
========================================================================*/

assertOptions(_,[]).

assertOptions(P,[Option:do|L]):- 
   option(P,Option,0,_,_), !,
   retract(option(Option,_)),
   assert(option(Option,do)),
   assertOptions(P,L).

assertOptions(P,[Option:Value|L]):- 
   option(P,Option,-1,_,_), 
   atomic(Value), !,
   retract(option(Option,_)),
   assert(option(Option,Value)),
   assertOptions(P,L).

assertOptions(P,[Option:Value|L]):- 
   option(P,Option,-2,_,_), 
   number(Value), !,
   retract(option(Option,_)),
   assert(option(Option,Value)),
   assertOptions(P,L).

assertOptions(P,[Option:Value|L]):- 
   atomic(Value), 
   option(P,Option,1,Value,_), !,
   retract(option(Option,_)),
   assert(option(Option,Value)),
   assertOptions(P,L).

assertOptions(P,[unknownoption:Option|L]):- !, 
   error('option ~p not supported',[Option]),
   assertOptions(P,L).

assertOptions(P,[unknown:Unknown|L]):- !,
   error('argument ~p not interpreted',[Unknown]),
   assertOptions(P,L).

assertOptions(P,[Option:Value|L]):- 
   error('unknown value ~p for option ~p',[Value,Option]), !,
   assertOptions(P,L).


/* =======================================================================
   Default Options
========================================================================*/

setDefaultOptions(P):- 
   retractall(option(_,_)), 
   setof(Op,Ar^Val^Def^option(P,Op,Ar,Val,Def),Options), 
   setDefaultOptions(Options,P).

setDefaultOptions([],_):- !.

setDefaultOptions([X|L],P):-  
   option(P,X,_,_,D), !,
   assert(option(X,D)),  
   setDefaultOptions(L,P).


/* =======================================================================
   Display Options
========================================================================*/

showOptions(P):-  
   ( setof(O,V^D^(option(P,O,0,V,D),format(user_error,'  ~p~n',[O])),_), !; true ),
   ( setof(O,V^D^(option(P,O,-1,V,D),format(user_error,'  ~p <file>~n',[O])),_), !; true ), 
   ( setof(O,V^D^(option(P,O,-2,V,D),format(user_error,'  ~p <integer> (default: ~p)~n',[O,D])),_), !; true ),
   ( setof(o(O,D),V^option(P,O,1,V,D),Options), !; true ),
   findall(_,( member(o(O,D),Options),
               findall(V,option(P,O,1,V,_),L),
               format(user_error,'  ~p <arg> (possible values: ~p, default: ~p)~n',[O,L,D])),_), 
   nl(user_error).


/* =======================================================================
   Tokkie Options         % option(Option,NumberArgs,Value,Default)
========================================================================*/

option( tokkie, '--help',       0, _, dont       ).
option( tokkie, '--version',    0, _, dont       ).
option( tokkie, '--stdin',      0, _, dont       ).
option( tokkie, '--warnings',   1, V, false      ):- member(V,[true,false]).
option( tokkie, '--language',   1, V, en         ):- member(V,[en,it]).
option( tokkie, '--quotes',     1, V, keep       ):- member(V,[keep,delete]).
option( tokkie, '--mode',       1, V, poor       ):- member(V,[poor,iob,rich]).
option( tokkie, '--format',     1, V, txt        ):- member(V,[prolog,txt]).
option( tokkie, '--input',     -1, _, user_input ).
option( tokkie, '--output',    -1, _, user_output).


/* =======================================================================
   Nutcracker Options         % option(Option,NumberArgs,Value,Default)
========================================================================*/

option( nutcracker, '--help',          0, _, dont      ).
option( nutcracker, '--version',       0, _, dont      ).
option( nutcracker, '--force',         1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--soap',          1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--modal',         1, V, false     ):- member(V,[true,false]).
%option( nutcracker, '--vpe',           1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--plural',        1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--copula',        1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--resolve',       1, V, true      ):- member(V,[true,false]).
option( nutcracker, '--nn',            1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--x',             1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--wordnet',       1, V, true      ):- member(V,[true,false]).
option( nutcracker, '--warnings',      1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--info',          1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--graph',         1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--contradiction', 1, V, false     ):- member(V,[true,false]). % use theorem prover to check for contradictions
option( nutcracker, '--wsd',           1, V, false     ):- member(V,[true,false]).
option( nutcracker, '--roles',         1, V, proto     ):- member(V,[proto,verbnet]). % ,framenet]).
option( nutcracker, '--language',      1, V, en        ):- member(V,[en,it]).
option( nutcracker, '--inference',     1, V, yes       ):- member(V,[yes,no,only]).
option( nutcracker, '--tp',            1, V, bliksem   ):- member(V,[vampire,bliksem,otter]).
option( nutcracker, '--mb',            1, V, mace      ):- member(V,[mace,paradox]).
option( nutcracker, '--mbbis',         1, V, none      ):- member(V,[none,mace]).
option( nutcracker, '--domsize',      -2, _, 50        ).
option( nutcracker, '--timelim',      -2, _, 30        ).
option( nutcracker, '--dir',          -1, _, 'working' ).
option( nutcracker, '--axioms',       -1, _, 'none' ).


/* =======================================================================
   Boxer Options         % option(Option,NumberArgs,Value,Default)
========================================================================*/

option( boxer, '--help',       0, _, dont       ).
option( boxer, '--version',    0, _, dont       ).
option( boxer, '--stdin',      0, _, dont       ).
option( boxer, '--resolve',    1, V, false      ):- member(V,[true,false]).
option( boxer, '--integrate',  1, V, false      ):- member(V,[true,false]).
option( boxer, '--warnings',   1, V, false      ):- member(V,[true,false]).
option( boxer, '--instantiate',1, V, false      ):- member(V,[true,false]).
option( boxer, '--ccg',        1, V, false      ):- member(V,[true,false]).
option( boxer, '--elimeq',     1, V, false      ):- member(V,[true,false]).
option( boxer, '--box',        1, V, false      ):- member(V,[true,false]).
%option( boxer, '--vpe',        1, V, false      ):- member(V,[true,false]).
option( boxer, '--nn',         1, V, false      ):- member(V,[true,false]).
option( boxer, '--tense',      1, V, false      ):- member(V,[true,false]).
option( boxer, '--modal',      1, V, false      ):- member(V,[true,false]).
option( boxer, '--plural',     1, V, false      ):- member(V,[true,false]).
option( boxer, '--x',          1, V, false      ):- member(V,[true,false]).
option( boxer, '--copula',     1, V, true       ):- member(V,[true,false]).
option( boxer, '--tokid',      1, V, local      ):- member(V,[local,global]).
option( boxer, '--mwe',        1, V, no         ):- member(V,[no,yes,all]).
%option( boxer, '--presup',     1, V, max        ):- member(V,[min,max]).
option( boxer, '--theory',     1, V, drt        ):- member(V,[drt,sdrt]).
option( boxer, '--roles',      1, V, proto      ):- member(V,[proto,verbnet]).  % ,framenet]).
option( boxer, '--format',     1, V, prolog     ):- member(V,[prolog,xml,latex,dot,no]).
option( boxer, '--semantics',  1, V, drs        ):- member(V,[drs,pdrs,fol,drg,amr,tacitus,der]).
option( boxer, '--input',     -1, _, user_input ).
option( boxer, '--output',    -1, _, user_output).


/* =======================================================================
   Dependent Options         % if O1:V1 then set O2:V2
========================================================================*/

dep(boxer, '--semantics',amr, '--elimeq',true). 
dep(boxer, '--semantics',amr, '--copula',false). 
dep(boxer, '--semantics',amr, '--modal',true). 
dep(boxer, '--semantics',amr, '--theory',sdrt). 
