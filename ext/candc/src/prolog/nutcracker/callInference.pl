
:- module(callInference,[callTPandMB/8,
                         callMBbis/7]).

:- use_module(library(lists),[member/2,select/3]).

:- use_module(nutcracker(fol2otter),[fol2otter/3,fol2mace/3]).
:- use_module(nutcracker(fol2bliksem),[fol2bliksem/3]).
:- use_module(nutcracker(fol2tptp),[fol2tptp/3,fol2tptpOld/3]).

:- use_module(semlib(options),[option/2]).


/*========================================================================
   Initialise Theorem Provers
========================================================================*/

initEngine(Opt,Temp,Axioms,Formula,vampire):-   
   option(Opt,vampire), 
   access_file('ext/bin/vampire',execute), !,
   atom_concat(Temp,'/vampire.in',InFile),
   open(InFile,write,Stream),
   fol2tptpOld(Axioms,Formula,Stream),
   close(Stream).

initEngine(Opt,Temp,Axioms,Formula,otter):- 
   option(Opt,otter), 
   access_file('ext/bin/otter',execute), !,
   atom_concat(Temp,'/otter.in',InFile),
   open(InFile,write,Stream),
   fol2otter(Axioms,not(Formula),Stream),
   close(Stream).

initEngine(Opt,Temp,Axioms,Formula,bliksem):- 
   option(Opt,bliksem), 
   access_file('ext/bin/bliksem',execute), !,
   atom_concat(Temp,'/bliksem.in',InFile),
   open(InFile,write,Stream),
   fol2bliksem(Axioms,not(Formula),Stream),
   close(Stream).

initEngine(Opt,Temp,Axioms,Formula,mace):- 
   option(Opt,mace),
   access_file('ext/bin/mace',execute), !,
   atom_concat(Temp,'/mace.in',InFile),
   open(InFile,write,Stream),
   fol2mace(Axioms,Formula,Stream),
   close(Stream).

initEngine(Opt,Temp,Axioms,Formula,paradox):- 
   option(Opt,paradox),
   access_file('ext/bin/paradox',execute), !,
   atom_concat(Temp,'/paradox.in',InFile),
   open(InFile,write,Stream),
   fol2tptp(Axioms,not(Formula),Stream),
   close(Stream).

initEngine(Opt,_,_,_,_):- 
   option(Opt,X),
   error('inference engine ext/bin/~p not accessible',[X]),
   !, fail.


/* ========================================================================
   Time Limit
======================================================================== */

timeLimit(TimeLim):-
   option('--timelim',TimeLim),
   access_file('ext/bin/CPULimitedRun',execute), !.

timeLimit(0).


/* ========================================================================
   Calls to Theorem Provers and Model Builders
======================================================================== */

callTPandMB(Dir,Axioms,TPProblem,MBProblem,MinDom,MaxDom,Model,Engine):-
   timeLimit(TimeLim),
   initEngine('--tp',Dir,Axioms,TPProblem,TP),
   initEngine('--mb',Dir,Axioms,MBProblem,MB),
   atomic_list_concat(['perl ./src/prolog/nutcracker/startTPandMB.pl ',
                       Dir,' ',TimeLim,' ',MinDom,' ',MaxDom,' ',TP,MB],Shell),        
   shell(Shell,Return), Return = 0,
   readResult(Model,Dir,Engine).


/* ========================================================================
   Call to Model Builder ("second opinion")
======================================================================== */

callMBbis(_,_,_,Model,Model,Engine,Engine):-
   option('--mbbis',none), !.

callMBbis(Dir,Axioms,MBProblem,FirstModel,Model,FirstEngine,Engine):-
   FirstModel = model(Dom,_), length(Dom,DomSize), DomSize > 0,
   timeLimit(TimeLim),
   initEngine('--mbbis',Dir,Axioms,MBProblem,MB),
   atomic_list_concat(['perl ./src/prolog/nutcracker/startTPandMB.pl ',
                       Dir,' ',TimeLim,' ',DomSize,' ',DomSize,' ',MB],Shell),        
   shell(Shell,Return), Return = 0,
   readResult(SecondModel,Dir,SecondEngine), !,
   ( SecondModel = unknown, Model = FirstModel, Engine = FirstEngine, !
   ; Model = SecondModel, Engine = SecondEngine ).

callMBbis(_,_,_,Model,Model,Engine,Engine).


/* ========================================================================
   Read result and translatate into standard format
======================================================================== */

readResult(Model,Temp,Engine):-
   atom_concat(Temp,'/tpmb.out',File),
   open(File,read,Out),
   read(Out,Result),
   (
      Result=proof, !, 
      read(Out,engine(Engine)),
      Model=model([],[])
   ;
      Result=interpretation(_,_), !,
      read(Out,engine(Engine)),
      mace2blackburnbos(Result,Model)
   ;
      Result=paradox(_), !,
      read(Out,engine(Engine)), 
      paradox2blackburnbos(Result,Model)
   ;
      Model=unknown,
      Engine=unknown
   ),       
   close(Out).


/*========================================================================
   Translate Paradox-type Model into Blackburn & Bos Models
========================================================================*/

paradox2blackburnbos(Paradox,model(D,F)):-
   Paradox = paradox(Terms), \+ Terms=[],
   paradox2d(Terms,[d1]-D),
   paradox2f(Terms,[]-F).

paradox2blackburnbos(Paradox,model([],[])):-
   Paradox = paradox([]).

paradox2blackburnbos(Paradox,unknown):-
   \+ Paradox = paradox(_).


/*========================================================================
   Translate Paradox Terms to Domain
========================================================================*/

paradox2d([],D-D).

paradox2d([_Constant=Entity|L],D1-D2):-
   \+ member(Entity,D1), !,
   paradox2d(L,[Entity|D1]-D2).

paradox2d([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,1), 
   \+ Functor = '$',
   arg(1,Symbol,Entity),
   \+ member(Entity,D1), !,
   paradox2d(L,[Entity|D1]-D2).

paradox2d([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,2),
   \+ Functor = '$',
   arg(1,Symbol,Entity1),
   arg(2,Symbol,Entity2),
   (
      \+ member(Entity1,D1), !,
      (
         \+ member(Entity2,D2), !,
         paradox2d(L,[Entity1,Entity2|D1]-D2)
      ;
         paradox2d(L,[Entity1|D1]-D2) 
      )
   ;
      \+ member(Entity2,D2), 
      paradox2d(L,[Entity2|D1]-D2) 
   ), !.

paradox2d([_|L],D1-D2):-
   paradox2d(L,D1-D2).


/*========================================================================
   Translate Paradox Terms to Interpretation Function
========================================================================*/

paradox2f([],F-F).

paradox2f([Constant=Entity|L],D1-D2):-
   Term = f(0,Constant,Entity),
   \+ member(Term,D1), !,
   paradox2f(L,[Term|D1]-D2).

paradox2f([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,1), 
   \+ Functor = '$', !,
   arg(1,Symbol,Arg),
   (
      select(f(1,Functor,E),D1,D3), !,
      paradox2f(L,[f(1,Functor,[Arg|E])|D3]-D2)
   ;
      paradox2f(L,[f(1,Functor,[Arg])|D1]-D2)
   ).

paradox2f([Symbol:0|L],D1-D2):-
   functor(Symbol,Functor,1), 
   \+ Functor = '$', !,
   (
      member(f(1,Functor,_),D1), !,
      paradox2f(L,D1-D2)
   ;
      paradox2f(L,[f(1,Functor,[])|D1]-D2)
   ).

paradox2f([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,2), 
   \+ Functor = '$', !,
   arg(1,Symbol,Arg1),
   arg(2,Symbol,Arg2),
   (
      select(f(2,Functor,E),D1,D3), !,
      paradox2f(L,[f(2,Functor,[(Arg1,Arg2)|E])|D3]-D2)
   ;
      paradox2f(L,[f(2,Functor,[(Arg1,Arg2)])|D1]-D2)
   ).

paradox2f([Symbol:0|L],D1-D2):-
   functor(Symbol,Functor,2), 
   \+ Functor = '$', !,
   (
      member(f(2,Functor,_),D1), !,
      paradox2f(L,D1-D2)
   ;
      paradox2f(L,[f(2,Functor,[])|D1]-D2)
   ).

paradox2f([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,3), 
   \+ Functor = '$', !,
   arg(1,Symbol,Arg1),
   arg(2,Symbol,Arg2),
   arg(3,Symbol,Arg3),
   (
      select(f(3,Functor,E),D1,D3), !,
      paradox2f(L,[f(3,Functor,[(Arg1,Arg2,Arg3)|E])|D3]-D2)
   ;
      paradox2f(L,[f(3,Functor,[(Arg1,Arg2,Arg3)])|D1]-D2)
   ).

paradox2f([Symbol:0|L],D1-D2):-
   functor(Symbol,Functor,3), 
   \+ Functor = '$', !,
   (
      member(f(3,Functor,_),D1), !,
      paradox2f(L,D1-D2)
   ;
      paradox2f(L,[f(3,Functor,[])|D1]-D2)
   ).

paradox2f([_|L],D1-D2):-
   paradox2f(L,D1-D2).


/*========================================================================
   Translate Mace-type Model into Blackburn & Bos Models
========================================================================*/

mace2blackburnbos(Mace,model(D,F)):-
   Mace = interpretation(Size,Terms),
   mace2d(1,Size,D),
   mace2f(Terms,D,F).

mace2blackburnbos(Mace,unknown):-
   \+ Mace = interpretation(_Size,_Terms).


/*========================================================================
   Translate Mace Model to Domain
========================================================================*/

mace2d(N,N,[V]):-
	name(N,Codes),
	name(V,[100|Codes]).

mace2d(I,N,[V|D]):-
	I < N,
	name(I,Codes),
	name(V,[100|Codes]),
	J is I + 1,
	mace2d(J,N,D).


/*========================================================================
   Translate Mace Model to Interpretation Function
========================================================================*/

mace2f([],_,[]):- !.

mace2f([function(Skolem,_)|Terms],D,F):-
	\+ atom(Skolem), !,
	mace2f(Terms,D,F).

mace2f([function(Constant,[V])|Terms],D,[f(0,Constant,X)|F]):-
	atom(Constant), !,
	Index is V + 1,
	name(Index,Codes),
	name(X,[100|Codes]),
	mace2f(Terms,D,F).

mace2f([predicate(Relation,V)|Terms],D,[f(1,Functor,X)|F]):-
	Relation =.. [Functor,_], !,
	positiveValues(V,1,X),
	mace2f(Terms,D,F).

mace2f([predicate(Relation,V)|Terms],D,[f(2,Functor,X)|F]):-
	Relation =.. [Functor,_,_], !,
	length(D,Size),
	positivePairValues(V,Size,1,1,X),
	mace2f(Terms,D,F).

mace2f([predicate(Relation,_V)|Terms],D,[f(3,Functor,X)|F]):-
	Relation =.. [Functor,_,_,_], !,
%	length(D,Size),
%	positivePairValues(V,Size,1,1,X), 
        X=[],   % hack for now
	mace2f(Terms,D,F).

mace2f([_|Terms],D,F):-
	mace2f(Terms,D,F).


/*========================================================================
   Take positive values of one-place predicates
========================================================================*/

positiveValues([],_,[]).

positiveValues([1|Values],I1,[X|Rest]):-
	name(I1,Codes),
	name(X,[100|Codes]),
	I2 is I1 + 1,
	positiveValues(Values,I2,Rest).
		
positiveValues([0|Values],I1,Rest):-
	I2 is I1 + 1,
	positiveValues(Values,I2,Rest).
		

/*========================================================================
   Take positive values of two-place predicates
========================================================================*/

positivePairValues([],_,_,_,[]).

positivePairValues([1|Values],Size,I1,J1,[(X2,X1)|Rest]):-
	name(I1,Codes1),
	name(X1,[100|Codes1]),
	name(J1,Codes2),
	name(X2,[100|Codes2]),
	(
	    I1 < Size,
	    I2 is I1 + 1,
	    J2 is J1
	;   
	    I1 = Size,
	    I2 = 1,
	    J2 is J1 + 1
	),
	positivePairValues(Values,Size,I2,J2,Rest).

positivePairValues([0|Values],Size,I1,J1,Rest):-
	(
	    I1 < Size, 
	    I2 is I1 + 1,
	    J2 is J1
	;
	    I1 = Size,
	    I2 = 1,
	    J2 is J1 + 1
	),
	positivePairValues(Values,Size,I2,J2,Rest).

