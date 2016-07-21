/* --------------------------------------------------------------------------
   Importing other modules
-------------------------------------------------------------------------- */

:- use_module(binarise,[binarise/3]).
:- use_module(ccg,[ccg/2]).
:- use_module(pp,[pp/2]).
:- use_module(printTree,[printBin/2]).
:- use_module(printTUT,[printTUT/2,lenTUT/2]).
:- use_module(printCCG,[printTokCat/2,printCCG/2,writeCCG/4]).

/* --------------------------------------------------------------------------
   Dynamic Predicates
-------------------------------------------------------------------------- */

:- dynamic switch/2.

/* --------------------------------------------------------------------------
   Switches (yes/no)
-------------------------------------------------------------------------- */

switch(tut,no).     % print TUT tree
switch(bin,no).     % print binary tree
switch(ppp,no).     % print CCG derivation (pre postprocessing)
switch(ccg,no).     % print CCG derivation (final derivation)
switch(pro,no).     % print CCG derivation (Prolog)
switch(lex,no).     % print lexical entry
switch(err,no).     % print errors

switch(pab,yes).     % abstract over punctuation categories
switch(sort,no).

/* --------------------------------------------------------------------------
   Exceptions (do not try to process these yet)
-------------------------------------------------------------------------- */

dont('ALB-100').               % wrong attachment
dont('ALB-245').               % coordination problem
dont('ALB-254').               % coordination of constituents of different type
dont('CODICECIVILE-364').      % idem (UNK)
dont('CODICECIVILE-56').       % coordination (?) problem, takes a long time, no analysis

dont('A-56').                  % problem with pro-drop/sentence coordination
dont('V-304').                 % problem with pro-drop/sentence coordination
dont('ALB-101').               % problem with pro-drop/sentence coordination
dont('CHIAM-8').               % problem with pro-drop/sentence coordination
dont('EVALITA-NEWSPAPER-80').  % problem with pro-drop/sentence coordination

dont('V-493').                 % problem with gliel'  (should be arg, not mod!) -> see mail Cristina (Oct 26, 2009)
dont('V-437').                 % something goes wrong here with punctuation... (UNK)

dont('ALB-22').                % sequence of two empty nodes
dont('ALB-128').               % sequence of two empty nodes
dont('ALB-257').               % sequence of two empty nodes
dont('A-37').                  % sequence of two empty nodes
dont('A-38').                  % sequence of two empty nodes
dont('A-46').                  % sequence of two empty nodes
dont('CHIAM-20').              % sequence of two empty nodes
dont('CODICECIVILE-288').      % sequence of two empty nodes
dont('CODICECIVILE-291').      % sequence of two empty nodes
dont('CODICECIVILE-461').      % sequence of two empty nodes (sopra o sotto il ...)

%   CODICECIVILE-19 % relative clause problem
%   Name = 'CODICECIVILE-18',  % coordination problem
%   Name = 'V-466',   % ne ne
%   Name = 'V-529',   % Sia
%   Name = 'ALB-98',    % funny "ma" + multiple conjunction
%   Name = 'ALB-26',  % relative clause
%   Name = 'ALB-172', % strange relative clause


/* --------------------------------------------------------------------------
   Process all TUT derivations
-------------------------------------------------------------------------- */

alltut(Stream):-
   switch(sort,no),
   tut(_,_,Name,_,Type:TUT),          % get a constituent tree
%   Name = 'V-437',
%   lenTUT(Type:TUT,Len), 
%   Len = 3,
   processTree(Name,Type,TUT,Stream),
   fail.                                                                  % take the next tree 

alltut(Stream):- 
   switch(sort,no), !,
   stats(user_output),
   close(Stream).

alltut(_):- 
   switch(sort,yes).

/* --------------------------------------------------------------------------
   Process one TUT tree
-------------------------------------------------------------------------- */

processTree(Name,Type,TUT,Stream):-
   increase(tree),                                                        % update counter

   ( switch(tut,yes), 
     format(Stream,'%%%% ~p~n',[Name]),                                   % print TUT format
     printTUT(Type:TUT,Stream)
   ;  \+ switch(tut,yes) ),    

   binarise(TUT,Name,Tree),                                               % binarise the TUT tree
   increase(bin),                                                         % update counter   
   ( switch(bin,yes), 
     format(Stream,'%%%% ~p~n',[Name]),                                   % print binary tree
     printBin(Type:Tree,Stream)
   ; \+ switch(bin,yes) ),  

   \+ dont(Name),

   ccg(Type:Tree,CCGTemp),                                                % convert it into a CCG derivation
   ( switch(ppp,yes), 
     printCCG(CCGTemp,Stream)
   ; \+ switch(ppp,yes) ),                                                % print pre-CCG derivation

   pp(CCGTemp,CCG),                                                       % apply post-processing
   increase(ccg),                                                         % update counter
   counter(ccg,CCGNo),
   ( switch(pro,yes), 
     writeCCG(CCG,Name,CCGNo,Stream)
   ; \+ switch(pro,yes) ),                                                % print CCG (prolog)

   ( switch(ccg,yes), 
     format(Stream,'%%%% ~p~n',[Name]),                                  
     printCCG(CCG,Stream)
   ; \+ switch(ccg,yes) ),                                                % print CCG derivation

   ( switch(lex,yes), 
     format(Stream,'%%%% ~p~n',[Name]),                                  
     printTokCat(CCG,Stream)
   ; \+ switch(lex,yes) ).                                                % print lexicon        


/* --------------------------------------------------------------------------
   Sorted on sentence length (wrapper)
-------------------------------------------------------------------------- */

sortbank(Stream):-
   switch(sort,yes), !,
   sortbank(1,Stream).

sortbank(_).


/* --------------------------------------------------------------------------
   Sorted on sentence length
-------------------------------------------------------------------------- */

sortbank(N,Stream):-
   tut(_,_,Name,_,Type:TUT),          % get a constituent tree
   lenTUT(Type:TUT,N), 
   processTree(Name,Type,TUT,Stream),
   fail.                                                                  % take the next tree of length N

%   \+ dont(Name),
%
%   binarise(TUT,Name,Tree),                                               % binarise the TUT tree
%   ccg(Type:Tree,CCGTemp),                                                % convert it into a CCG derivation
%   pp(CCGTemp,CCG),                                                       % apply post-processing
%   increase(ccg),                                                         % update counter
%   format(Stream,'%%%% ~p~n',[Name]), 
%   printCCG(CCG,Stream),

sortbank(N,Stream):-
   tut(_,_,_,_,Type:TUT),
   lenTUT(Type:TUT,M), M > N, !,
   I is N + 1,
   sortbank(I,Stream).

sortbank(_,Stream):-
   close(Stream).


/* --------------------------------------------------------------------------
   Stats
-------------------------------------------------------------------------- */

stats(Stream):-
   write(Stream,'Statistics'), nl(Stream), 
   write(Stream,'=========='), nl(Stream), 
   counter(tree,Tree),
   write(Stream,tut:Tree), nl(Stream),
   counter(bin,Bin),
   write(Stream,bin:Bin), nl(Stream),
   counter(ccg,CCG),
   write(Stream,ccg:CCG), nl(Stream), nl(Stream).


/* --------------------------------------------------------------------------
   File handling
-------------------------------------------------------------------------- */

output(Command,user_output):-
   parseCommand(Command,[File]), !,
   consult(File).

output(Command,Stream):-
   parseCommand(Command,[FileIn,FileOut]), !,
   consult(FileIn),
   open(FileOut,write,Stream).

/* --------------------------------------------------------------------------
   Main Predicate
-------------------------------------------------------------------------- */

run:-
   prolog_flag(argv,Command),
   output(Command,Stream),   
   alltut(Stream), 
   sortbank(Stream), !.

run.


/* --------------------------------------------------------------------------
   Parse Command
-------------------------------------------------------------------------- */

parseCommand([_,'-c',_Comm|Options],Files):- parseOptions(Options,Files), !.
parseCommand([_,_,'-c',_Comm|Options],Files):- parseOptions(Options,Files), !.

/* --------------------------------------------------------------------------
   Parse Options
-------------------------------------------------------------------------- */

parseOptions([],[]).

parseOptions(['--errors'|L1],L2):- !, 
   retractall(switch(err,_)), 
   assert(switch(err,yes)),
   parseOptions(L1,L2).

parseOptions(['--tut'|L1],L2):- !, 
   retractall(switch(tut,_)), 
   assert(switch(tut,yes)),
   parseOptions(L1,L2).

parseOptions(['--bin'|L1],L2):- !, 
   retractall(switch(bin,_)), 
   assert(switch(bin,yes)),
   parseOptions(L1,L2).

parseOptions(['--ccg'|L1],L2):- !, 
   retractall(switch(ccg,_)), 
   assert(switch(ccg,yes)),
   parseOptions(L1,L2).

parseOptions(['--pro'|L1],L2):- !, 
   retractall(switch(pro,_)), 
   assert(switch(pro,yes)),
   parseOptions(L1,L2).

parseOptions(['--sort'|L1],L2):- !, 
   retractall(switch(sort,_)), 
   assert(switch(sort,yes)),
   parseOptions(L1,L2).

parseOptions(['--lex'|L1],L2):- !, 
   retractall(switch(lex,_)), 
   assert(switch(lex,yes)),
   parseOptions(L1,L2).

parseOptions([X|L1],[X|L2]):- parseOptions(L1,L2).

/* --------------------------------------------------------------------------
   Counting
-------------------------------------------------------------------------- */

:- dynamic counter/2.

increase(X):-
   \+ counter(X,_), !,
   assert(counter(X,1)).

increase(X):-
   retract(counter(X,N)),
   M is N + 1,
   assert(counter(X,M)), !.


/* --------------------------------------------------------------------------
   Starting
-------------------------------------------------------------------------- */

:- run, halt.

