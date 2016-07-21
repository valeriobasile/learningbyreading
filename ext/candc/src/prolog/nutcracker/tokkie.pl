
% tokkie.pl, by Johan Bos

/*========================================================================
   File Search Paths
========================================================================*/

file_search_path(semlib, 'src/prolog/lib').
file_search_path(boxer,  'src/prolog/boxer').


/*========================================================================
   Load other libraries
========================================================================*/

:- use_module(library(lists),[member/2,append/3]).
:- use_module(library(readutil),[read_line_to_codes/2]).
:- use_module(boxer(version),[version/1]).
:- use_module(semlib(errors),[error/2,warning/2]).
:- use_module(semlib(options),[option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).


/*========================================================================
   Main
========================================================================*/

tokkie:-
   option(Option,do), 
   member(Option,['--version','--help']), !, 
   version,
   help.

tokkie:-
   openInput(InStream),
   openOutput(OutStream), !,
   read_line_to_codes(InStream,Codes),
   readLines(Codes,InStream,OutStream).

tokkie:-
   setOption(tokkie,'--help',do), !,
   help.
  

/* ----------------------------------------------------------------------
   Read lines
---------------------------------------------------------------------- */

readLines(end_of_file,Stream1,Stream2):- !,
   close(Stream1),
   close(Stream2).

readLines(Codes,InStream,OutStream):- !,
   tok(Codes,TokCodes,Last),
   format(OutStream,'~s',[TokCodes]),
   read_line_to_codes(InStream,NewCodes),
   decideNewLine(NewCodes,Last,OutStream),
   readLines(NewCodes,InStream,OutStream).


/* =======================================================================
   Determine New Line
========================================================================*/

decideNewLine(end_of_file,_Last,Stream):- !, nl(Stream).
decideNewLine([C1,C2|_],_,Stream):- lower(C1), lower(C2), !, write(Stream,' ').
decideNewLine(_,Last,Stream):- title(Last), !, write(Stream,' ').
decideNewLine(_,Last,_Stream):- mistake(Last), !.
decideNewLine(_,_Last,Stream):- nl(Stream).


/* =======================================================================
   Tokenise
========================================================================*/

tok([],[],[]):- !.
tok([65533|L1],L2,Last):- !, warning('skipping non-utf8 character',[]), tok(L1,L2,Last).
tok([32|L1],L2,Last):- !, tok(L1,L2,Last).
tok([9|L1],L2,Last):- !, tok(L1,L2,Last).
tok(L1,L2,Last):- tok(L1,[],L2,Last).

tok([],Last,[],Last):- !.
tok([65533|L1],Prev,L2,Last):- !, warning('skipping non-utf8 character',[]), tok(L1,Prev,L2,Last).
tok(P1,Prev,P2,Last):- pattern(P1-L1,Prev,P2-L2,Next), !, tok(L1,Next,L2,Last).
tok([32|L1],_,[32|L2],Last):- !, tok(L1,[],L2,Last).
tok([X|L1],Prev,[X|L2],Last):- tok(L1,[X|Prev],L2,Last).


/* ==================================================================================
   Patterns
================================================================================== */

/* ----------------------------------------------------------------------------------
   Remove space at end of line
---------------------------------------------------------------------------------- */

pattern([32]-[], X, B-B, X):- !.
pattern([9]-[],  X, B-B, X):- !.

/* ----------------------------------------------------------------------------------
   Squeeze space
---------------------------------------------------------------------------------- */

pattern([32,32|A]-A, [], B-B, []):- !.                         %%% double space
pattern([32,32|A]-[32|A], X, B-B, X):- !.                      %%% double space
pattern([9,32|A]-[32|A],  X, B-B, X):- !.                      %%% double space/tab
pattern([32,9|A]-[32|A],  X, B-B, X):- !.                      %%% double space/tab
pattern([9,9|A]-[32|A],   X, B-B, X):- !.                      %%% double tab
pattern([9|A]-[32|A],     X, B-B, X):- !.                      %%% tab -> space

/* ----------------------------------------------------------------------------------
   Dot dot dot (end of line)
   If the last token before the ... is an abbreviation, an extra . is preserved
---------------------------------------------------------------------------------- */

pattern(D-[], Prev, [46,32,46,46,46|B]-B, [46,46,46]):- dots(D,A), end(A), abb(Prev), !. 
pattern(D-[], Prev, B1-B2, [46,46,46]):- dots(D,A),end(A), !, insertSpace(Prev,[46,46,46|B2],B1).  

/* ----------------------------------------------------------------------------------
   Dot dot dot (not end of line)
---------------------------------------------------------------------------------- */

pattern(D-[L|A], Prev, B1-B2,[]):- dots(D,[L|A]), lower(L), !, insertSpace(Prev,[46,46,46,32|B2],B1).
pattern(D-[L|A], Prev, B1-B2,[]):- dots(D,[L|A]), upper(L), !, insertSpace(Prev,[46,46,46,10|B2],B1).
pattern(D-A,     Prev, B1-B2,[]):- dots(D,A), !, insertSpace(Prev,[46,46,46,32|B2],B1).

/* ----------------------------------------------------------------------------------
   Full stop and bracket (end of line)
---------------------------------------------------------------------------------- */

pattern([46,Q|A]-[], Prev, B1-B2, [Q]):- bracket(Q), end(A), !, insertSpace(Prev,[46,32,Q|B2],B1).   %%% X.) -> X . )

/* ----------------------------------------------------------------------------------
   Full stop and ending quotes (end of line)
---------------------------------------------------------------------------------- */

pattern([46,Q|A]-[], Prev, B1-B2, [46]):- quote(Q),end(A),option('--quotes',delete), !, insertSpace(Prev,[46|B2],B1).      %%% X." -> X .
pattern([46,Q|A]-[], Prev, B1-B2,  [Q]):- quote(Q),end(A),option('--quotes',keep), !, insertSpace(Prev,[46,32,Q|B2],B1).   %%% X." -> X . "

pattern([46,Q,Q|A]-[], Prev, B1-B2, [46]):- quotes(Q),end(A),option('--quotes',delete), !, insertSpace(Prev,[46|B2],B1).       %%% X.'' -> X .
pattern([46,Q,Q|A]-[], Prev, B1-B2,  [Q]):- quotes(Q),end(A),option('--quotes',keep), !, insertSpace(Prev,[46,32,Q,Q|B2],B1).  %%% X.'' -> X . ''

pattern([46,32,Q1,Q2|A]-[], Prev, B1-B2,  [46]):- quote(Q1),quote(Q2),\+Q1=Q2,end(A),option('--quotes',delete), !, insertSpace(Prev,[46|B2],B1).            %%% X. '" -> X . ' "
pattern([46,32,Q1,Q2|A]-[], Prev, B1-B2,  [Q2]):- quote(Q1),quote(Q2),\+Q1=Q2,end(A),option('--quotes',keep), !, insertSpace(Prev,[46,32,Q1,32,Q2|B2],B1).  %%% X. '" -> X . ' "

/* ----------------------------------------------------------------------------------
   Full stop and ending quotes (not end of line)
---------------------------------------------------------------------------------- */

pattern([46,Q,32,U|A]-[U|A], Prev, B1-B2, []):- quote(Q),upper(U),option('--quotes',delete), !, insertSpace(Prev,[46,10|B2],B1). %%% X." U
pattern([46,Q,32,U|A]-[U|A], Prev, B1-B2, []):- quote(Q),upper(U),option('--quotes',keep), !, insertSpace(Prev,[46,32,Q,10|B2],B1). %%% X." U
pattern([46,Q,32,U|A]-[U|A], Prev, B1-B2, []):- closing_bracket(Q),upper(U), !, insertSpace(Prev,[46,32,Q,10|B2],B1). %%% X.) U

/* ----------------------------------------------------------------------------------
   Full stop (end of line)
   If the last token before the . is an abbreviation, no extra . is produced.
---------------------------------------------------------------------------------- */

pattern([46|A]-[], Prev, [46|B]-B, Prev):- end(A), title(Prev), !.                   %%% X. -> X. 
pattern([46|A]-[], Prev, [46|B]-B, [46|Prev]):- end(A), abb(Prev), !.                %%% X. -> X. 
pattern([46|A]-[], Prev, B1-B2, [46]):- end(A), !, insertSpace(Prev,[46|B2],B1).     %%% X. -> X . 

/* ----------------------------------------------------------------------------------
   Full stop, followed by opening quote
---------------------------------------------------------------------------------- */

pattern([46,32,Q,115|A]-A, [_|_], [46,32,Q,115|B]-B, [115,Q]):- rsq(Q), !.   %% U.S. \'s
pattern([46,32,Q,C|A]-[Q,C|A],     Prev, B1-B2, []):- quote(Q), upper(C), !, insertSpace(Prev,[46,10|B2],B1).
pattern([46,32,Q,Q,C|A]-[Q,Q,C|A], Prev, B1-B2, []):- quotes(Q), upper(C), !, insertSpace(Prev,[46,10|B2],B1).
pattern([46,32,Q,C|A]-[Q,C|A],     Prev, B1-B2, []):- opening_bracket(Q), upper(C), !, insertSpace(Prev,[46,10|B2],B1).

/* ----------------------------------------------------------------------------------
   Full stop (not end of line), next token starts with uppercase --- arhhhhh....
   Case 1: A full stop after a space -> sentence boundary.
   Case 2: A full stop after a one-character token --> initial, no sentence boundary
   Case 3: A full stop after a title --> no sentence boundary
   Case 4: A full stop after a non-abbreviation --> sentence boundary
%  Case 5: A full stop after abbreviation --> no sentence boundary
---------------------------------------------------------------------------------- */

pattern([46,32,U|A]-[U|A], [], [46,10|B]-B,   []):- upper(U), !.
pattern([46,32,U|A]-[U|A], [_], [46,32|B]-B,  []):- upper(U), !.    %%% Initial
pattern([46,32,U|A]-[U|A], Prev, [46,32|B]-B, []):- upper(U), title(Prev), !.
pattern([46,32,U|A]-[U|A], Prev, [32,46,10|B]-B, []):- upper(U), \+ abb(Prev), !.
%pattern([46,32,U|A]-[U|A], Prev, [46,10|B]-B, []):- upper(U), abb(Prev), !.

pattern([46,32,32,U|A]-[U|A], [], [46,10|B]-B,   []):- upper(U), !.
pattern([46,32,32,U|A]-[U|A], [_], [46,32|B]-B,  []):- upper(U), !.    %%% Initial
pattern([46,32,32,U|A]-[U|A], Prev, [46,32|B]-B, []):- upper(U), title(Prev), !.
pattern([46,32,32,U|A]-[U|A], Prev, [32,46,10|B]-B, []):- upper(U), \+ abb(Prev), !.

/* ----------------------------------------------------------------------------------
   The comma
---------------------------------------------------------------------------------- */

pattern([X,44,Y|A]-[Y|A], P, [X,44|B]-B, [44|P]):- num(X), num(Y), !.   %%% "0,0" -> "0,0"
pattern([44|A]-[32|A],  Prev, B1-B2, [44]):- !, insertSpace(Prev,[44|B2],B1).  %%% "X," -> "X , "

/* ----------------------------------------------------------------------------------
   The brackets
---------------------------------------------------------------------------------- */

pattern([X|A]-[32|A], Prev, B1-B2, [X]):- bracket(X), !, insertSpace(Prev,[X|B2],B1).

/* ----------------------------------------------------------------------------------
   Colon
---------------------------------------------------------------------------------- */

pattern([58|A]-[32|A], Prev, B1-B2, [58]):- !, insertSpace(Prev,[58|B2],B1).

/* ----------------------------------------------------------------------------------
   Semicolon
---------------------------------------------------------------------------------- */

pattern([59|A]-[32|A], Prev, B1-B2, [59]):- !, insertSpace(Prev,[59|B2],B1).

/* ----------------------------------------------------------------------------------
   Question and Exclamation Mark
---------------------------------------------------------------------------------- */

pattern([X|A]-[32|A], Prev, B1-B2, [X]):- mark(X), !, insertSpace(Prev,[X|B2],B1).

/* ----------------------------------------------------------------------------------
   Percentage     "100%" -> "100 % "
---------------------------------------------------------------------------------- */

pattern([X,37|A]-[32|A], _, [X,32,37|B]-B, [37]):- num(X), !.          

/* ----------------------------------------------------------------------------------
   Monetary units  "$100" -> "$ 100"
---------------------------------------------------------------------------------- */

pattern([36,X|A]-[X|A], _, [36,32|B]-B, [32]):- num(X), !.        
pattern([128,X|A]-[X|A], _, [128,32|B]-B, [32]):- num(X), !.        

/* ----------------------------------------------------------------------------------
   Contractions: year/decade expressions
---------------------------------------------------------------------------------- */

pattern([Q,N1,N2,115|A]-A, [], [Q,N1,N2,115|B]-B, [115,N2,N1,Q]):- rsq(Q), num(N1),num(N2), !.  %%% "'30s" -> "'30s"
pattern([Q,N1,N2,N|A]-[N|A], [], [Q,N1,N2|B]-B, [N2,N1,Q]):- rsq(Q), num(N1),num(N2), \+ alphanum(N), !.  %%% "'30" -> "'30"

/* ----------------------------------------------------------------------------------
   Contractions: \'s (English)
---------------------------------------------------------------------------------- */

pattern([X,Q,115,N|A]-[N|A], [_|_], [X,32,Q,115|B]-B, [115,Q]):- option('--language',en), rsq(Q), alpha(X), \+ alphanum(N), !.  %%% "X's" -> "X 's"
pattern([Q,115,N|A]-[N|A], Prev, [32,Q,115|B]-B, [115,Q]):- option('--language',en), abb(Prev), rsq(Q), \+ alphanum(N), !.  %%% "U.S.'s" -> "U.S. 's"
pattern([X,Q,83,N|A]-[N|A],  [_|_], [X,32,Q,83|B]-B,  [83,Q]):- option('--language',en), rsq(Q), alpha(X), \+ alphanum(N), !.   %%% "X'S" -> "X 'S"
pattern([Q,115,N|A]-[N|A], [], [Q,115|B]-B, [115,Q]):- option('--language',en), rsq(Q), \+ alphanum(N), !.  %%% " 's" -> " 's"
pattern([115,Q,N|A]-[N|A], [_|_], [115,32,Q|B]-B, [Q,115]):- option('--language',en), rsq(Q), \+ alphanum(N), !.  %%% "s' " -> "s ' "

/* ----------------------------------------------------------------------------------
   Contractions: auxiliary verbs (English)
---------------------------------------------------------------------------------- */

pattern([X,Q,109|A]-A,     _, [X,32,Q,109|B]-B,         [109,Q]):- option('--language',en), rsq(Q), alpha(X), !.  %%% "X'm" -> "X 'm"
pattern([X,Q,100|A]-A,     _, [X,32,Q,100|B]-B,         [100,Q]):- option('--language',en), rsq(Q), alpha(X), !.  %%% "X'd" -> "X 'd"
pattern([X,Q,108,108|A]-A, _, [X,32,Q,108,108|B]-B, [108,108,Q]):- option('--language',en), rsq(Q), alpha(X), !.  %%% "X'll" -> "X 'll"
pattern([X,Q,118,101|A]-A, _, [X,32,Q,118,101|B]-B, [101,118,Q]):- option('--language',en), rsq(Q), alpha(X), !.  %%% "X've" -> "X 've"
pattern([X,Q,114,101|A]-A, _, [X,32,Q,114,101|B]-B, [101,114,Q]):- option('--language',en), rsq(Q), alpha(X), !.  %%% "X're" -> "X 're"
pattern([X,110,Q,116|A]-A, _, [X,32,110,Q,116|B]-B, [116,Q,110]):- option('--language',en), rsq(Q), alpha(X), !.  %%% "Xn't" -> "X n't"

/* ----------------------------------------------------------------------------------
   Contractions (Italian)
---------------------------------------------------------------------------------- */

pattern([108,Q,X|A]-[X|A],   Prev, B1-B2, []):- option('--language',it), alpha(X), rsq(Q), !, insertSpace(Prev,[108,Q,32|B2],B1).   %%% " l'X" -> " l' X"


/* ----------------------------------------------------------------------------------
   Contractions: Irish and foreign names
---------------------------------------------------------------------------------- */

pattern([U1,Q,U2|A]-A, [], [U1,Q,U2|B]-B, [U2,Q,U1]):- rsq(Q), alpha(U1),alpha(U2).  %%% "O'R" -> "O'R"

/* ----------------------------------------------------------------------------------
   Double character quotes
---------------------------------------------------------------------------------- */

pattern([32,Q,Q,32|A]-[32|A], X, B-B, X):- quotes(Q), option('--quotes',delete), !.
pattern([Q,Q|A]-A, X, B-B, X):- quotes(Q), option('--quotes',delete), !.
pattern([X,X|A]-[32|A], Prev, B1-B2, [X,X]):- quotes(X), !, insertSpace(Prev,[X,X|B2],B1).

/* ----------------------------------------------------------------------------------
   Single character quotes
---------------------------------------------------------------------------------- */

pattern([32,Q,32|A]-[32|A], X, B-B, X):- quote(Q), option('--quotes',delete), !.
pattern([Q|A]-A, X, B-B, X):- quote(Q), option('--quotes',delete), !.
pattern([X|A]-[32|A], Prev, B1-B2, [X]):- quote(X), !, insertSpace(Prev,[X|B2],B1).   


/* ==================================================================================
   Aux Predicates
====================================================================================*/

alphanum(X):- alpha(X), !.
alphanum(X):- num(X), !.

alpha(62):- !.                         %%% '>' (end of markup)
alpha(X):- upper(X), !.
alpha(X):- lower(X), !.

upper(X):- X > 64, X < 91, !.
lower(X):- X > 96, X < 123, !.

num(X):- X > 47, X < 58, !.


/* ----------------------------------------------------------------------------------
   Insert space, but only if there is a token just before
---------------------------------------------------------------------------------- */

insertSpace([], L, L):- !.
insertSpace( _, L, [32|L]).


/* ----------------------------------------------------------------------------------
   Codes for Brackets
---------------------------------------------------------------------------------- */

bracket(X):- opening_bracket(X).
bracket(X):- closing_bracket(X).

opening_bracket(40).  %%% (
opening_bracket(91).  %%% [
opening_bracket(123). %%% {

closing_bracket(41).  %%% )
closing_bracket(93).  %%% ]
closing_bracket(125). %%% }


/* ----------------------------------------------------------------------------------
   Codes for right single quotation marks (used in genitives)
---------------------------------------------------------------------------------- */

rsq(39).
rsq(8217).


/* ----------------------------------------------------------------------------------
   Codes for single-character quotes
---------------------------------------------------------------------------------- */

quote(34).    %%% "
quote(39).    %%% '
quote(96).    %%% `
quote(8216).  %%% left single quotation mark
quote(8217).  %%% right single quotation mark
quote(8218).  %%% low single quotation mark
quote(8220).  %%% left double quotation mark
quote(8221).  %%% right double quotation mark
quote(8222).  %%% low double quotation mark


/* ----------------------------------------------------------------------------------
   Codes for double quotes
---------------------------------------------------------------------------------- */

quotes(96).    %%% ``
quotes(39).    %%% ''
quotes(8216).
quotes(8217).
quotes(8218).

/* ----------------------------------------------------------------------------------
   Codes for punctuation marks
---------------------------------------------------------------------------------- */

mark(63).    %%% ?
mark(33).    %%% !


/* ----------------------------------------------------------------------------------
   Titles (or other expressions that never/rarely occur at end of sentence)
   The actual string (in double quotes) is reversed!
---------------------------------------------------------------------------------- */

title(Title):- option('--language',Language), title(Language,Title), !.

title(en, "rM").           % Mr     sg
title(en, "srsseM").       % Messrs pl
title(en, "srM").          % Mrs    sg
title(en, "semM").         % Mmes   pl
title(en, "sM").           % Ms
title(en, "rD").           % Dr     sg
title(en, "srD").          % Drs    pl
title(en, "forP").         % Prof
title(en, "neS").          % Sen
title(en, "voG").          % Gov
title(en, "tS").           % St    (for Saint)
title(en, "peR").          % Rep
title(en, "neG").          % Gen
title(en, "tL").           % Lt    Lieutenant
title(en, "tueiL").        % Lieut    Lieutenant
title(en, "loC").          % Col   Colonel
title(en, "mdA").          % Adm   Admiral
title(en, "tpC").          % Cpt   Captain
title(en, "veR").          % Rev   Reverend
title(en, "noH").          % Hon   Honoroble
title(en, "tpaC").         % Capt
title(en, "rdmC").         % Cmdr
title(en, "nlpahc").       % Chapln

title(en, "v").            % v
title(en, "sv").           % vs
title(en, "eiC").          % Cie
title(en, "a.k.a").        % a.k.a
title(en, "tM").           % Mt    Mount

/* ----------------------------------------------------------------------------------
   Abbreviations
---------------------------------------------------------------------------------- */

abb(Codes):- member(46,Codes), member(X,Codes), alpha(X), !.
abb(Abb):- option('--language',Language), abb(Language,Abb), !.

abb(en, "proC"). % Corp
abb(en, "cnI").  % Inc
abb(en, "oC").   % Co
abb(en, "dtL").  % Ltd
abb(en, "rJ").   % Jr
abb(en, "rS").   % Sr
abb(en, "soC").  % Cos
abb(en, "sorB"). % Bros
abb(en, "cte").  % etc


/* ----------------------------------------------------------------------------------
   Mistake in WSJ tokenisation
---------------------------------------------------------------------------------- */

mistake(".p.S").


/* =======================================================================
   End (only spaces or tabs before end of line)
========================================================================*/

end([]):- !.
end([32|L]):- !, end(L).
end([9|L]):- !, end(L).


/* =======================================================================
   Dots
========================================================================*/

dots(In,Out):- 
   dots(In,0,Out).

dots([32,46,32|In],N,Out):- !,
   M is N + 1,
   dots(In,M,Out).

dots([46,32|In],N,Out):- !,
   M is N + 1,
   dots(In,M,Out).

dots([46|In],N,Out):- !,
   M is N + 1,
   dots(In,M,Out).

dots(Out,N,Out):- 
   N > 1.


/* =======================================================================
   Open Input File
========================================================================*/

openInput(Stream):-
   option('--stdin',dont),
   option('--input',File),
   exists_file(File), !,
   open(File,read,Stream,[encoding(utf8)]).

openInput(Stream):-
   option('--stdin',do), 
   set_prolog_flag(encoding,utf8),
   warning('reading from standard input',[]),
   prompt(_,''),
   Stream = user_input.


/* =======================================================================
   Open Output File
========================================================================*/

openOutput(Stream):-
   option('--output',Output),
   atomic(Output),
   \+ Output=user_output,
   ( access_file(Output,write), !,
     open(Output,write,Stream,[encoding(utf8)])
   ; error('cannot write to specified file ~p',[Output]),
     Stream=user_output ), !.

openOutput(user_output).


/* =======================================================================
   Version
========================================================================*/

version:-
   option('--version',do), !,
   version(V),
   format(user_error,'~p~n',[V]).

version.


/* =======================================================================
   Help
========================================================================*/

help:-
   option('--help',do), !,
   format(user_error,'usage: tokkie [options]~n~n',[]),
   showOptions(tokkie).

help:-
   option('--help',dont), !.


/* =======================================================================
   Definition of start
========================================================================*/

start:-
   current_prolog_flag(argv,[_Comm|Args]),
   setDefaultOptions(tokkie), 
   parseOptions(tokkie,Args),
   tokkie, !,
   halt.

start:- 
   error('tokkie failed',[]), 
   halt.


