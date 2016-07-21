
% the new tokkie.pl, by Johan Bos

/* ========================================================================
   File Search Paths
======================================================================== */

file_search_path(semlib, 'src/prolog/lib').
file_search_path(boxer,  'src/prolog/boxer').


/* ========================================================================
   Dynamic Predicates
======================================================================== */

:- dynamic split/7, title/1.


/* ========================================================================
   Load other libraries
======================================================================== */

:- use_module(library(lists),[member/2,append/3,reverse/2]).
:- use_module(library(readutil),[read_stream_to_codes/2]).
:- use_module(semlib(abbreviations),[iAbb/2,tAbb/2]).
:- use_module(semlib(errors),[error/2,warning/2]).
:- use_module(semlib(options),[option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).


/* ========================================================================
   Main
======================================================================== */

tokkie:-
   option(Option,do), 
   member(Option,['--help']), !, 
   help.

tokkie:-
   openInput(InStream),
   openOutput(OutStream), !,
   read_stream_to_codes(InStream,Codes),
   close(InStream),
   initTokkie,
   readLines(Codes,0,1,OutStream,Tokens),
   outputIOB(Codes,Tokens,OutStream),
   close(OutStream).

tokkie:-
   setOption(tokkie,'--help',do), !,
   help.


/* ----------------------------------------------------------------------
   Read lines
---------------------------------------------------------------------- */

readLines(Codes1,I1,S1,Stream,[Tokens|L]):-
   begSent(Codes1,I1,Codes2,I2), !,       % determine begin of a new sentence
   endSent(Codes2,I2,Codes3,I3,Rest,[]),  % determine end of this sentence  
%  format(Stream,'sen(~p,~p,~s).~n',[I2,I3,Codes3]),
%  write(Codes3),nl,
   tokenise(Codes3,I2,I2,T-T,Tokens),     % split sentence into tokens
   outputTokens(Tokens,S1,Stream),
   S2 is S1 + 1,                          % increase sentence counter
   readLines(Rest,I3,S2,Stream,L).        % process remaining of document

readLines(_,_,_,_,[]).


/* ----------------------------------------------------------------------
   Determine beginning of sentence
---------------------------------------------------------------------- */

begSent([Sep|C1],I1,C2,I3):- 
   sep(Sep), !,               % skip space, tab or newline
   I2 is I1 + 1,
   begSent(C1,I2,C2,I3).

begSent([C|L],I,[C|L],I).


/* ----------------------------------------------------------------------
   Determine end of sentence

   endSent(+CodesI,             % Input string
           +CurrentPosition,    % Current character position
           +CodesO,             % Output string (until sentence boundary)
           +BoundaryPosition,   % Character position of boundary
           +CodesR,             % Rest string
           +CodesLast)          % Last token

---------------------------------------------------------------------- */

endSent([],I,[],I,[],_):- !.

% Case 1: A full stop after a space 
%         --> sentence boundary.
endSent([46|Rest],I1,[46],I2,Rest,[]):- !, 
   I2 is I1 + 1.

% Case 2: full stop before a quote followed by a space
%         --> sentence boundary
endSent([46,Q1,Q2,X|Rest],I1,[46,Q1,Q2],I2,[X|Rest],_):- 
   \+ alphanum(X), quote(Q1), quote(Q2), !, I2 is I1 + 3.

endSent([46,Q,X|Rest],I1,[46,Q],I2,[X|Rest],_):- 
   \+ alphanum(X), quote(Q), !, I2 is I1 + 2.

% Case 3: full stop, but no sentence boundary
% 
endSent([C|C1],I1,[C|C2],I3,Rest,Last):- 
   noSentenceBoundary([C],C1,Last), !,
   I2 is I1 + 1,
   endSent(C1,I2,C2,I3,Rest,[C|Last]).

% Case 4: A full stop/question/exclemation mark after a non-abbreviation 
%         --> sentence boundary
endSent([End|Rest],I1,[End],I2,Rest,_):- 
   member(End,[46,63,33]), !, 
   I2 is I1 + 1.

endSent([46|Rest],I1,[46],I2,Rest,_):- !, 
   I2 is I1 + 1.

endSent([C|C1],I1,[C|C2],I3,Rest,Last):-
   alphanum(C), !,
   I2 is I1 + 1,
   endSent(C1,I2,C2,I3,Rest,[C|Last]).

endSent([C|C1],I1,[C|C2],I3,Rest,_):-
   I2 is I1 + 1,
   endSent(C1,I2,C2,I3,Rest,[]).


/* ----------------------------------------------------------------------
   Cases describing NO sentence boundaries

   noSentenceBoundary(Char,     % Character that could signal boundary
                      Next,     % Codes following
                      Last)     % Last token

---------------------------------------------------------------------- */
% Case 1: full stop after uppercase one-character token (i.e. initial)
noSentenceBoundary(".",_,Last):- Last = [Upper], upper(Upper).
% Case 2: full stop after a title 
noSentenceBoundary(".",_,Last):- title(Last).
% Case 2: full stop after an abbrev 
noSentenceBoundary(".",_,Last):- member(46,Last).
% Case 3: full stop before number
noSentenceBoundary(".",[N|_],_):- num(N).


/* ----------------------------------------------------------------------
   Split Line into Tokens
---------------------------------------------------------------------- */

% Nothing left to do, no tokens in queue
%
tokenise([],_,_,Sofar-[],[]):- Sofar=[], !.

% Nothing left to do, still a token present (input empty): store last token 
%
tokenise([],CurrentPos,StartPos,Sofar-[],[tok(StartPos,CurrentPos,Sofar)]):- !.

% Separator follows separator
%
tokenise([Sep|Codes],CurrentPos,_,T1-T2,Tokens):-
   sep(Sep), T2=[], T1=[], !,
   Pos is CurrentPos + 1, 
   tokenise(Codes,Pos,Pos,T-T,Tokens).

% Separator follows token
%
tokenise([Sep|Codes],CurrentPos,StartPos,Sofar-Tail,[Token|Tokens]):-
   sep(Sep), !, Tail = [],
   Token = tok(StartPos,CurrentPos,Sofar), 
   Pos is CurrentPos + 1, 
   tokenise(Codes,Pos,Pos,T-T,Tokens).

% Last character is a split, nothing in the queue: store last character
%
tokenise(Input,CurrentPos,_,Sofar-[],[Token|Tokens]):- 
   final(Input,Head,Rest,Len), Sofar = [], !,
   FinalPos is CurrentPos + Len,
   Token = tok(CurrentPos,FinalPos,Head),
   tokenise(Rest,FinalPos,FinalPos,T-T,Tokens).

% Last character is a split, store item in the queue and last character
%
tokenise(Input,CurrentPos,StartPos,Sofar-[],[Token1,Token2|Tokens]):- 
   final(Input,Head,Rest,Len), !,
   FinalPos is CurrentPos + Len,
   Token1 = tok(StartPos,CurrentPos,Sofar),
   Token2 = tok(CurrentPos,FinalPos,Head),
   tokenise(Rest,FinalPos,FinalPos,T-T,Tokens).

% Do not perform a split
%
tokenise(Input,CurrentPos,StartPos,OldSofar,Tokens):-
   dontsplit(Input,Rest,Diff,OldSofar,NewSofar), !,
   Pos is CurrentPos + Diff, 
   tokenise(Rest,Pos,StartPos,NewSofar,Tokens).


% Perform a token split operation
%
tokenise(Input,CurrentPos,StartPos,Sofar-Tail,[Token|Tokens]):-
   trysplit(Input,Left,Right,Rest,LenLeft,LenRight), !,
%  format('Input: ~s~n',[Input]),
%  format('Left: ~s~n',[Left]),
%  format('Right: ~s~n',[Right]),
%  format('Rest: ~s~n',[Rest]),
   Pos is CurrentPos + LenLeft,
   NewPos is Pos + LenRight,
   Tail = Left,
   Token = tok(StartPos,Pos,Sofar),    
   append(Right,NewTail,New),
   tokenise(Rest,NewPos,Pos,New-NewTail,Tokens).

% Do nothing but collect new token
%
tokenise([X|Codes],CurrentPos,StartPos,Sofar-Tail,Tokens):-
   Pos is CurrentPos + 1, 
   Tail = [X|NewTail],
   tokenise(Codes,Pos,StartPos,Sofar-NewTail,Tokens).


/* ----------------------------------------------------------------------
   Output Tokens
---------------------------------------------------------------------- */

outputTokens(Tokens,S,Stream):-
   option('--mode',poor), !,
   printTokens(Tokens,S,1,Stream).

outputTokens(Tokens,S,Stream):-
   option('--mode',rich), !,
   printTokens(Tokens,S,1,Stream).

outputTokens(_,_,_).


/* ----------------------------------------------------------------------
   Wrapper IOB format
---------------------------------------------------------------------- */

outputIOB(Codes,Tokens,Stream):-
   option('--mode',iob), !,
   printIOB(Codes,0,Tokens,Stream).

outputIOB(_,_,_).


/* ----------------------------------------------------------------------
   Output IOB format
---------------------------------------------------------------------- */

printIOB([],_,_,_).

printIOB([X|L],N1,TokenSet,Stream):-
   member([tok(N1,_,Tok)|_],TokenSet), !, Tag = 'S',
   tupleIOB(N1,X,Tag,Tok,Stream),
   N2 is N1 + 1,
   printIOB(L,N2,TokenSet,Stream).

printIOB([X|L],N1,TokenSet,Stream):-
   member(Tokens,TokenSet),
   member(tok(N1,_,Tok),Tokens), !, Tag = 'T',
   tupleIOB(N1,X,Tag,Tok,Stream),
   N2 is N1 + 1,
   printIOB(L,N2,TokenSet,Stream).

printIOB([X|L],N1,TokenSet,Stream):-
   member(Tokens,TokenSet),
   member(tok(Start,End,_),Tokens), N1 > Start, N1 < End, !, Tag = 'I',
   tupleIOB(N1,X,Tag,[],Stream),
   N2 is N1 + 1,
   printIOB(L,N2,TokenSet,Stream).

printIOB([X|L],N1,TokenSet,Stream):-
   Tag = 'O',
   tupleIOB(N1,X,Tag,[],Stream),
   N2 is N1 + 1,
   printIOB(L,N2,TokenSet,Stream).


/* ----------------------------------------------------------------------
   Tuple IOB format
---------------------------------------------------------------------- */

tupleIOB(_,X,Tag,_,Stream):- 
   option('--format',txt), !,
   format(Stream,'~p ~p~n',[X,Tag]).

tupleIOB(N,X,Tag,Tok,Stream):- 
   option('--format',prolog), !,
   format(Stream,'tok(~p,\'~p\'). % ~p ~s~n',[X,Tag,N,Tok]).


/* ----------------------------------------------------------------------
   Print Tokens
---------------------------------------------------------------------- */

printTokens([],_,_,_). 

printTokens([tok(_,_,Tok)],_,_,Stream):- 
   option('--mode',poor), !,
   format(Stream,'~s~n',[Tok]). 

printTokens([tok(I,J,Tok)|L],S,T1,Stream):- 
   option('--format',prolog),
   option('--mode',rich), !,
   Index is S*1000+T1,
   format(Stream,'tok(~p, ~p, ~p, ~s).~n',[I,J,Index,Tok]), 
   T2 is T1+1,
   printTokens(L,S,T2,Stream).

printTokens([tok(I,J,Tok)|L],S,T1,Stream):- 
   option('--format',txt),
   option('--mode',rich), !,
   Index is S*1000+T1,
   format(Stream,'~p ~p ~p ~s~n',[I,J,Index,Tok]), 
   T2 is T1+1,
   printTokens(L,S,T2,Stream).

printTokens([tok(_,_,Tok)|L],S,T,Stream):- 
   option('--mode',poor), !,
   format(Stream,'~s ',[Tok]), 
   printTokens(L,S,T,Stream).


/* ----------------------------------------------------------------------
   Type checking
---------------------------------------------------------------------- */

sep(10).    % new line
sep(13).    % new line
sep(32).    % space
sep(9).     % tab
sep(160).   % nbsp (non-breaking space)
sep(8194).  % en space
sep(8195).  % em space

alphanum(X):- alpha(X), !.
alphanum(X):- num(X), !.

alpha(62):- !.                         %%% '>' (end of markup)
alpha(X):- upper(X), !.
alpha(X):- lower(X), !.

upper(X):- number(X), X > 64, X < 91, !.
upper(X):- var(X), member(X,"ABCDEFGHIJKLMNOPQRSTUVWXYZ").

lower(X):- number(X), X > 96, X < 123, !.
lower(X):- var(X), member(X,"abcdefghijklmnopqrstuvwxyz").

num(X):- number(X), X > 47, X < 58, !.
num(X):- var(X), member(X,"0123456789").


/* ----------------------------------------------------------------------
   Rules for splitting tokens
   split(+Left,+ConditionsOnLeft,+Right,+ConditionsOnRight,+Context)
---------------------------------------------------------------------- */

split("can",[], "not",[], []).
split([_],[], "n't",[], []).
split([_],[], "'ll",[], []).
split([_],[], "'ve",[], []).
split([_],[], "'re",[], []).

split([_],[], "'m",[], []).
split([_],[], "'d",[], []).
split([_],[], "'s",[], []).

split([N],[num(N)],   [], [], "%").
split("%",[],         ",",[],[]).
split(")",[],         ",",[],[]).

split([N],[num(N)],   ",",[], [32]).
split([N],[num(N)],   ",",[], [10]).
split([A],[alpha(A)], [], [], ",").
split([_],[],         ";",[], []).
split([_],[],         ":",[], []).
split([_],[],         [],[], ")").
%split([_],[],         ")",[], []).
split([_],[],         "]",[], []).

split("$",[],   [N],[num(N)], []).     % dollar
split([163],[], [N],[num(N)], []).     % pound
split([165],[], [N],[num(N)], []).     % yen
split("(",[],   [X],[alphanum(X)], []).
split("[",[],   [X],[alphanum(X)], []).

split([_],[],         [Q],[quote(Q)], []).
split([Q],[quote(Q)], [X],[alphanum(X)], []).


/* ----------------------------------------------------------------------
   Exceptions (do not split)
---------------------------------------------------------------------- */

dontsplit(Input,Rest,N,Old-OldTail,Old-NewTail):- 
   nosplit(Left,N),
   append(Left,Rest,Input), !,
   append(Left,NewTail,OldTail).

nosplit("hi'it",5).
nosplit("e.g.",4).
nosplit([79,Q,U],3):- rsq(Q), upper(U).   % Irish names


/* ----------------------------------------------------------------------
   Initialisation
---------------------------------------------------------------------- */

initTokkie:-  
   initTitles,
   initSplitRules.

initTitles:-
   option('--language',Language), !,
   findall(Title,
           ( tAbb(Language,Title),
             reverse(Title,Reversed),
             assertz(title(Reversed)) ),
           _).
          
initSplitRules:-
   findall(Ri,
          ( split(Le,CondLe,Ri,CondRi,Context),
            length(Le,LenLe),
            length(Ri,LenRi),
            assertz(split(Le,LenLe,CondLe,Ri,LenRi,CondRi,Context)) ),
          _).


/* ----------------------------------------------------------------------
   Rules for final tokens
---------------------------------------------------------------------- */

final("?", "?", [], 1).
final(".", ".", [], 1).

final([46,Q],[46], [Q],1):- quote(Q).


/* ----------------------------------------------------------------------
   Try a splitting rule on the input
---------------------------------------------------------------------- */

trysplit(Input,Left,Right,Rest,LenLeft,LenRight):-
   split(Left,LenLeft,CondsLeft,Right,LenRight,CondsRight,RightContext),
   append(Left,Middle,Input), 
   checkConds(CondsLeft),  
   append(Right,Rest,Middle), 
   checkConds(CondsRight),   
   append(RightContext,_,Rest), !.


/* ----------------------------------------------------------------------
   Check Conditions
---------------------------------------------------------------------- */

checkConds([]).
checkConds([C|L]):- call(C), !, checkConds(L).


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


