
/* --------------------------------------------------------------
 WSJ Section, paths of PTB and annotation
-------------------------------------------------------------- */

wsj('20').
ptb('working/PTB2.0/raw/wsj').
annotation('src/data/vpe/wsj').
context_window(40).
check_file('working/check-annotation.txt').


/* --------------------------------------------------------------
   aux(+Pattern, +Aux, +Skip, +Length).
-------------------------------------------------------------- */

%aux('What\'s', be, 4, 2).
%aux('what\'s', be, 4, 2).
%aux('Who\'s', be, 4, 2).
%aux('who\'s', be, 4, 2).

%aux('I\'m', be, 1, 2).
%aux('He\'s', be, 2, 2).
%aux('he\'s', be, 2, 2).
%aux('It\'s', be, 2, 2).
%aux('it\'s', be, 2, 2).
%aux('She\'s', be, 3, 2).
%aux('she\'s', be, 3, 2).

%aux('We\'re', be, 2, 3).
%aux('we\'re', be, 2, 3).
%aux('You\'re', be, 3, 3).
%aux('you\'re', be, 3, 3).
%aux('They\'re', be, 4, 3).
%aux('they\'re', be, 4, 3).

%aux('I\'ve', have, 1, 3).
%aux('We\'ve', have, 2, 3).
%aux('we\'ve', have, 2, 3).
%aux('You\'ve', have, 3, 3).
%aux('you\'ve', have, 3, 3).
%aux('They\'ve', have, 4, 3).
%aux('they\'ve', have, 4, 3).

%aux('I\'ll', will, 1, 3).
%aux('We\'ll', will, 2, 3).
%aux('we\'ll', will, 2, 3).
%aux('He\'ll', will, 2, 3).
%aux('he\'ll', will, 2, 3).
%aux('She\'ll', will, 3, 3).
%aux('she\'ll', will, 3, 3).
%aux('You\'ll', will, 3, 3).
%aux('you\'ll', will, 3, 3).
%aux('They\'ll', will, 4, 3).
%aux('they\'ll', will, 4, 3).

%aux('I\'d', would, 1, 2).
%aux('We\'d', would, 2, 2).
%aux('we\'d', would, 2, 2).
%aux('He\'d', would, 2, 2).
%aux('he\'d', would, 2, 2).
%aux('She\'d', would, 3, 2).
%aux('she\'d', would, 3, 2).
%aux('You\'d', would, 3, 2).
%aux('you\'d', would, 3, 2).
%aux('They\'d', would, 4, 2).
%aux('they\'d', would, 4, 2).

%aux(do,    do, 0, 2).
%aux(don,   do, 0, 2).
%aux(does,  do, 0, 4).
%aux(doesn, do, 0, 4).
%aux(did,   do, 0, 3).
%aux(didn,  do, 0, 3).
%aux(done,  do, 0, 4).
%aux(doing, do, 0, 5).

%aux(have,  have, 0, 4).
%aux('haven\'t', have, 0, 4).
%aux(has,   have, 0, 3).
%aux('hasn\'t',  have, 0, 3).
%aux(had,   have, 0, 3).
%aux(hadn,  have, 0, 3).

%aux(may,   may,  0, 3). 
%aux(mayn,  may,  0, 3). 
%aux(can,   can,  0, 3). 
%aux(cannot,   can,  0, 3). 
%aux(might,   might,  0, 5). 
%aux(must,    must,   0, 4). 
%aux('will',    will,   0, 4). 
%aux('won\'t',     will,   0, 2). 
%aux(would,    would,  0, 5). 
%aux(wouldn,   would,  0, 5). 
%aux('could',   could,  0, 5). 
%aux(couldn,  could,  0, 5). 
%aux(should,  should, 0, 5). 
%aux('Should',  should, 0, 5). 
%aux(shouldn, should, 0, 5). 
%aux(shall, shall, 0, 5).
%aux(shan, shall, 0, 4).

%aux(needn,   need,  0, 4).
%aux(daren,   dare,  0, 4).
%aux(oughtn,  ought,  0, 5).

%aux('to.', to, 0, 2).
%aux('to!', to, 0, 2).
%aux('to?', to, 0, 2).
%aux('to,', to, 0, 2).
%aux('to"', to, 0, 2).
%aux('to\'', to, 0, 2).

%aux(am, be, 0, 2).
%aux('Am', be, 0, 2).
%aux(are, be, 0, 3).
%aux('Are', be, 0, 3).
%aux(aren, be, 0, 3).
%aux('Aren', be, 0, 3).
%aux(is, be, 0, 2).
%aux('Is', be, 0, 2).
%aux('Isn', be, 0, 2).
%aux(isn, be, 0, 2).
aux(was, be, 0, 3).
%aux('Was', be, 0, 3).
%aux(wasn, be, 0, 3).
%aux('Wasn', be, 0, 3).
%aux(were, be, 0, 4).
%aux('Were', be, 0, 4).
%aux(weren, be, 0, 4).
%aux('Weren', be, 0, 4).
%aux(be, be, 0, 2).
%aux(been, be, 0, 4).

%aux(seems, seem, 0, 5).

%aux('do the same', same, 0, 2).
%aux('does the same', same, 0, 4).
%aux('doing the same', same, 0, 5).
%aux('does the opposite', same, 0, 4).
%aux('do just the opposite', same, 0, 2).
%aux('be so', so, 0, 2).

/* --------------------------------------------------------------
   Dynamic Predicates
-------------------------------------------------------------- */

:- dynamic vpe/12, candidate/7, current_id/1.
current_id(0).


/* --------------------------------------------------------------
   Main Predicate
-------------------------------------------------------------- */

run:-
   wsj(Section), write('Section: '), write(Section), nl,
   annotation(Ann),
   concat_atom([Ann,'/',Section],AnnFile),
   readAnnotation(AnnFile),
   ptb(PTB),
   concat_atom([PTB,'/',Section,'/*'],WSJ),
   expand_file_name(WSJ,Files),
   run(Files),
   ask, 
   open(AnnFile,write,Out,[encoding(utf8)]),
   results(Out),
   close(Out),
   results(user_output).


/* --------------------------------------------------------------
   Open annotation file
-------------------------------------------------------------- */

readAnnotation(File):-
   exists_file(File), !,
   open(File,read,Stream,[encoding(utf8)]),
   read_line_to_codes(Stream,Codes),
   readAnnLines(Codes,Stream).
   
readAnnotation(_).


/* --------------------------------------------------------------
   Read annotation lines
-------------------------------------------------------------- */

readAnnLines(end_of_file,Stream):- !,
   close(Stream).

readAnnLines(Codes,Stream):- !,
   processAnn(Codes), 
   read_line_to_codes(Stream,NewCodes),
   readAnnLines(NewCodes,Stream).


/* --------------------------------------------------------------
   Process existing annotation
-------------------------------------------------------------- */

processAnn(Codes):-
   squeezeSpace(Codes,Temp1),
   append(FileC,[32|Temp2],Temp1),   atom_codes(File,FileC),
   append(LinVPEC,[32|Temp3],Temp2), number_codes(LinVPE,LinVPEC),
   append(BegVPEC,[32|Temp4],Temp3), number_codes(BegVPE,BegVPEC),
   append(EndVPEC,[32|Temp5],Temp4), number_codes(EndVPE,EndVPEC),
   append(LinVPAC,[32|Temp6],Temp5), number_codes(LinVPA,LinVPAC),
   append(BegVPAC,[32|Temp7],Temp6), number_codes(BegVPA,BegVPAC),
   append(EndVPAC,[32|Temp8],Temp7), number_codes(EndVPA,EndVPAC),
   append(AuxVPAC,[32|Temp9],Temp8), !, atom_codes(AuxVPA,AuxVPAC),
   append(TypVPAC,[32|Temp10],Temp9), !, atom_codes(TypVPA,TypVPAC),
   append(ConVPAC,[32|Temp11],Temp10), !, atom_codes(ConVPA,ConVPAC),
   append(RelVPAC,[32|ContextC],Temp11), !, atom_codes(RelVPA,RelVPAC),
   atom_codes(Context,ContextC),
   format('~p ~p ~p ~p ~p ~p ~p ~p~n',[File,LinVPE,BegVPE,EndVPE,LinVPA,BegVPA,EndVPA,AuxVPA]),
   assert(vpe(File,LinVPE,BegVPE,EndVPE,LinVPA,BegVPA,EndVPA,AuxVPA,TypVPA,ConVPA,RelVPA,Context)).

processAnn(_).


/* --------------------------------------------------------------
   Squeeze Spaces
-------------------------------------------------------------- */

squeezeSpace([],[]).

squeezeSpace([32,32|L1],L2):- !,
   squeezeSpace([32|L1],L2).

squeezeSpace([X|L1],[X|L2]):- !,
   squeezeSpace(L1,L2).


/* --------------------------------------------------------------
   Read Files
-------------------------------------------------------------- */

run([]).

run([F|Files]):-
   file_base_name(F,File),
   open(F,read,Stream,[encoding(utf8)]),
   lees(Stream,File,0),
   close(Stream),
   run(Files).


/* --------------------------------------------------------------
   Read File
-------------------------------------------------------------- */

lees(Stream,File,M):-
   N is M + 1,
   read_line_to_codes(Stream,Codes),
   processLine(Codes,Stream,File,N).


/* --------------------------------------------------------------
   Process lines
-------------------------------------------------------------- */

processLine(end_of_file,_,_,_):- !.

processLine(Codes,Stream,File,N):-
   findall(Aux,aux(Aux,_,_,_),Auxs),
   locate(Auxs,Codes,N,File),
   lees(Stream,File,N).


locate([],_,_,_).

locate([Aux|L],Codes,N,File):-
   atom_codes(Aux,AuxCodes),
   locate(Codes,AuxCodes,0,Codes,1,N,File),
   locate(L,Codes,N,File).


/* --------------------------------------------------------------
   Locate VPE candidate and store in database
-------------------------------------------------------------- */

locate([],_,_,_,_,_,_):- !.

locate(L2,L1,Prev,S,P1,N,F):-
   \+ alpha(Prev),
   L1 = L2, !,
   new_id(Id), write(Id), tab(1), ttyflush,
   atom_codes(Aux,L1),
   aux(Aux,Stem,OffSet,Len),
   P3 is P1+OffSet,
   P4 is ((P3+Len)-1),
   assert(candidate(Id,F,N,P3,P4,Stem,S)),
   L2=[Next|L3],
   P2 is P1 + 1,
   locate(L3,L1,Next,S,P2,N,F).

locate(L2,L1,Prev,S,P1,N,F):-
   \+ alpha(Prev),
   append(L1,[After|_],L2), 
   \+ alpha(After), !,
   new_id(Id), write(Id), tab(1), ttyflush,
   atom_codes(Aux,L1),
   aux(Aux,Stem,OffSet,Len),
   P3 is P1+OffSet,
   P4 is ((P3+Len)-1),
   assert(candidate(Id,F,N,P3,P4,Stem,S)),
   L2=[Next|L3],
   P2 is P1 + 1,
   locate(L3,L1,Next,S,P2,N,F).

locate([Next|L2],L1,_,S,P1,N,F):-
   P2 is P1 + 1,
   locate(L2,L1,Next,S,P2,N,F).


/* --------------------------------------------------------------
   Alphabetic character
-------------------------------------------------------------- */

alpha(X):- X > 96, X < 123, !.
alpha(X):- X > 64, X < 91, !.


/* --------------------------------------------------------------
   Present candidate to user (init)
-------------------------------------------------------------- */

ask:-
   candidate(1,_,_,_,_,_,_), !,
   ask(1).

ask:-
   write('No new VPE candidates found'), nl.


/* --------------------------------------------------------------
   Present candidate to user (loop)
-------------------------------------------------------------- */

ask(Id):-
   candidate(Id,F,N,P1,P2,Aux,S), !,
   Len is (P2-P1)+1,
   current_id(Last),
   currentAnnotation(Id,Current),
   nl,write('======= '),write(Id),write('/'),write(Last),write(' ======= '),
   write('VPE: '), write(Current),nl,
   get_context(S,P1,C,P5),
   context_window(CW),   
   Tab is ((CW+3)-P5),
   tab(CW), tab(3), printN(Len,'v'), nl,
   tab(Tab), format('~p~n',[C]),
   tab(CW), tab(3), printN(Len,'^'), nl,
   ask(F,N,P1,P2,Aux,C,S,Id,Next), !,
   ask(Next).

ask(0):- !, 
   nl, nl.

ask(Id):- 
   current_id(Last), 
   Last < Id, !, 
   ask(Last).

ask(_).


/* --------------------------------------------------------------
   Print N times
-------------------------------------------------------------- */

printN(0,_):- !.

printN(N,X):- 
   N > 0, M is N - 1, 
   write(X), 
   printN(M,X).


/* --------------------------------------------------------------
   Get context around VPE
-------------------------------------------------------------- */

get_context(S,P1,Context,P3):-
   context_window(CW),
   Temp is P1 - CW,
   ( Temp < 1, P2 is P1-1, !, Start = 1; Start = Temp, P2 = CW ),
   start_context(1,Start,S,C),
   atom_codes(Context1,C),
   concat_atom(['...',Context1,'...'],Context),
   P3 is P2+3.

start_context(N,N,S,Context):- !,
   end_context(S,Context).

start_context(N1,N2,[_|S],Context):-
   N3 is N1 + 1,
   start_context(N3,N2,S,Context).

end_context(S,Context):-
   length(Context,70),
   append(Context,_,S), !.

end_context(S,S).


/* --------------------------------------------------------------
   Ask user
-------------------------------------------------------------- */

ask(F,N,P1,P2,Aux,C,S,I1,I2):-
   write('VPE? (y)es, (u)ndo, (n)ext, (p)revious, (d)isplay, (c)heck, (q)uit: '),
   get_single_char(Char),
   output(Char,F,N,P1,P2,Aux,C,S,I1,I2).


/* --------------------------------------------------------------
   Process user input
-------------------------------------------------------------- */

output(Code,F,N,P1,P2,Aux,C,_,I1,I2):-
   member(Code,[89,121]), !,
   I2 is I1 + 1,
   write('yes'),nl,nl,
   ( vpe(F,N,P1,P2,_,_,_,Aux,_,_,_,_), !
   ; assert(vpe(F,N,P1,P2,N,0,0,Aux,vp,oth,oth,C)) ).

output(99,F,_,_,_,_,_,S,I,I):- !,
   write(check),nl,nl,
   format('~s~n~n',[S]),
   check_file(File),
   open(File,append,Stream,[encoding(utf8)]),
   ptb(PTB),
   wsj(Section),
   format(Stream,'---~nFile: ~p/~p/~p~n',[PTB,Section,F]),
   format(Stream,'~s~n',[S]), 
   close(Stream).

output(100,_,_,_,_,_,_,_,I,I):- !,
   nl, nl, 
   results(user_output), 
   nl, nl.

output(117,F,N,P1,P2,Aux,_,_,I1,I2):-
   I2 is I1 + 1,
   vpe(F,N,P1,P2,_,_,_,Aux,Typ,Con,Rel,_), !,
   retract(vpe(F,N,P1,P2,_,_,_,Aux,Typ,Con,Rel,_)),
   write('undo'),nl,nl.

output(112,_,_,_,_,_,_,_,I1,I2):-
   I1 > 1, !,
   nl,nl,
   I2 is I1 - 1.

output(112,_,_,_,_,_,_,_,I1,I2):-
   I1 = 1, !,
   nl,nl,
   I2 = I1.

output(113,_,_,_,_,_,_,_,_,0):- !.

output(_,_,_,_,_,_,_,_,I1,I2):-
   I2 is I1 + 1,
   write('no'),nl,nl.


/* --------------------------------------------------------------
   Display results
-------------------------------------------------------------- */

results(Stream):-
   vpe(A,B,C,D,E,F,G,H,I,J,K,L),
   format(Stream,'~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p~n',[A,B,C,D,E,F,G,H,I,J,K,L]),
   fail.

results(_).


/* --------------------------------------------------------------
   Annotation
-------------------------------------------------------------- */

currentAnnotation(Id,Result):-
   candidate(Id,File,Line,P1,P2,Aux,_),
   vpe(File,Line,P1,P2,_,_,_,Aux,_,_,_,_), !,
   Result = '\033[31mYES\033[0m'.

currentAnnotation(_,no).


/* --------------------------------------------------------------
   VPE Candidate ID
-------------------------------------------------------------- */

new_id(N):-
   current_id(Old), !,
   N is Old + 1,
   retract(current_id(Old)),
   assert(current_id(N)).

%   candidate(M1,_,_,_,_,_,_),
%   \+ (candidate(M2,_,_,_,_,_,_), M2 > M1), !,
%   N is M1 + 1.



/* --------------------------------------------------------------
   Last Candidate ID
-------------------------------------------------------------- */

lastid(N):-
   candidate(N,_,_,_,_,_,_),
   \+ (candidate(M,_,_,_,_,_,_), M > N), !.


/* --------------------------------------------------------------
   Automatic start
-------------------------------------------------------------- */

:- run, halt.
