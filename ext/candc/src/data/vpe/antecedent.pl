
/* --------------------------------------------------------------
   WSJ Section, paths of PTB and annotation
-------------------------------------------------------------- */

wsj('14').
ptb('working/PTB2.0/raw/wsj').
annotation('src/data/vpe/wsj').


/* --------------------------------------------------------------
   Dynamic Predicates
-------------------------------------------------------------- */

:- dynamic vpe/13, line/3.


/* --------------------------------------------------------------
   Main Predicate
-------------------------------------------------------------- */

run:-
   wsj(Section),
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
   append(LinVPAC,[32|Temp6],Temp5), number_codes(TLinVPA,LinVPAC),
   append(BegVPAC,[32|Temp7],Temp6), number_codes(TBegVPA,BegVPAC),
   append(EndVPAC,[32|Temp8],Temp7), number_codes(TEndVPA,EndVPAC),
   append(AuxVPAC,[32|Temp9],Temp8), atom_codes(AuxVPA,AuxVPAC),
   append(TypVPAC,[32|Temp10],Temp9), atom_codes(TypVPA,TypVPAC),   
   append(ConVPAC,[32|Temp11],Temp10), atom_codes(ConVPA,ConVPAC),
   append(RelVPAC,[32|ContextC],Temp11), !, atom_codes(RelVPA,RelVPAC),
   atom_codes(Context,ContextC),
   id(Id),
   ( TLinVPA=0, !, LinVPA=LinVPE; LinVPA=TLinVPA ),
   ( TBegVPA=0, !, BegVPA=1; BegVPA=TBegVPA ),
   ( TEndVPA=0, !, EndVPA=2; EndVPA=TEndVPA ),
   assert(vpe(Id,File,LinVPE,BegVPE,EndVPE,LinVPA,BegVPA,EndVPA,AuxVPA,TypVPA,ConVPA,RelVPA,Context)).

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
   assert(line(File,N,Codes)),
   lees(Stream,File,N).


/* --------------------------------------------------------------
   Present candidate to user
-------------------------------------------------------------- */

ask:-
   vpe(1,_,_,_,_,_,_,_,_,_,_,_,_),
   ask(1).

ask(Id):-
   vpe(Id,F,VPL,_VP1,_VP2,AL,AP1,AP2,_Aux,_,_,_,S), !,
   lastid(Last),
   lines(VPL,F,Lines),
   nl,nl,write('======= '),write(Id),write('/'),write(Last),write(' ======= '), 
   format('~p~n',[S]),   
   displayLines(Lines,F,AL,AP1,AP2),
   ask(Id,Lines,Next), !,
   ask(Next).

ask(0):- !, nl, nl.

ask(Id):- lastid(Last), Last < Id, !, ask(Last).

ask(_).


/* --------------------------------------------------------------
   Get Lines (VPE Context in which antecedent is expected)
-------------------------------------------------------------- */

lines(L1,F,Rev):-
   L1 > 1,
   linelen(F,L1,Len,Spaces),
   L2 is L1-1,
   secondLine(L2,F,L), !,
   reverse([L1:Len:Spaces|L],Rev).

lines(L,F,[L:Len:Spaces]):-
   linelen(F,L,Len,Spaces).

secondLine(L,F,[L:Len:Spaces]):-
   linelen(F,L,Len,Spaces), 
   Len > 1, !.

secondLine(L1,F,L):-
   L1 > 1,
   L2 is L1-1,
   secondLine(L2,F,L), !.
   
secondLine(_,_,[]).


/* --------------------------------------------------------------
   Length of a line
-------------------------------------------------------------- */

linelen(F,L,Len,Spaces):-
   line(F,L,Codes), !,
   spaces(Codes,1,Spaces),
   length(Codes,Len).


/* --------------------------------------------------------------
   Find spaces
-------------------------------------------------------------- */

spaces([],_,[]).

spaces([32|L1],N,[N|L2]):-
   M is N + 1,
   spaces(L1,M,L2).

spaces([_|L1],N,L2):-
   M is N + 1,
   spaces(L1,M,L2).


/* --------------------------------------------------------------
   Display lines
-------------------------------------------------------------- */

displayLines([],_,_,_,_).

displayLines([X:_:_|L],F,X,Begin,End):- !,
   line(F,X,Codes),
   atom_codes(Atom,Codes),
   displayAntecedent(Atom,Begin,End,New),
   write(New),nl,
   displayLines(L,F,X,Begin,End).

displayLines([X:_:_|L],F,Y,Begin,End):- !,
   line(F,X,Codes),
   atom_codes(Atom,Codes),
   write(Atom),nl,
   displayLines(L,F,Y,Begin,End).


/* --------------------------------------------------------------
   Display antecedent
-------------------------------------------------------------- */

displayAntecedent(Atom,Begin,End,New):-
   Temp1 is Begin-1,
   Temp2 is (End-Begin)+1,
   Temp3 is End,
   sub_atom(Atom,0,Temp1,_,Part1),
   sub_atom(Atom,Temp1,Temp2,_,Part2),
   sub_atom(Atom,Temp3,_,0,Part3), !,
%   On = '\033[40m\033[37m',
   On = '\033[32m',
   Off = '\033[0m',
   concat_atom([Part1,On,Part2,Off,Part3],New).


/* --------------------------------------------------------------
   Ask user
-------------------------------------------------------------- */

ask(I1,Lines,I2):-
   write('VPE (B/b)egin, (E/e)nd, (L/l)ine (n)ext, (p)revious, (d)isplay, (q)uit: '),
   get_single_char(Char),
   output(Char,Lines,I1,I2).


/* --------------------------------------------------------------
   Decrease pointer
-------------------------------------------------------------- */

dec(N1,N2,Min,Spaces):- 
   N2 is N1 - 1,  
   N2 >= Min,
   \+ member(N2,Spaces), !.

dec(N1,N2,Min,Spaces):- 
   N is N1 - 1,  
   N >= Min,
   member(N,Spaces), !,
   dec(N,N2,Min,Spaces).


/* --------------------------------------------------------------
   Increase pointer
-------------------------------------------------------------- */

inc(N1,N2,Max,Spaces):- 
   N2 is N1 + 1,  
   N2 =< Max,
   \+ member(N2,Spaces), !.

inc(N1,N2,Max,Spaces):- 
   N is N1 + 1,  
   N =< Max,
   member(N,Spaces), !,
   inc(N,N2,Max,Spaces).


/* --------------------------------------------------------------
   Process user input
-------------------------------------------------------------- */

% B 66
output(66,Lines,I,I):- 
   vpe(I,F,VL,VP1,VP2,L,B1,E,Aux,Con,Typ,Rel,S), 
   member(L:_:Spaces,Lines), 
   dec(B1,B2,1,Spaces), !,
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B2,E,Aux,Con,Typ,Rel,S)).

% b 98
output(98,Lines,I,I):- 
   vpe(I,F,VL,VP1,VP2,L,B1,E1,Aux,Typ,Con,Rel,S), !,
   member(L:Max:Spaces,Lines), 
   inc(B1,B2,Max,Spaces), !,
   ( B2 > E1, !, E2 = B2; E2 = E1 ),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B2,E2,Aux,Typ,Con,Rel,S)).

% E 69 
output(69,Lines,I,I):- 
   vpe(I,F,VL,VP1,VP2,L,B1,E1,Aux,Typ,Con,Rel,S),
   member(L:_:Spaces,Lines), 
   dec(E1,E2,1,Spaces), !,
   ( E2 < B1, !, B2 = E2; B2 = B1 ),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B2,E2,Aux,Typ,Con,Rel,S)).

% e 101
output(101,Lines,I,I):- 
   vpe(I,F,VL,VP1,VP2,L,B,E1,Aux,Typ,Con,Rel,S), 
   member(L:Max:Spaces,Lines), 
   inc(E1,E2,Max,Spaces), !,
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B,E2,Aux,Typ,Con,Rel,S)).

% L previous line
output(76,Lines,I,I):- 
   vpe(I,F,VL,VP1,VP2,L2,B1,E1,Aux,Typ,Con,Rel,S), 
   nextto(L1:Max:_, L2:_:_, Lines), !,
   ( E1 < Max, !, B2=B1, E2=E1; B2=1, E2=2 ),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L1,B2,E2,Aux,Typ,Con,Rel,S)).

output(76,_Lines,I,I):- !.
 
% l next line
output(108,Lines,I,I):- 
   vpe(I,F,VL,VP1,VP2,L1,B1,E1,Aux,Typ,Con,Rel,S), 
   nextto(L1:_:_,L2:Max:_,Lines), !,
   ( E1 < Max, !, B2=B1, E2=E1; B2=1, E2=2 ),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L2,B2,E2,Aux,Typ,Con,Rel,S)).

output(108,_Lines,I,I):- !.

% (d)isplay
output(100,_,I,I):- !,
   nl, nl, 
   results(user_output), 
   nl.

% (p)revious
output(112,_,I1,I2):-
   ( I1 > 1, !, I2 is I1 - 1; I2 = 1 ), !.

% (q)uit
output(113,_,_,0):- !.

% (n)ext
output(Char,_,I1,I2):-
   member(Char,[110,32,13]), !,
   I2 is I1 + 1.

output(_,_,I,I).


/* --------------------------------------------------------------
   Display results
-------------------------------------------------------------- */

results(Stream):-
   vpe(_,A,B,C,D,E,F,G,H,I,J,K,L),
   format(Stream,'~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p~n',[A,B,C,D,E,F,G,H,I,J,K,L]),
   fail.

results(_).



/* --------------------------------------------------------------
   VPE Candidate ID
-------------------------------------------------------------- */

id(N):-
   vpe(M1,_,_,_,_,_,_,_,_,_,_,_,_),
   \+ (vpe(M2,_,_,_,_,_,_,_,_,_,_,_,_), M2 > M1), !,
   N is M1 + 1.

id(1).


/* --------------------------------------------------------------
   Last Candidate ID
-------------------------------------------------------------- */

lastid(N):-
   vpe(N,_,_,_,_,_,_,_,_,_,_,_,_),
   \+ (vpe(M,_,_,_,_,_,_,_,_,_,_,_,_), M > N), !.


/* --------------------------------------------------------------
   Automatic start
-------------------------------------------------------------- */

:- run, halt.
