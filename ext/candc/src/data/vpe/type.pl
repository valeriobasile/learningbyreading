
/* --------------------------------------------------------------
   WSJ Section, paths of PTB and annotation
-------------------------------------------------------------- */

wsj('00').
ptb('working/PTB2.0/raw/wsj').
annotation('src/data/vpe/wsj').


/* --------------------------------------------------------------
   Typology: antecedent
-------------------------------------------------------------- */

at(vp,   1, 'VP -- verb phrase').
at(vpng, 2, 'VP-NG -- present participle').
at(vped, 3, 'VP-ED -- past participle').
at(np,   4, 'NP -- noun phrase').
at(ap,   5, 'AP -- adjectival phrase').
at(pp,   6, 'PP -- prepositional phrase').
at(tv,   7, 'TV -- transitive verb').
at(tvng, 8, 'TV-NG -- present participle').
at(tved, 9, 'TV-ED -- past participle').
at(xp,  10, 'all other cases').


/* --------------------------------------------------------------
   Typology: connector
-------------------------------------------------------------- */

ct(asa, 1, '[$_{src}$ NP VP] (,) as [$_{tgt}$ AUX NP]').
ct(soa, 2, '[$_{src}$ NP VP] ,$\mid$. (and$\mid$but) so [$_{tgt}$ AUX NP]').
ct(asi, 3, '[$_{src}$ NP VP] (,) (just$\mid$much) as$\mid$like [$_{tgt}$ NP AUX]').
ct(too, 4, '[$_{src}$ NP VP] (.$\mid$,) (and) [$_{tgt}$ NP AUX] (,) too').
ct(onc, 5, '[$_{src}$ NP VP] .$\mid$,$\mid$; (and$\mid$but) once$\mid$before$\mid$after$\mid$when$\mid$until [$_{tgt}$ NP AUX]').
ct(cat, 6, '(but) (just) as [$_{tgt}$ NP AUX] , [$_{src}$ NP VP]').  
ct(way, 7, '[$_{src}$ NP VP] (,) (in) the (same$\mid$exact) way [$_{tgt}$ NP AUX]').
ct(and, 8, '[$_{src}$ NP VP] (.$\mid$,$\mid$--) and [$_{tgt}$ NP AUX]').
ct(dis, 9, '[$_{src}$ NP VP] (.$\mid$,) or [$_{tgt}$ NP AUX]').
ct(nor,10, '[$_{src}$ NP VP] (.$\mid$,) (and) nor$\mid$neither [$_{tgt}$ AUX NP').
ct(but,11, '[$_{src}$ NP VP] (.$\mid$,) but [$_{tgt}$ NP AUX]').
ct(bec,12, '[$_{src}$ NP VP] (,) because$\mid$so [$_{tgt}$ NP AUX]').   % so-either
ct(now,13, '[$_{src}$ NP VP] (.$\mid$,) now$\mid$today [$_{tgt}$ NP AUX]').
ct(whi,14, '[$_{src}$ NP VP] (,) while [$_{tgt}$ NP AUX]').
ct(whe,15, '[$_{src}$ NP VP] before$\mid$until$\mid$when$\mid$whenever [$_{tgt}$ NP AUX]').
ct(whn,16, 'while$\mid$although$\mid$though [$_{src}$ NP VP] , [$_{tgt}$ NP AUX]').
ct(con,17, '[$_{src}$ NP VP] (.$\mid$,$\mid$and$\mid$or) (even) if [$_{tgt}$ NP AUX]').
ct(the,18, 'if [$_{src}$ NP VP] (,) [$_{tgt}$ NP AUX]').                % also includes one 'when'
ct(rel,19, '[$_{src}$ NP TV NP (that) [$_{tgt}$ NP AUX]]').
ct(wha,20, '[$_{src}$ NP TV what [$_{tgt}$ NP AUX]]').
ct(com,21, '[$_{src}$ NP VP X-er than [$_{tgt}$ NP AUX]]').
ct(coi,22, '[$_{src}$ NP VP X-er than [$_{tgt}$ AUX NP]]').
ct(equ,23, '[$_{src}$ NP VP as AP as [$_{tgt}$ NP AUX]]').
ct(eqi,24, '[$_{src}$ NP VP as AP as [$_{tgt}$ AUX NP]]').
ct(sem,25, '[$_{src}$ NP VP] ; [$_{tgt}$ NP AUX]').                              
ct(das,26, '[$_{src}$ NP VP] -- (and) [$_{tgt}$ NP AUX]').
ct(not,27, '[$_{src}$ NP VP] .$\mid$;$\mid$.$\mid$-- (and) [$_{tgt}$ NP AUX not]').             
ct(mod,28, '[$_{src}$ NP VP] . MOD (,) [$_{tgt}$ NP AUX]').             
ct(ful,29, '[$_{src}$ NP VP] . [$_{tgt}$ NP AUX]').                                 %%% also: MODAL, NEG
ct(qum,30, '[$_{src}$ NP VP] ? [$_{tgt}$ NP AUX]').                                
ct(cma,31, '[$_{src}$ NP VP] , [$_{tgt}$ NP AUX]').                                 %%% also: which, MODAL
ct(scl,32, 'NP who$\mid$that [$_{src}$ VP] TV NP who$\mid$that [$_{tgt}$ AUX]').    %%% one examples not realy fits here
ct(rso,33, 'NP who$\mid$that [$_{src}$ VP] (often$\mid$generally$\mid$usually) [$_{tgt}$ AUX] so').
ct(tag,34, '[$_{src}$ NP VP] (,$\mid$--) (or) [$_{tgt}$ AUX (NEG) NP]?').
ct(oth,35, '\textit{all other cases}').


/* --------------------------------------------------------------
   Typology: discourse relation
-------------------------------------------------------------- */

dr(con,1,'contrast').
dr(par,2,'parallellism').
dr(vex,3,'violated_expectation').
dr(exp,4,'explanation').
dr(res,5,'result').
dr(cau,6,'cause').
dr(nar,7,'narration').
dr(elb,8,'elaboration').
dr(ins,9,'instance').
dr(qar,10,'question-answer').
dr(oth,11,'other').


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
   readAnnLines(Codes,Stream,[]-Ann),
   sort(Ann,Sorted),
   add2db(Sorted,1).
   
readAnnotation(_).


/* --------------------------------------------------------------
   Read annotation lines
-------------------------------------------------------------- */

readAnnLines(end_of_file,Stream,Ann-Ann):- !,
   close(Stream).

readAnnLines(Codes,Stream,Ann1-Ann3):- !,
   processAnn(Codes,Ann1-Ann2), 
   read_line_to_codes(Stream,NewCodes),
   readAnnLines(NewCodes,Stream,Ann2-Ann3).


/* --------------------------------------------------------------
   Add annotation to Prolog database
-------------------------------------------------------------- */

add2db([],_).

add2db([vpe(A,B,C,D,E,F,G,H,I,J,K,L)|Rest],Id):-
    assert(vpe(Id,A,B,C,D,E,F,G,H,I,J,K,L)),
    NewId is Id + 1,
    add2db(Rest,NewId).


/* --------------------------------------------------------------
   Process existing annotation
-------------------------------------------------------------- */

processAnn(Codes,Ann-[New|Ann]):-
   squeezeSpace(Codes,Temp1),
   append(FileC,  [32|Temp2], Temp1), atom_codes(File,FileC),
   append(LinVPEC,[32|Temp3], Temp2), number_codes(LinVPE,LinVPEC),
   append(BegVPEC,[32|Temp4], Temp3), number_codes(BegVPE,BegVPEC),
   append(EndVPEC,[32|Temp5], Temp4), number_codes(EndVPE,EndVPEC),
   append(LinVPAC,[32|Temp6], Temp5), number_codes(TLinVPA,LinVPAC),
   append(BegVPAC,[32|Temp7], Temp6), number_codes(TBegVPA,BegVPAC),
   append(EndVPAC,[32|Temp8], Temp7), number_codes(TEndVPA,EndVPAC),
   append(AuxC,   [32|Temp9], Temp8), atom_codes(Aux,AuxC),
   append(ATC,   [32|Temp10], Temp9), atom_codes(AT,ATC),
   append(CTC,   [32|Temp11],Temp10), atom_codes(CT,CTC),
   append(DRC,   [32|Temp12],Temp11), atom_codes(DR,DRC), !,
   atom_codes(Context,Temp12),
   ( at(AT,_,_), !, AT1 = AT; AT1 = xp ), 
   ( ct(CT,_,_), !, CT1 = CT; CT1 = oth ), 
   ( dr(DR,_,_), !, DR1 = DR; DR1 = oth ), 
   ( TLinVPA=0, !, LinVPA=LinVPE; LinVPA=TLinVPA ),
   ( TBegVPA=0, !, BegVPA=1; BegVPA=TBegVPA ),
   ( TEndVPA=0, !, EndVPA=2; EndVPA=TEndVPA ),
   New = vpe(File,LinVPE,BegVPE,EndVPE,LinVPA,BegVPA,EndVPA,Aux,AT1,CT1,DR1,Context).

processAnn(_,Ann-Ann).


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
   vpe(Id,F,VPL,VP1,VP2,AL,AP1,AP2,_Aux,AT,CT,DR,_S), !,
   lastid(Last),
   lines(VPL,F,Lines),
   nl, nl, write('======= '),write(Id),write('/'),write(Last),write(' ======= '), nl,
   at(AT,_,ATyp), ct(CT,_,CTyp), dr(DR,_,DRel),
   format('[Antecedent: ~p]~n[Connector: ~p]~n[Relation: ~p]~n',[ATyp,CTyp,DRel]),   
   write('============== '), nl,
   Inserts = [i(AL,AP1),j(AL,AP2),i(VPL,VP1),j(VPL,VP2)],
   displayLines(Lines,F,Inserts),
   ask(Id,Next), !,
   ask(Next).

ask(0):- !, nl, nl.

ask(Id):- lastid(Last), Last < Id, !, ask(Last).

ask(_).


/* --------------------------------------------------------------
   Get Lines (VPE Context in which antecedent is expected)
-------------------------------------------------------------- */

lines(L1,F,Rev):-
   L1 > 1,
   linelen(F,L1,_Len),
   L2 is L1-1,
   secondLine(L2,F,L), !,
   reverse([L1|L],Rev).

lines(L,F,[L]):-
   linelen(F,L,_).

secondLine(L,F,[L]):-
   linelen(F,L,Len), 
   Len > 1, !.

secondLine(L1,F,L):-
   L1 > 1,
   L2 is L1-1,
   secondLine(L2,F,L), !.
   
secondLine(_,_,[]).


/* --------------------------------------------------------------
   Length of a line
-------------------------------------------------------------- */

linelen(F,L,Len):-
   line(F,L,Codes), !,
   length(Codes,Len).


/* --------------------------------------------------------------
   Display lines
-------------------------------------------------------------- */

displayLines([],_,_).

displayLines([X|L],File,Inserts):-
    line(File,X,Codes),
    formatLine(Codes,X,1,Inserts,NewCodes),
    ( Codes = NewCodes, !
    ; atom_codes(Atom,NewCodes),
      write(Atom),nl
    ),
    displayLines(L,File,Inserts).


/* --------------------------------------------------------------
   Format a line
-------------------------------------------------------------- */

formatLine([],_,_,_,[]):- !.

formatLine(L,_,_,[],L):- !.

formatLine([X|L1],Line,Pos1,Inserts,[27,91,51,52,109,X|L2]):-
   select(i(Line,Pos1),Inserts,NewInserts), !,
   Pos2 is Pos1 + 1,
   formatLine(L1,Line,Pos2,NewInserts,L2).

formatLine([X|L1],Line,Pos1,Inserts,[X,27,91,48,109|L2]):-
   select(j(Line,Pos1),Inserts,NewInserts), !,
   Pos2 is Pos1 + 1,
   formatLine(L1,Line,Pos2,NewInserts,L2).

formatLine([X|L1],Line,Pos1,Inserts,[X|L2]):-
   Pos2 is Pos1 + 1,
   formatLine(L1,Line,Pos2,Inserts,L2).


/* --------------------------------------------------------------
   Ask user
-------------------------------------------------------------- */

ask(I1,I2):-
   write('VPE (a/A)ype, (c/C)onnector (r/R)elation, (n)ext, (p)revious, (d)isplay, (i)nfo, (q)uit: '),
   get_single_char(Char),
   output(Char,I1,I2).


/* --------------------------------------------------------------
   Next Type
-------------------------------------------------------------- */

nexttype(Old,New):- 
   at(Old,N,_), 
   M is N + 1, 
   at(New,M,_), !.

nexttype(_,New):- 
   at(New,1,_).

/* --------------------------------------------------------------
   Previous Type
-------------------------------------------------------------- */

prevtype(Old,New):- 
   at(Old,N,_), 
   M is N - 1, 
   at(New,M,_), !.

prevtype(_,New):- 
   at(New,N,_),
   \+ (at(_,M,_), M > N), !.


/* --------------------------------------------------------------
   Next Connector
-------------------------------------------------------------- */

nextconn(Old,New):- 
   ct(Old,N,_), 
   M is N + 1, 
   ct(New,M,_), !.

nextconn(_,New):- 
   ct(New,1,_).

/* --------------------------------------------------------------
   Previous Connector
-------------------------------------------------------------- */

prevconn(Old,New):- 
   ct(Old,N,_), 
   M is N - 1, 
   ct(New,M,_), !.

prevconn(_,New):- 
   ct(New,N,_),
   \+ (ct(_,M,_), M > N), !.


/* --------------------------------------------------------------
   Next Discourse Relation
-------------------------------------------------------------- */

nextdr(Old,New):- 
   dr(Old,N,_), 
   M is N + 1, 
   dr(New,M,_), !.

nextdr(_,New):- 
   dr(New,1,_).

/* --------------------------------------------------------------
   Previous Discourse Relation
-------------------------------------------------------------- */

prevdr(Old,New):- 
   dr(Old,N,_), 
   M is N - 1, 
   dr(New,M,_), !.

prevdr(_,New):- 
   dr(New,N,_),
   \+ (dr(_,M,_), M > N), !.


/* --------------------------------------------------------------
   Process user input
-------------------------------------------------------------- */

% c 99 
output(99,I,I):- !, 
   vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ,Con1,Rel,S), 
   nextconn(Con1,Con2),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ,Con2,Rel,S)).

% C 67
output(67,I,I):- !, 
   vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ,Con1,Rel,S), 
   prevconn(Con1,Con2),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ,Con2,Rel,S)).

% a 97
output(97,I,I):- !, 
   vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ1,Con,Rel,S), 
   nexttype(Typ1,Typ2),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ2,Con,Rel,S)).

% A 65
output(65,I,I):- !, 
   vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ1,Con,Rel,S), 
   prevtype(Typ1,Typ2),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ2,Con,Rel,S)).

% r 114
output(114,I,I):- !, 
   vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ,Con,Rel1,S), 
   nextdr(Rel1,Rel2),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ,Con,Rel2,S)).

% R 82
output(82,I,I):- !, 
   vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ,Con,Rel1,S), 
   prevdr(Rel1,Rel2),
   retract(vpe(I,_,_,_,_,_,_,_,_,_,_,_,_)),
   assert(vpe(I,F,VL,VP1,VP2,L,B,E,Aux,Typ,Con,Rel2,S)).

% (d)isplay
output(100,I,I):- !,
   nl, nl, 
   results(user_output), 
   nl.

% (i)nfo
output(105,I,I):- !,
   nl, nl,write('Antecedent:'),nl,
   findall(AT,(at(AT0,_,AT), write(AT0),write(' = '),write(AT),nl),_),
   nl, nl,write('Connector:'),nl,
   findall(CT,(ct(CT0,_,CT), write(CT0),write(' = '),write(CT),nl),_),
   nl, write('Relations:'),nl,
   findall(Rel,(dr(R,_,Rel), write(R),write(' = '),write(Rel),nl),_).

% (p)revious
output(112,I1,I2):-
   ( I1 > 1, !, I2 is I1 - 1; I2 = 1 ), !.

% (q)uit
output(113,_,0):- !.

% (n)ext
output(Char,I1,I2):-
   member(Char,[110,32,13]), !,
   I2 is I1 + 1.

output(_,I,I).


/* --------------------------------------------------------------
   Display results
-------------------------------------------------------------- */

results(Stream):- results(1,Stream).

results(Id,Stream):-
   vpe(Id,A,B,C,D,E,F,G,H,I,J,K,L), !,
   format(Stream,'~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p ~p~n',[A,B,C,D,E,F,G,H,I,J,K,L]),
   NewId is Id + 1,
   results(NewId,Stream).

results(_,_).


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
