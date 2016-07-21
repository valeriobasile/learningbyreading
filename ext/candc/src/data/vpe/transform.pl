
/* --------------------------------------------------------------
 WSJ Section, paths of PTB and annotation
-------------------------------------------------------------- */

ptb('working/PTB2.0/raw/wsj').
annotation('src/data/vpe/wsj').


/* --------------------------------------------------------------
   Dynamic Predicates
-------------------------------------------------------------- */

:- dynamic vpe/12, new_vpe/10, current_id/1.
current_id(0).


/* --------------------------------------------------------------
   Main Predicate
-------------------------------------------------------------- */

run:- run(['00','01','02','03','04','05','06','07','08','09',
           '10','11','12','13','14','15','16','17','18','19',
           '20','21','22','23','24']).

run([Section|Rest]):-
   retractall(new_vpe(_,_,_,_,_,_,_,_,_,_)),
   write('Section: '), write(Section), nl,
   annotation(Ann),                     %%% directory of annotation
   concat_atom([Ann,'/',Section],AnnFile),
   readAnnotation(AnnFile),
   trans(Section),
   concat_atom([Ann,'/',Section,'.ann'],NewAnnFile),
   open(NewAnnFile,write,Out,[encoding(utf8)]),
   results(Out),
   close(Out),
   results(user_output),
   run(Rest).

run([]).


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

trans(Section):- 
   vpe(_,_,_,_,_,_,_,_,_,_,_,_), !,
   retract(vpe(File,EL,EB,EE,AL,AB,AE,I1,I2,I3,I4,I5)),
   ptb(PTB),
   concat_atom([PTB,'/',Section,'/',File],WSJ),   
   open(WSJ,read,Stream1,[encoding(utf8)]),
   trans(Stream1,0,0,EL,EB,EE,EBegin,EEnd),
   close(Stream1),
   open(WSJ,read,Stream2,[encoding(utf8)]),
   trans(Stream2,0,0,AL,AB,AE,ABegin,AEnd),
   close(Stream2),
   assert(new_vpe(File,EBegin,EEnd,ABegin,AEnd,I1,I2,I3,I4,I5)),
   trans(Section).

trans(_).
   

trans(Stream,OldOff,M,OldLine,OldBeg,OldEnd,NewBeg,NewEnd):-
   NewLine is M + 1,
   read_line_to_codes(Stream,Codes),
   ( NewLine = OldLine, !,
     computenew(Codes,OldOff,1,OldBeg,TmpBeg), NewBeg is TmpBeg - 1,
     computenew(Codes,OldOff,1,OldEnd,NewEnd)
   ; \+ NewLine = OldLine,
     offset(Codes,OldOff,NewOff),
     trans(Stream,NewOff,NewLine,OldLine,OldBeg,OldEnd,NewBeg,NewEnd)
   ).


/* --------------------------------------------------------------
   New offset within a line
-------------------------------------------------------------- */

computenew(_,Offset,Pos,Pos,Temp):- !, Temp is Offset + 1.

computenew([_|L],Offset,Pos,Old,New):-
   Temp is Offset + 1,
   NewPos is Pos + 1,
   computenew(L,Temp,NewPos,Old,New).


/* --------------------------------------------------------------
   Offset within a line
-------------------------------------------------------------- */

offset(end_of_file,N,N).
offset([],Old,New):- New is Old + 1.
offset([_|L],Old,New):- Temp is Old + 1, offset(L,Temp,New).


/* --------------------------------------------------------------
   Display results
-------------------------------------------------------------- */

results(Stream):-
   new_vpe(A,B,C,D,E,F,G,H,I,J),
   format(Stream,'~p ~p ~p ~p ~p ~p ~p ~p ~p ~p~n',[A,B,C,D,E,F,G,H,I,J]),
   fail.

results(_).


/* --------------------------------------------------------------
   Automatic start
-------------------------------------------------------------- */

:- run, halt.
