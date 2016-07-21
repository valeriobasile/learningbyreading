
:- dynamic found_vpe/4, acc_ant/3.

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

:- use_module(library(lists),[member/2,select/3]).

/* =======================================================================
   Consulting gold standard 
========================================================================*/


:- ['working/wsj/vpe00.pl'].
:- ['working/wsj/vpe01.pl'].
:- ['working/wsj/vpe02.pl'].
:- ['working/wsj/vpe03.pl'].
:- ['working/wsj/vpe04.pl'].
:- ['working/wsj/vpe05.pl'].
:- ['working/wsj/vpe06.pl'].
:- ['working/wsj/vpe07.pl'].
:- ['working/wsj/vpe08.pl'].
:- ['working/wsj/vpe09.pl'].
:- ['working/wsj/vpe10.pl'].
:- ['working/wsj/vpe11.pl'].
:- ['working/wsj/vpe12.pl'].
:- ['working/wsj/vpe13.pl'].
:- ['working/wsj/vpe14.pl'].
:- ['working/wsj/vpe15.pl'].
:- ['working/wsj/vpe16.pl'].
:- ['working/wsj/vpe17.pl'].
:- ['working/wsj/vpe18.pl'].
:- ['working/wsj/vpe19.pl'].

/*
:- ['working/wsj/vpe20.pl'].
:- ['working/wsj/vpe21.pl'].
:- ['working/wsj/vpe22.pl'].
:- ['working/wsj/vpe23.pl'].
:- ['working/wsj/vpe24.pl'].
*/


/* =======================================================================
   Corpora (comment in/out to select/unselect)

   Files created with

   bin/candc
   --candc-int-betas "0.075 0.03 0.01 0.005 0.001 0.0005 0.00001" 
   --candc-int-dict_cutoffs "20 20 20 20 150 200 500"
   --candc-parser-maxsupercats 1000000

   bin/boxer --input working/wsj/wsj.discourse.??.ccg 
             --output working/wsj/wsj.discourse.??.drs 
             --resolve --vpe --flat --instantiate --presup min --x
======================================================================= */

corpus(wsj00,'working/wsj/wsj.discourse.00.drs').
/*
corpus(wsj01,'working/wsj/wsj.discourse.01.drs').
corpus(wsj02,'working/wsj/wsj.discourse.02.drs').
corpus(wsj03,'working/wsj/wsj.discourse.03.drs').
corpus(wsj04,'working/wsj/wsj.discourse.04.drs').
corpus(wsj05,'working/wsj/wsj.discourse.05.drs').
corpus(wsj06,'working/wsj/wsj.discourse.06.drs').
corpus(wsj07,'working/wsj/wsj.discourse.07.drs').
corpus(wsj08,'working/wsj/wsj.discourse.08.drs').
corpus(wsj09,'working/wsj/wsj.discourse.09.drs').
corpus(wsj10,'working/wsj/wsj.discourse.10.drs').
corpus(wsj11,'working/wsj/wsj.discourse.11.drs').
corpus(wsj12,'working/wsj/wsj.discourse.12.drs').
corpus(wsj13,'working/wsj/wsj.discourse.13.drs').
corpus(wsj14,'working/wsj/wsj.discourse.14.drs').
corpus(wsj15,'working/wsj/wsj.discourse.15.drs').
corpus(wsj16,'working/wsj/wsj.discourse.16.drs').
corpus(wsj17,'working/wsj/wsj.discourse.17.drs').
corpus(wsj18,'working/wsj/wsj.discourse.18.drs').
corpus(wsj19,'working/wsj/wsj.discourse.19.drs').
*/

/*
corpus(wsj20,'working/wsj/wsj.discourse.20.drs').
corpus(wsj21,'working/wsj/wsj.discourse.21.drs').
corpus(wsj22,'working/wsj/wsj.discourse.22.drs').
corpus(wsj23,'working/wsj/wsj.discourse.23.drs').
corpus(wsj24,'working/wsj/wsj.discourse.24.drs').
*/

/* =======================================================================
   Main
======================================================================= */

main:-
   retractall(found_vpe(_,_,_,_)),
   retractall(acc_ant(_,_,_)),
   assert(acc_ant(0,0,0)),
   setof(c(Corpus,File),corpus(Corpus,File),L),
   open('working/vpe.html',write,Stream,[encoding(utf8)]),
   write(Stream,'<html>'), nl(Stream),
   write(Stream,'<h2>Verb Phrase Ellipsis Detection</h2>'), nl(Stream),
   write(Stream,'<table border="1">'), nl(Stream),
   write(Stream,'<th>Section</th><th>Line</th><th>VPE</th><th>Acc</th><th>F</th><th>Example</th>'), nl(Stream),
   detect(L,[],Stream),
   write(Stream,'</table>'), nl(Stream),
   results(Stream),
   write(Stream,'</html>'), nl(Stream),
   close(Stream).


/* =======================================================================
   Detection
======================================================================= */

detect([],_,_).

detect([c(Corpus,FILE)|L],Corpora,Stream):-
   retractall(sem(_,_,_,_,_)),
   retractall(id(_,_)),
   consult(FILE),
   findall(Id,id(Id,_),IDs),
   detect2(IDs,Corpus,Stream),
   detect(L,[Corpus|Corpora],Stream).

detect2([],Corpus,Stream):-
   missing(Corpus,Stream).

detect2([ID|L],Corpus,Stream):-
   id(ID,Sem),
   sem(Sem,Words,_,_,DRS),
   detect_vpe(Words,Corpus,ID,DRS,Stream), 
   detect2(L,Corpus,Stream).


/* -----------------------------------------------------------------------------
   VPE
----------------------------------------------------------------------------- */

detect_vpe(Words,Corpus,ID,DRS,Str):-
   member(_:VPEInd:pred(E1,do,vpe,1),DRS),
   member(Type,[likewise,so]),
   member(_:_:pred(E2,Type,a,0),DRS), E1==E2,
   output(Corpus,ID,VPEInd,Type,Words,Str),
   fail.

detect_vpe(Words,Corpus,ID,DRS,Str):-
   member(_:VPEInd:pred(E1,Sym,vpe,1),DRS),
   \+ (Sym=do, member(Type,[likewise,so]), member(_:_:pred(E2,Type,a,0),DRS), E1==E2),
   output(Corpus,ID,VPEInd,Sym,Words,Str),
   fail.

detect_vpe(Words,Corpus,ID,DRS,Str):-
   member(_:VPEInd:pred(E1,do,v,97),DRS),
   member(Type,[likewise,so]),
   member(_:_:pred(E2,Type,a,0),DRS), E1==E2,
   output(Corpus,ID,VPEInd,Type,Words,Str),
   fail.

detect_vpe(Words,Corpus,ID,DRS,Str):-
   member(_:VPEInd:pred(E1,Sym,v,97),DRS),
   \+ (Sym=do, member(Type,[likewise,so]), member(_:_:pred(E2,Type,a,0),DRS), E1==E2),
   output(Corpus,ID,VPEInd,Sym,Words,Str),
   fail.

detect_vpe(_,_,_,_,_).


/* =======================================================================
   Output
======================================================================= */

output(_Corpus,_ID,_,Type,_Words,_Stream):-
   excluded(Type), !.

output(Corpus,_ID,[VPE|_],_,_Words,_Stream):-
   Position is mod(VPE,1000),
   Sentence is (VPE-Position)/1000,
   found_vpe(Corpus,Sentence,Position,_), !.

output(Corpus,ID,[VPE,ABeg,AEnd|_],Type,Words,Stream):-
   Position is mod(VPE,1000),
   Sentence is (VPE-Position)/1000,
   write(Stream,'<tr>'),
   format(Stream,'<td>~p</td><td>~p</td>',[Corpus,ID]),
   assert(found_vpe(Corpus,Sentence,Position,Type)),
   correct(Corpus,Sentence,Position,Stream,Result,GoldAnt),
   evalAntecedent([ABeg,AEnd],GoldAnt,Stream),
   printVPE(Words,[VPE,ABeg,AEnd],Result,GoldAnt,Stream), !,
   write(Stream,'</td></tr>'),
   nl(Stream).

output(Corpus,ID,[VPE],Type,Words,Stream):-
   Position is mod(VPE,1000),
   Sentence is (VPE-Position)/1000,
   write(Stream,'<tr>'),
   format(Stream,'<td>~p</td><td>~p</td>',[Corpus,ID]),
   assert(found_vpe(Corpus,Sentence,Position,Type)),
   correct(Corpus,Sentence,Position,Stream,Result,GoldAnt),
   evalAntecedent([],GoldAnt,Stream),
   printVPE(Words,[VPE],Result,GoldAnt,Stream), !,
   write(Stream,'</td></tr>'),
   nl(Stream).


/* =======================================================================
   Check Correctness VPE
======================================================================= */

correct(Corpus,Sentence,Position,Stream,r,Gold):-
   vpe(Id,Corpus,Sentence,Position), 
   \+ (excluded(Type),vpe_type(Id,Corpus,Type,_)), !,
   ant_beg(Id,Corpus,SenBegAnt,PosBegAnt), BegAnt is (SenBegAnt*1000)+PosBegAnt,
   ant_end(Id,Corpus,SenEndAnt,PosEndAnt), EndAnt is (SenEndAnt*1000)+PosEndAnt,
   Gold = [BegAnt,EndAnt],
   format(Stream,'<td>R</td>',[]).

correct(_,_,_,Stream,w,[]):-
   write(Stream,'<td>W</td>').


/* =======================================================================
   Check Correctness Antecedent choice
======================================================================= */

nCorrect(N,N,Beg,End,Old,New):-
   (N = Beg; N = End; N > Beg, N < End), !,
   New is Old + 1.

nCorrect(N,N,_,_,Old,Old):- !.

nCorrect(I,N,Beg,End,Old,New):-
   (I = Beg; I = End; I > Beg, I < End), !, 
   Temp is Old + 1,
   J is I + 1,
   nCorrect(J,N,Beg,End,Temp,New).

nCorrect(I,N,Beg,End,Old,New):-
   J is I + 1,
   nCorrect(J,N,Beg,End,Old,New).


/* =======================================================================
   Check Correctness Antecedent choice
======================================================================= */

evalAntecedent(Found,Gold,Stream):-
   Found = [BF,EF], Gold = [BG,EG], !,

   ( BG > EF, !, Result = 0 
   ; BF > EG, !, Result = 0
   ; Result is ((1+(min(EF,EG)-max(BF,BG))) / (1+(max(EF,EG)-min(BF,BG)))) ),
   retract(acc_ant(Total1,Total3,N1)),
   Total2 is Total1 + Result,
   
   NGold is (EG-BG)+1,
   NFound is (EF-BF)+1,
   nCorrect(BF,EF,BG,EG,0,NCorrect),
   Precision is NCorrect/NFound,
   Recall is NCorrect/NGold,
   ( Precision = 0, Recall = 0, Fscore = 0
   ; Fscore is (2*Precision*Recall)/(Precision+Recall) ),
   Total4 is Total3 + Fscore,

   N2 is N1 + 1,
   assert(acc_ant(Total2,Total4,N2)),

%   format(Stream,'<td>~p</td><td>P:~p R:~p F:~p (Found:~p Gold:~p Correct:~p)</td><td>',[Result,Precision,Recall,Fscore,NFound,NGold,NCorrect]).
   format(Stream,'<td>~p</td><td>~p</td><td>',[Result,Fscore]).

evalAntecedent(_Found,_Gold,Stream):-
   format(Stream,'<td>-</td><td>-</td><td>',[]).
   

/* =======================================================================
   Exclude certain types
======================================================================= */

%excluded(do).
%excluded(be).      % checked
%excluded(have).    % checked

%excluded(can).     % checked
%excluded(could).   % checked
%excluded(would).   % checked
%excluded(should).  % checked
%excluded(will).    % checked
%excluded(might).   % checked
%excluded(may).     % checked
%excluded(must).    % checked

%excluded(to).      % checked - antecednets can be improved/one missing case?

excluded(same).    % checked
excluded(likewise).% checked
excluded(so).      % checked


/* =======================================================================
   Calculate Results VPE detection (by type)
======================================================================= */

results_by_type(Stream):-
   setof(Type,A^B^C^vpe_type(A,B,Type,C),Types),
   results_by_type(Types,Stream).

results_by_type([],Stream):- nl(Stream).

results_by_type([Type|L],Stream):- 
   excluded(Type), !,
   results_by_type(L,Stream).

results_by_type([Type|L],Stream):- 
   findall(X,(found_vpe(X,S,P,_),vpe(ID,X,S,P),vpe_type(ID,X,Type,_)),LC),
   length(LC,CorrectVPEFound),

   findall(X,(corpus(Corpus,_),vpe(ID,Corpus,_,_),vpe_type(ID,Corpus,Type,_)),LA),
   length(LA,AllVPEInTest),

   findall(t(S,P),found_vpe(_,S,P,Type),LF),
   length(LF,AllVPEFound),

   ( AllVPEInTest = 0, Recall = 0 ; \+ AllVPEInTest = 0, Recall is CorrectVPEFound/AllVPEInTest ),
   ( AllVPEFound = 0,  Precision = 1 ; \+ AllVPEFound = 0, Precision is CorrectVPEFound/AllVPEFound ),
   ( \+ (Precision = 0, Recall = 0), FScore is (2 * Precision * Recall)/(Precision + Recall)
   ;  Precision = 0, Recall = 0, FScore = 0 ),

   format(Stream,'<tr><td><i>~p</i></td><td>~f (~p/~p)</td><td>~f (~p/~p)</td><td>~f</td></tr>~n',[Type,Recall,CorrectVPEFound,AllVPEInTest,Precision,CorrectVPEFound,AllVPEFound,FScore]),

   results_by_type(L,Stream).


/* =======================================================================
   Calculate Results VPE detection
======================================================================= */

results(Stream):-
   write(Stream,'<h2>Results VPE Detection</h2>'), nl(Stream),
   write(Stream,'<table border="1">'), nl(Stream),
   write(Stream,'<th>Type</th><th>Recall</th><th>Precision</th><th>F-Score</th>'), nl(Stream),
   results_by_type(Stream),

   findall(X,(found_vpe(X,S,P,_),vpe(ID,X,S,P),vpe_type(ID,X,Type,_),\+ excluded(Type)),LC),
   length(LC,CorrectVPEFound),

   findall(X,(corpus(Corpus,_),vpe(ID,Corpus,_,_),vpe_type(ID,Corpus,Type,_),\+ excluded(Type)),LA),
   length(LA,AllVPEInTest),

   findall(t(S,P),found_vpe(_,S,P,_),LF),
   length(LF,AllVPEFound),

   (  AllVPEInTest = 0, Recall = 0; \+ AllVPEInTest = 0,
      Recall is CorrectVPEFound/AllVPEInTest ),

   (  AllVPEFound = 0, Precision = 1; \+ AllVPEFound = 0,
      Precision is CorrectVPEFound/AllVPEFound ),

   ( \+ (Precision = 0, Recall = 0),  FScore is (2 * Precision * Recall)/(Precision + Recall);
      Precision = 0, Recall = 0, FScore = 0 ),

   format(Stream,'<tr><td>~p</td><td>~p (~p/~p)</td><td>~p (~p/~p)</td><td>~p</td></tr>~n',
                  ['Total',Recall,CorrectVPEFound,AllVPEInTest,Precision,CorrectVPEFound,AllVPEFound,FScore]),
   write(Stream,'</table'), nl(Stream),

   write(Stream,'<h2>Results VPE antecedent choice</h2>'),
   acc_ant(AccTotal,AccF,AccN),
   ( AccN = 0, !, AveAcc = 0, AveF = 0
   ; AveAcc is AccTotal/AccN, AveF is AccF/AccN ),
   format(Stream,'<p>Average accuracy (~p/~p): ~f</p>~n',[AccTotal,AccN,AveAcc]),
   format(Stream,'<p>Average F-score (~p/~p): ~f</p>~n',[AccF,AccN,AveF]).


/* =======================================================================
   Print VPE Sentence
======================================================================= */

printVPE([],_,_,_,_):- !.

printVPE([word(Index,_)|L],VPE,Res,Gold,Stream):- 
   \+ relevantContext(Index,VPE), 
   \+ relevantContext(Index,Gold), !,
   printVPE(L,VPE,Res,Gold,Stream).

printVPE([word(Index,Word)|L],VPE,Res,Gold,Stream):- 
   VPE = [Index|_], !,
   ( Res=r, Col=blue, !; Res=m, Col=orange, !; Col=red ),
   format(Stream,'<font color="~p"><b>',[Col]),
   write(Stream,Word),
   write(Stream,'</b></font> '),
   printVPE(L,VPE,Res,Gold,Stream).

printVPE([word(Index,Word)|L],VPE,Res,Gold,Stream):- 
   VPE = [_,Index,Index|_], !,
   printGoldBeg(Index,Gold,Stream),
   write(Stream,'<b>'),
   write(Stream,Word),
   printGoldEnd(Index,Gold,Stream),
   write(Stream,'</b> '),
   printVPE(L,VPE,Res,Gold,Stream).

printVPE([word(Index,Word)|L],VPE,Res,Gold,Stream):- 
   VPE = [_,Index|_], !,
   printGoldBeg(Index,Gold,Stream),
   write(Stream,'<b>'),
   write(Stream,Word),
   printGoldEnd(Index,Gold,Stream),
   write(Stream,' '),
   printVPE(L,VPE,Res,Gold,Stream).

printVPE([word(Index,Word)|L],VPE,Res,Gold,Stream):- 
   VPE = [_,_,Index|_], !,
   printGoldBeg(Index,Gold,Stream),
   write(Stream,Word),
   printGoldEnd(Index,Gold,Stream),
   write(Stream,'</b> '),
   printVPE(L,VPE,Res,Gold,Stream).

printVPE([word(Index,Word)|L],VPE,Res,Gold,Stream):-
   printGoldBeg(Index,Gold,Stream),
   write(Stream,Word),
   printGoldEnd(Index,Gold,Stream),
   tab(Stream,1),
   printVPE(L,VPE,Res,Gold,Stream).



/* =======================================================================
   Print Gold Antecedent
======================================================================= */

printGoldBeg(Index,[Gold,_],Stream):-
   Gold = Index, !,
   write(Stream,'<B>[</B>').

printGoldBeg(_,_,_).

printGoldEnd(Index,[_,Gold],Stream):-
   Gold = Index, !,
   write(Stream,'<B>]</B>').

printGoldEnd(_,_,_).


/* =======================================================================
   Relevant Context
======================================================================= */

relevantContext(Index,L):-
   S is Index-mod(Index,1000),
   member(X,L), S is X-mod(X,1000), !.


/* =======================================================================
   Missing VPE
======================================================================= */

missing(Corpus,Stream):-  
   findall(gold(Corpus,S,P),
           ( vpe(ID,Corpus,S,P), \+ (excluded(Type), vpe_type(ID,Corpus,Type,_)) ),
           Gold),
   missing2(Gold,Stream).

missing2([],_).

missing2([gold(Corpus,S,P)|L],Stream):-
   found_vpe(Corpus,S,P,_), !,
   missing2(L,Stream).

missing2([gold(Corpus,S,P)|L],Stream):-
   VPEIndex is (S*1000)+P,
   sem(SemID,Words,_,_,_), 
   id(ID,SemID),
   member(word(VPEIndex,_),Words), !,
   format(Stream,'<tr><td>~p</td><td>~p</td><td>M</td><td>-</td><td>-</td><td>',[Corpus,ID]),
   printVPE(Words,[VPEIndex],m,[],Stream),
   write(Stream,'</td></tr>'), 
   nl(Stream), 
   missing2(L,Stream).   

missing2([gold(Corpus,S,P)|L],Stream):-
   format(Stream,'<tr><td>~p</td><td>?</td><td>M</td><td>-</td><td>-</td><td>~p ~p (no DRS)</td></tr>~n',[Corpus,S,P]),   
   missing2(L,Stream).


/* =======================================================================
   Automatically start
======================================================================= */

:- main, halt.
