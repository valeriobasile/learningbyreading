
% nutcracker.pl, by Johan Bos

/* ========================================================================
   File Search Paths
======================================================================== */

file_search_path(semlib,     'src/prolog/lib').
file_search_path(nutcracker, 'src/prolog/nutcracker').
file_search_path(knowledge,  'src/prolog/boxer/knowledge').


/* ========================================================================
   Dynamic Predicates
======================================================================== */

:- dynamic axiom/3.


/* ========================================================================
   Load other libraries
======================================================================== */

:- use_module(library(lists),[member/2,append/3]).
:- use_module(library(ordsets),[list_to_ord_set/2,ord_intersection/3]).
:- use_module(library(readutil),[read_line_to_codes/2]).

:- use_module(semlib(drs2fol),[drs2fol/2]).
:- use_module(semlib(errors),[error/2,warning/2,inform/2]).
:- use_module(semlib(options),[option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).

:- use_module(nutcracker(version),[version/1]).
:- use_module(nutcracker(input),[openInput/1,inputDRS/2,lenDRS/2,openModel/3]).
:- use_module(nutcracker(callInference),[callMBbis/7,callTPandMB/8]).
:- use_module(nutcracker(miniFrameNet),[axiomsFN/2]).
:- use_module(nutcracker(counting),[countingAxioms/2]).
:- use_module(nutcracker(miniWordNet),[compConcepts/2,compISA/0,
                                       clearMWN/0,cutDownMWN/0,
                                       addTopMWN/0,graphMWN/2,sizeMWN/1,
                                       outputMWN/2,axiomsWN/1]).


/* ========================================================================
   Main
======================================================================== */

main:-
   option(Option,do), 
   member(Option,['--version','--help']), !, 
   version,
   help.

main:-
   checkDir([Dir|Dirs]), !,
   axioms(Dir),
   main([Dir|Dirs]).

main:-
   setOption(nutcracker,'--help',do), !,
   help.


/*========================================================================
   Main (traverse directories)
========================================================================*/

main([]).

main([X|Dirs]):-
   checkFiles(X), !,
   tokenise(X,Overlap), 
   pipeline(X,Overlap),
   main(Dirs).

main([X|Dirs]):-
   atom_concat(X,'/*',Wild),
   subdirs(Wild,SubDirs), \+ SubDirs = [], !,
   main(SubDirs),
   main(Dirs).

main([_|Dirs]):-
   main(Dirs).


/*------------------------------------------------------------------------
   Pipeline
------------------------------------------------------------------------*/

pipeline(X,Overlap):-
   option('--inference',yes),
   meta(X), parse(X), wsd(X), box(X), 
   mwn(X,Ax1,Ax2,Ax3,Novelty),
   nc(X,Ax1,Ax2,Ax3), !,
   prediction(X,Novelty,Overlap).

pipeline(X,Overlap):-
   option('--inference',no),
   meta(X), parse(X), wsd(X), box(X), 
   mwn(X,_,_,_,Novelty), !,
   prediction(X,Novelty,Overlap).

pipeline(X,Overlap):-
   option('--inference',only),
   box(X),
   mwn(X,Ax1,Ax2,Ax3,Novelty), 
   nc(X,Ax1,Ax2,Ax3), !,
   prediction(X,Novelty,Overlap).

pipeline(X,Overlap):-
   prediction(X,-1,Overlap).


/*------------------------------------------------------------------------
   Check Dir
------------------------------------------------------------------------*/

checkDir(Dirs):-
   checkDir1(Dir),        % remove slash at end (if there is one)
   checkDir2(Dir,Dirs).   % check permissions

checkDir1(NewDir):-
   option('--dir',Dir),
   atom_chars(Dir,Chars),
   append(NewChars,['/'],Chars), !,
   atom_chars(NewDir,NewChars),
   setOption(nutcracker,'--dir',NewDir).

checkDir1(Dir):-
   option('--dir',Dir).

checkDir2(Dir,[Dir]):-
   exists_directory(Dir), 
   access_file(Dir,write), !.

checkDir2(Dir,List):- 
   subdirs(Dir,List), !.

checkDir2(Dir,[]):-
   error('cannot access directory ~p',[Dir]).


/*------------------------------------------------------------------------
   Sub Dirs
------------------------------------------------------------------------*/

subdirs(Wild,Dirs):-
   expand_file_name(Wild,List),
   findall(D,( member(D,List),
               exists_directory(D),
               access_file(D,write) ),Dirs), !.


/*------------------------------------------------------------------------
   Check presence of files t and h
------------------------------------------------------------------------*/

checkFiles(Dir):-
   atomic_list_concat([Dir,'/','t'],TFile),
   atomic_list_concat([Dir,'/','h'],HFile),   
   atomic_list_concat([Dir,'/','gold.txt'],GFile),   
   access_file(TFile,read),
   access_file(HFile,read),
   access_file(GFile,read), !,
   printTHG(Dir,TFile,HFile,GFile).

checkFiles(Dir):-
   atomic_list_concat([Dir,'/','t'],TFile),
   atomic_list_concat([Dir,'/','h'],HFile),   
   access_file(TFile,read),
   access_file(HFile,read), !,
   printTHG(Dir,TFile,HFile).

checkFiles(Dir):-
   warning('directory ~p does not contain files named t and h',[Dir]), 
   !, fail.   


/*------------------------------------------------------------------------
   Print t and h file   
------------------------------------------------------------------------*/

printTHG(_,_,_,_):-
   option('--info',false), !.

printTHG(Dir,TFile,HFile,GFile):-
   option('--info',true), 
   inform('[=====> ~p <=====]',[Dir]),
   inform('Text:',[]),
   atomic_list_concat(['cat',TFile],' ',Shell1),
   shell(Shell1,Return1), Return1 = 0,
   inform('Hypothesis:',[]),
   atomic_list_concat(['cat',HFile],' ',Shell2),
   shell(Shell2,Return2), Return2 = 0, 
   inform('Annotation:',[]),
   atomic_list_concat(['cat',GFile],' ',Shell3),
   shell(Shell3,Return3), Return3 = 0, !.

printTHG(_,_,_,_):-
   error('failed to access t and h file',[]).

printTHG(_,_,_):-
   option('--info',false), !.

printTHG(Dir,TFile,HFile):-
   option('--info',true), 
   inform('[=====> ~p <=====]',[Dir]),
   inform('Text:',[]),
   atomic_list_concat(['cat',TFile],' ',Shell1),
   shell(Shell1,Return1), Return1 = 0,
   inform('Hypothesis:',[]),
   atomic_list_concat(['cat',HFile],' ',Shell2),
   shell(Shell2,Return2), Return2 = 0, !.

printTHG(_,_,_):-
   error('failed to access t and h file',[]).


/*------------------------------------------------------------------------
   Tokenise (init)
------------------------------------------------------------------------*/

tokenise(Dir,Overlap):-
   atomic_list_concat([Dir,'/','t'],TFile),
   atomic_list_concat([Dir,'/','h'],HFile),   
   atomic_list_concat([Dir,'/','t.tok'],TFileTOK),
   atomic_list_concat([Dir,'/','h.tok'],HFileTOK),
   tokeniseFile(TFile,TFileTOK),
   tokeniseFile(HFile,HFileTOK),
   bagofwords(TFileTOK,TWords),
   bagofwords(HFileTOK,HWords),
   overlap(TWords,HWords,Overlap).


/*------------------------------------------------------------------------
   Tokenise
------------------------------------------------------------------------*/

tokeniseFile(In,Out):-
   atomic_list_concat(['bin/tokkie',
                '--quotes',delete,
                '--input',In,
                '--output',Out],' ',Shell),
   write(Shell), nl,
   shell(Shell,Return), 
   Return = 0, !.

tokeniseFile(In,_):-
   error('problem tokenising ~p',[In]), 
   !, fail.


/* ------------------------------------------------------------------------
   Bag of words overlap: 
   the higher the overlap, the more likely an entailment

   T : a b c d      a b c    a b c       a b 
   H :     c d e      b          c d e       c d
   O = 2/3          1/1      1/3         0/2
------------------------------------------------------------------------ */

bagofwords(File,Bag):-
   open(File,read,Stream),
   read_line_to_codes(Stream,Codes),
   close(Stream),
   atom_codes(Atom,Codes),
   atomic_list_concat(Bag,' ',Atom).

overlap(T,H,Overlap):-
   list_to_ord_set(T,TO),
   list_to_ord_set(H,HO),
   ord_intersection(TO,HO,Intersection),
   length(Intersection,CardTandH),
   length(HO,CardH), CardH > 0,
   Overlap is CardTandH/CardH.


/*------------------------------------------------------------------------
   File preparation (adding META markup)
------------------------------------------------------------------------*/

meta(Dir):-
   atomic_list_concat([Dir,'/','t.tok'],TFile),
   access_file(TFile,read),
   atomic_list_concat([Dir,'/','h.tok'],HFile),
   access_file(HFile,read), 
   atomic_list_concat([Dir,'/','th.tok'],THFile),
   atomic_list_concat([cat,TFile,HFile,'>',THFile],' ',Shell), 
   write(Shell), nl,
   shell(Shell,0), !.

meta(Dir):-
   error('directory ~p does not contain files named t.tok and h.tok',[Dir]), 
   !, fail.   


/*------------------------------------------------------------------------
   Parse (init)
------------------------------------------------------------------------*/

parse(Dir):-
   atomic_list_concat([Dir,'/', 't.tok'], TFileTOK),
   atomic_list_concat([Dir,'/', 'h.tok'], HFileTOK),   
   atomic_list_concat([Dir,'/','th.tok'],THFileTOK),   
   access_file( TFileTOK,read),
   access_file( HFileTOK,read),
   access_file(THFileTOK,read), !,
   atomic_list_concat([Dir,'/', 't.ccg'], TFileCCG),
   atomic_list_concat([Dir,'/', 'h.ccg'], HFileCCG),
   atomic_list_concat([Dir,'/','th.ccg'],THFileCCG),
   parse( TFileTOK, TFileCCG),
   parse( HFileTOK, HFileCCG),
   parse(THFileTOK,THFileCCG).

parse(Dir):-
   error('directory ~p does not contain files named *.tok',[Dir]), 
   !, fail.   


/*------------------------------------------------------------------------
   Parse
------------------------------------------------------------------------*/

parse(In,Out):-
   option('--soap',true),
   atomic_list_concat(['bin/soap_client',
                '--url http://localhost:9000',
                '--input',In,
                '--output',Out],' ',Shell),
   write(Shell), nl,
   shell(Shell,0), !.

parse(In,Out):-
   option('--soap',false),
   atomic_list_concat(['bin/candc',
                '--input',In,
                '--output',Out,
                '--models models/boxer',
                '--candc-printer boxer'],' ',Shell),
   write(Shell), nl,
   shell(Shell,0), !.

parse(In,_):-
   error('cannot parse ~p',[In]), 
   !, fail.   


/*------------------------------------------------------------------------
   Boxer (init)
------------------------------------------------------------------------*/

box(Dir):-
   ( option('--wsd',true), !, Ext = 'ccg.wsd'; Ext = 'ccg' ),
   atomic_list_concat([Dir,'/', 't.',Ext], TFileCCG),
   atomic_list_concat([Dir,'/', 'h.',Ext], HFileCCG),
   atomic_list_concat([Dir,'/','th.',Ext],THFileCCG),
   access_file( TFileCCG,read),
   access_file( HFileCCG,read),
   access_file(THFileCCG,read), !,
   atomic_list_concat([Dir,'/', 't.drs'], TFileDRS),
   atomic_list_concat([Dir,'/', 'h.drs'], HFileDRS),
   atomic_list_concat([Dir,'/','th.drs'],THFileDRS),
   box( TFileCCG,TFileDRS),
   box( HFileCCG,HFileDRS),
   box(THFileCCG,THFileDRS).

box(Dir):-
   error('directory ~p does not contain files named t.ccg and h.ccg',[Dir]), 
   !, fail.   


/*------------------------------------------------------------------------
   Boxer 
------------------------------------------------------------------------*/

box(In,Out):-
   option('--plural',PluralOpt), 
   option('--modal',ModalOpt), 
%  option('--vpe',VpeOpt), 
   option('--copula',CopOpt), 
   option('--warnings',WarOpt), 
   option('--roles',RolesOpt), 
   option('--resolve',ResolveOpt), 
   option('--nn',NNOpt), 
   option('--x',XOpt), 
   atomic_list_concat([Out,xml],'.',OutXML),
   atomic_list_concat(['bin/boxer',
                '--input',In,
                '--output',OutXML,
                '--plural',PluralOpt,
                '--modal',ModalOpt,
                '--copula',CopOpt,
                '--roles',RolesOpt,
		'--format',xml,
                '--nn',NNOpt,
                '--x',XOpt,
                '--elimeq',false,
                '--resolve',ResolveOpt,
                '--integrate',true,
                '--warnings',WarOpt,
                '--box'],' ',ShellXML),
   shell(ShellXML,_),
   atomic_list_concat(['bin/boxer',
                '--input',In,
                '--output',Out,
                '--plural',PluralOpt,
                '--modal',ModalOpt,
                '--copula',CopOpt,
                '--roles',RolesOpt,
                '--nn',NNOpt,
                '--x',XOpt,
                '--elimeq',false,
                '--resolve',ResolveOpt,
                '--integrate',true,
                '--warnings',WarOpt,
                '--box'],' ',Shell),
   write(Shell), nl,
   shell(Shell,Return),
   Return = 0, !.

box(In,_):-
   error('cannot box ~p',[In]), 
   !, fail.   


/*------------------------------------------------------------------------
   WSD (init)
------------------------------------------------------------------------*/

wsd(Dir):-
   option('--wsd',true), 
   atomic_list_concat([Dir,'/','t.ccg'],TFileCCG),
   atomic_list_concat([Dir,'/','h.ccg'],HFileCCG),
   atomic_list_concat([Dir,'/','th.ccg'],THFileCCG),
   access_file(TFileCCG,read),
   access_file(HFileCCG,read), !,
   access_file(THFileCCG,read), !,
   atomic_list_concat([Dir,'/','t.ccg.wsd'],TFileWSD),
   atomic_list_concat([Dir,'/','h.ccg.wsd'],HFileWSD),
   atomic_list_concat([Dir,'/','th.ccg.wsd'],THFileWSD),
   wsd(TFileCCG,TFileWSD),
   wsd(HFileCCG,HFileWSD),
   wsd(THFileCCG,THFileWSD).

wsd(Dir):-
   option('--wsd',true), 
   error('directory ~p does not contain files named t.ccg and h.ccg',[Dir]), 
   !, fail.   

wsd(_):- 
   option('--wsd',false).


/*------------------------------------------------------------------------
   WSD (external) 
------------------------------------------------------------------------*/

wsd(CCG,WSD):-
   atomic_list_concat(['ext/wsd.pl',
                       '--input',CCG,
                       '--output',WSD,
                       '--slearner','ext/senselearner/'],' ',Shell),
   write(Shell), nl,
   shell(Shell,Return),
   Return = 0, !.

wsd(_,In):-
   error('cannot wsd ~p',[In]), 
   !, fail.   


/* =======================================================================
   Textual Entailment (logical inference)
========================================================================*/

nc(Dir,KT,KH,KTH):-
   openInput(Dir),

   inputDRS(t,TDRS), 
   inputDRS(h,HDRS), 
   inputDRS(th,THDRS), 

   countingAxioms([],Ax),
   consistent(  TDRS, Ax, Dir,  t, 1,ModT), domSize(ModT,DomT),
   consistent(  HDRS, Ax, Dir,  h, 1,ModH),   
   consistent( THDRS, Ax, Dir, th, DomT,ModTH),

   informative(TDRS,THDRS, Ax, Dir, tth, 1, _),

   countingAxioms(KT,KTAx),
   bk(ModT,KTAx,KTBAx),
   consistent(  TDRS, KTBAx, Dir,  kt, 1,ModKT), domSize(ModKT,DomKT),

   countingAxioms(KH,KHAx),
   bk(ModH,KHAx,KHBAx),
   consistent(  HDRS, KHBAx, Dir, kh, 1, _), 

   countingAxioms(KTH,KTHAx),
   bk(ModTH,KTHAx,KTHBAx),
   consistent( THDRS,KTHBAx, Dir, kth, DomKT,_),
   informative(TDRS,THDRS,KTHBAx, Dir,ktkth, 1, _).


/* =======================================================================
   Load Axioms
========================================================================*/

axioms(_):-
   option('--axioms',File), 
   File = none, !.

axioms(Dir):-
   option('--axioms',File), 
   access_file(File,read),
   catch(load_files([File],[autoload(true),encoding(utf8)]),_,fail),
   findall(imp(A,B),imp(A,B),Axioms), !,
   preprocessAxioms(Axioms,Dir,0).
   
axioms(_):-
   option('--axioms',File), 
   error('cannot access axioms ~p',[File]).


/* =======================================================================
   Process Axioms
========================================================================*/

preprocessAxioms([],_,N):-
   inform('Background knowledge: ~p axioms',[N]).

preprocessAxioms([imp(A,B)|L],Dir,M):-
   option('--modal',true), N is M+1,
   drs2fol(drs([],[nec(drs([],[imp(A,B)]))]),Axiom), 
   drs2fol(A,Antecedent), !,
   callTPandMB(Dir,[],not(Antecedent),Antecedent,1,10,Model,_Engine),
   Model = model(_,F),
   findall(Sym,member(f(_,Sym,_),F),Symbols),
   assert(axiom(N,Symbols,Axiom)),
   preprocessAxioms(L,Dir,N).

preprocessAxioms([imp(A,B)|L],Dir,M):-
   option('--modal',false), N is M+1,
   drs2fol(drs([],[imp(A,B)]),Axiom), !,
   callTPandMB(Dir,[],not(Axiom),Axiom,1,10,Model,_Engine),
   Model = model(_,F),
   findall(Sym,member(f(_,Sym,_),F),Symbols),
   assert(axiom(N,Symbols,Axiom)),
   preprocessAxioms(L,Dir,N).


/* =======================================================================
   Include Background Knowledge
========================================================================*/

bk(model(_,F),In,Out):-
   findall(N,axiom(N,_,_),L),
   bk(L,F,0,In,Out).

bk([],_,N,A,A):-
   inform('added a total of ~p axioms',[N]).

bk([A|L],F,N1,In,[Axiom|Out]):-
   axiom(A,Symbols,Axiom),
   member(f(1,X,_),F), 
   member(X,Symbols), 
   \+ X = n1numeral,
   !,
   inform('added axiom ~p triggered by ~p',[A,X]),
   N2 is N1 + 1,
   bk(L,F,N2,In,Out).

bk([_|L],F,N,In,Out):-
   bk(L,F,N,In,Out).


/* =======================================================================
   Textual Entailment (WordNet)
========================================================================*/

mwn(Dir,AxiomsKT,AxiomsKH,AxiomsKTH,Novelty):-
   openInput(Dir),
   inputDRS(t,TDRS), computeMWN(TDRS,Dir,kt,DomT),  
   axiomsWN(WNAxiomsKT), 
   axiomsFN(TDRS,FNAxiomsKT), 
   append(WNAxiomsKT,FNAxiomsKT,AxiomsKT),
   inputDRS(h,HDRS), computeMWN(HDRS,Dir,kh,DomH), 
   axiomsWN(WNAxiomsKH), axiomsFN(HDRS,FNAxiomsKH),
   append(WNAxiomsKH,FNAxiomsKH,AxiomsKH),
   inputDRS(th,THDRS), computeMWN(THDRS,Dir,kth,DomTH), 
   axiomsWN(WNAxiomsKTH), axiomsFN(THDRS,FNAxiomsKTH),
   append(WNAxiomsKTH,FNAxiomsKTH,AxiomsKTH),
   computeNovelty(DomT,DomH,DomTH,Novelty).


/* =======================================================================
   Inference -- consistency check
========================================================================*/

consistent(_,_,Dir,Name,DomSize,Model):- 
   DomSize = 0, !, 
   Model = model([],[]),
   outputModel(Model,Name,Dir,DomSize),
   inform('previously inconsistent, no inference for ~p',[Name]).

consistent(B,BK,Dir,Name,MinDom,Model):-
   drs2fol(B,F),
   option('--domsize',MaxDom),
   callTPandMB(Dir,BK,not(F),F,MinDom,MaxDom,TmpModel,TmpEngine),
   ( member(Name,[kt,kh,kth]), !, callMBbis(Dir,BK,F,TmpModel,Model,TmpEngine,Engine)
   ; TmpModel = Model, TmpEngine = Engine ),
   outputModel(Model,Name,Dir,DomSize),
   ( DomSize > 0, !, Result = 'consistent'
   ; DomSize = 0, !, Result = 'inconsistent'
   ; DomSize < 0,    Result = 'unknown' ),
   inform('~p found result for ~p (~p, domain size: ~p)',[Engine,Name,Result,DomSize]).


/* =======================================================================
   Inference -- informativeness check
========================================================================*/

informative(B1,B2,BK,Dir,Name,MinDom,Model):-
   drs2fol(B1,F1),
   drs2fol(B2,F2),
   F = imp(F1,F2),
   option('--domsize',MaxDom),
   callTPandMB(Dir,BK,F,not(F),MinDom,MaxDom,Model,Engine),
   outputModel(Model,Name,Dir,DomSize),
   ( DomSize > 0, !, Result = 'informative'
   ; DomSize = 0, !, Result = 'uninformative'
   ; DomSize < 0,    Result = 'unknown' ),
   inform('~p found result for ~p (~p, domain size: ~p)',[Engine,Name,Result,DomSize]).


/* =======================================================================
   Prediction (try inference first, else back off to WordNet)
========================================================================*/
 
prediction(Dir,WNNovelty,Overlap):-

   openModel(Dir,t,ModT),     openModel(Dir,h,ModH),
   openModel(Dir,th,ModTH),   openModel(Dir,tth,ModTNH),
   openModel(Dir,kt,ModKT),   openModel(Dir,kh,ModKH),
   openModel(Dir,kth,ModKTH), openModel(Dir,ktkth,ModKTNH),

   domSize(ModT,DomT),        domSize(ModH,DomH),
   domSize(ModTH,DomTH),      domSize(ModTNH,DomTNH),
   domSize(ModKT,DomKT),      domSize(ModKH,DomKH),
   domSize(ModKTH,DomKTH),    domSize(ModKTNH,DomKTNH),

   relSize(ModKT,RelKT),   
   relSize(ModKH,RelKH),
   relSize(ModKTH,RelKTH),

   SizeKT is DomKT*RelKT,    % modelSize(ModKT,SizeKT),
   SizeKH is DomKH*RelKH,    % modelSize(ModKH,SizeKH),
   SizeKTH is DomKTH*RelKTH, % modelSize(ModKTH,SizeKTH),

%  compareModels(Dir,ModKTH,ModKT),

   computeNovelty(DomKT,DomKH,DomKTH,DomNovelty),
   computeNovelty(SizeKT,SizeKH,SizeKTH,SizeNovelty),
   computeNovelty(RelKT,RelKH,RelKTH,RelNovelty),

   makePrediction(DomT,DomH,DomTH,DomTNH,DomKT,DomKH,DomKTH,DomKTNH,
                  DomNovelty,RelNovelty,WNNovelty,Overlap,Prediction), !,

   outputPrediction(Dir,Prediction,DomKTNH,DomKTH,
                    DomNovelty,RelNovelty,WNNovelty,SizeNovelty,Overlap).


/* =======================================================================
   Make prediction...

   makePrediction( +T,  +H,  +TH,  +TnotH,   %%% model sizes without BK
              +KT, +KH, +KTH, +KTnotH,   %%% model sizes wit BK
              +DomNovelty, 
              +RelNovelty, 
              +WNNovelty, 
              +WordOverlap, 
              -Prediction )              %%% prediction description
========================================================================*/

% PROOF without BK by THEOREM PROVER: INPUT INCONSISTENT
%
makePrediction(T,H,_,_,_,_,_,_,_,_,_,_,Prediction):-
   option('--contradiction',true), 
   (T = 0; H = 0), !,
   Prediction = 'unknown (simple input contradiction)'.

% PROOF without BK by THEOREM PROVER: INCONSISTENT
%
makePrediction(T,H,TH,_,_,_,_,_,_,_,_,_,Prediction):-
   option('--contradiction',true), 
   T > 0, H > 0, TH = 0, !,
   Prediction = 'informative (simple inconsistency)'.

% PROOF without BK by THEOREM PROVER: UNINFORMATIVE (ENTAILMENT)
%
makePrediction(T,H,TH,TNH,_,_,_,_,_,_,_,_,Prediction):-
   T > 0, H > 0, TH > 0, TNH = 0, !,
   Prediction = 'entailed (simple proof)'. 

% PROOF with BK by THEOREM PROVER: INPUT INCONSISTENT
%
makePrediction(T,H,TH,_,KT,KH,_,_,_,_,_,_,Prediction):-
   option('--contradiction',true), 
   T > 0, H > 0, TH > 0, (KT = 0; KH = 0),  !,
   Prediction = 'unknown (complex input contradiction)'.

% PROOF with BK by THEOREM PROVER: INCONSISTENT
%
makePrediction(T,H,TH,_,KT,KH,KTH,_,_,_,_,_,Prediction):-
   option('--contradiction',true), 
   T > 0, H > 0, TH > 0, KT > 0, KH > 0, KTH = 0, !,
   Prediction = 'informative (complex inconsistency)'.

% PROOF with BK by THEOREM PROVER: UNINFORMATIVE
%
makePrediction(T,H,TH,TNH,KT,KH,KTH,KTNH,_,_,_,_,Prediction):-
    T > 0,  H > 0,  TH > 0,  TNH > 0, 
   KT > 0, KH > 0, KTH > 0, KTNH = 0, !,
   Prediction = 'entailed (complex proof)'. %%% proof with BK

% WORDNET NOVELTY
%
makePrediction(_,_,_,_,_,_,_,_,DomNovelty,_,WNNovelty,_,Prediction):-
   option('--modal',false), WNNovelty >= 0, DomNovelty < 0, !, 
   %%% DRS but no model could be computed, back off to WN novelty
%  Threshold = 0.416667, %%% RTE-2 dev  (J48, 59.6%, n= 768)
%  Threshold = 0.25,     %%% RTE-2 test (J48, 59.1%, n= 766)
%  Threshold = 0.4     , %%% RTE-2      (J48, 58.6%, n=1534)
%  Threshold = 0.363636, %%% RTE-3 dev  (J48, 58.7%, n= 770)
%  Threshold = 0.375,    %%% RTE-3 test (J48, 55.9%, n= 782) 
%  Threshold = 0.375,    %%% RTE-3      (J48, 59.6%, n=1552)
   Threshold = 0.375,    %%% RTE-2+3    (J48, 59.6%, n=3086)  
   ( WNNovelty =< Threshold, !
   , Prediction = 'entailed (wordnet novelty)'
   ; Prediction = 'informative (wordnet novelty)' ).
   
% WORD OVERLAP
%
makePrediction(_,_,_,_,_,_,_,_,_,_,_,Overlap,Prediction):-
%  Threshold = 0.576923, %%% RTE-2 dev  (J48, 60.6%)
%  Threshold = 0.533333, %%% RTE-2 test (J48, 56.1%)
%  Threshold = 0.55,     %%% RTE-2      (J48, 58.8%)
%  Threshold = 0.692308, %%% RTE-3 dev  (J48, 61.3%)
%  Threshold = 0.4,      %%% RTE-3 test (J48, 57.6%)
%  Threshold = 0.428571, %%% RTE-3      (J48, 60.8%)
   Threshold = 0.55,     %%% RTE-2+3    (J48, 60.6%)
   ( Overlap > Threshold, !
   , Prediction = 'entailed (word overlap)'
   ; Prediction = 'informative (word overlap)' ).

% MODEL NOVELTY
%
makePrediction(_,_,_,_,_,_,_,_,DomNovelty,_,_,_,Prediction):-
%  Threshold = 0.4,      %%% RTE-2 dev  ( 710 instances, J48, 63,6%) model -> 58.1; rel -> 62.9
%  Threshold = 0.375,    %%% RTE-2 test ( 692 instances, J48, 58.4%) model -> 52.0; rel -> 60.0
%  Threshold = 0.416777, %%% RTE-2      (1412 instances, J48, 60.9%) model -> 57.2; rel -> 60.0
%  Threshold = 0.375,    %%% RTE-3 dev  ( 656 instances, J48, 58.4%) model -> 58.8; rel -> 60.8
%  Threshold = 0.387,    %%% RTE-3 test ( 686 instances, J48, 58.2%) model -> 58.2; rel -> 59.5
%  Threshold = 0.6,      %%% RTE-3      (1342 instances, J48, 59,6%) model -> 57.9; rel -> 59.0
   Threshold = 0.416667, %%% RTE-2+3    (2758 instances, J48, 60,3%) model -> 57.9; rel -> 60.8
   ( DomNovelty =< Threshold, !
   , Prediction = 'entailed (model novelty)'
   ; Prediction = 'informative (model novelty)' ).


/* =======================================================================
   Output Model
========================================================================*/

outputModel(Model,Name,Dir,Size):-
   atomic_list_concat([Dir,'/',Name,'.mod'],File),
   open(File,write,Stream),
   printModel(Model,Stream), 
   write(Stream,'.'), nl(Stream),
   close(Stream),
   domSize(Model,Size).


/* =======================================================================
   Print Model
========================================================================*/

printModel(model(D,[]),Stream):- !, format(Stream,'model(~p, [])',[D]).

printModel(model(D,[F]),Stream):- !, format(Stream,'model(~p,~n  [~p])',[D,F]).

printModel(model(D,[X,Y|F]),Stream):- !,
   setof(M,Sym^Ext^(member(M,[X,Y|F]),\+ M=f(0,Sym,Ext)),[First|Sorted]),
   format(Stream,'model(~p,~n  [~p,~n',[D,First]),
   printModel(Sorted,Stream).

printModel([Last],Stream):- !, format(Stream,'   ~p])',[Last]).

printModel([X|L],Stream):- !, 
   format(Stream,'   ~p,~n',[X]), 
   printModel(L,Stream).

printModel(Model,Stream):- write(Stream,Model).



/* =======================================================================
   Determine Model Size (Domain)

modelSize(Model,Size):- 
   Model = model(_,F), !,
   modelSize(F,0,Size).

modelSize(_,-1).

modelSize([],S,S).

modelSize([f(_,Symbol,[E|Xtension])|L],Old,New):-
   idf(Symbol,IDF,_), !,
   length([E|Xtension],N), 
   Temp is Old+(IDF*N),
   modelSize(L,Temp,New).
   
modelSize([_|L],Old,New):-
   modelSize(L,Old,New).
========================================================================*/

compareModels(Dir,model(_,F1),model(_,F2)):- !, 
   atomic_list_concat([Dir,'/','novel.txt'],File),
   open(File,write,Stream),
   compareExtensions(F1,F2,Stream),
   close(Stream).

compareModels(_,_,_).


compareExtensions([],_,_).

compareExtensions([f(Arity,Sym,[_|_])|L],F,Stream):- 
   ( member(f(Arity,Sym,[_|_]),F), !
   ; write(Stream,Sym), nl(Stream)),
   compareExtensions(L,F,Stream).

compareExtensions([_|L],F,Stream):- 
   compareExtensions(L,F,Stream).


/* =======================================================================
   Determine Domain Size 
========================================================================*/

domSize(Model,Size):-
   Model = model(Dom,_), !,
   length(Dom,Size).

domSize(_,-1).


/* =======================================================================
   Determine Model Size (Relations)
========================================================================*/

relSize(Model,Size):-
   Model = model(_,F), !,
%  findall(R,(member(f(2,_,E),F),member(R,E)),Rs),
   findall(R,(member(f(_,_,E),F),member(R,E)),Rs),
   length(Rs,Size).

relSize(_,-1).


/* =======================================================================
   Output Prediction
========================================================================*/

outputPrediction(Dir,Prediction,Proof,Contra,DomNovelty,RelNovelty,WNNovelty,SizeNovelty,Overlap):-
   atomic_list_concat([Dir,'/','prediction.txt'],File),
   open(File,write,Stream),
   write(Stream,Prediction), nl(Stream),
   close(Stream),
   inform('prediction: ~p',[Prediction]),
   outputDomSizeDif(Dir,Proof,Contra,DomNovelty,RelNovelty,WNNovelty,SizeNovelty,Overlap).


/* =======================================================================
   Output Domain Size Difference
========================================================================*/

outputDomSizeDif(Dir,Proof,Contradiction,Dom,Rel,WordNet,Model,Overlap):-
   atomic_list_concat([Dir,'/','modsizedif.txt'],File),
   open(File,write,Stream),
   ( Contradiction=0, !, Prover=contradiction
   ; Proof=0, !, Prover=proof
   ; Prover=unknown ),
   format(Stream,'~p.   % prover output    ~n',[Prover]),
   format(Stream,'~p.   % domain novelty   ~n',[Dom]),
   format(Stream,'~p.   % relation novelty ~n',[Rel]),
   format(Stream,'~p.   % wordnet novelty  ~n',[WordNet]),
   format(Stream,'~p.   % model novelty    ~n',[Model]),
   format(Stream,'~p.   % word overlap     ~n',[Overlap]),
   close(Stream).


/* ========================================================================
   Compute Novelty of H given T
======================================================================== */

computeNovelty(SizeT,SizeH,SizeTH,Novelty):-
   SizeT > 0, SizeH > 0, SizeTH > 0, !,
   Novelty is 1-((SizeTH-SizeT)/SizeH).

computeNovelty(_,_,_,-1).


/* ========================================================================
   MiniWordNet
======================================================================== */

computeMWN(DRS,Dir,File,Size):-   
   option('--wordnet',true), !,
   clearMWN,
   compConcepts(DRS,_),
   compISA,
   addTopMWN,                   %%% this can cause inconsistencies!
%  cutDownMWN,
   sizeMWN(Size),
   outputMWN(Dir,File),
   graphMWN(Dir,File).

computeMWN(_,_,_,0).


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
   format(user_error,'usage: nc [options]~n~n',[]),
   showOptions(nutcracker).

help:-
   option('--help',dont), !.


/* =======================================================================
   Definition of start
========================================================================*/

start:-
   current_prolog_flag(argv,[_Comm|Args]), 
   \+ Args = [],
   set_prolog_flag(float_format,'%.20g'),
   setDefaultOptions(nutcracker), 
   parseOptions(nutcracker,Args),
   shell('chmod 755 src/prolog/nutcracker/startTPandMB.pl', Return),
   Return = 0,
%  catch(load_files(['working/symidf.pl'],[autoload(true),encoding(utf8)]),_,fail),
   main, !,
   halt.

start:- 
   setDefaultOptions(nutcracker), 
   setOption(nutcracker,'--help',do), !,
   help,
   halt.
