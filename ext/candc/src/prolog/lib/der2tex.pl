% lex2tex.pl, by Johan Bos

:- dynamic sem/3, tok/3, freq/2, ex/3, inex/3, pos/2.

/*========================================================================
   File Search Paths
========================================================================*/

file_search_path(semlib,     'src/prolog/lib').
file_search_path(boxer,      'src/prolog/boxer').
file_search_path(knowledge,  'src/prolog/boxer/knowledge').
file_search_path(lex,        'src/prolog/boxer/lex').


/*========================================================================
   Load Modules
========================================================================*/

%:- use_module(boxer(drs2fdrs),[instDrs/1,elimEqDrs/2]).
%:- use_module(boxer(lexicon),[semlex/5,semlex/6]).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).

:- use_module(library(lists),[member/2,append/3,select/3]).

:- use_module(semlib(der),[der/2]).
:- use_module(semlib(options),[option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).


/*========================================================================
   Latex Var
========================================================================*/

printTypedVar(V,Stream):- var(V), !, write(Stream,'X').
printTypedVar(e,Stream):- !, write(Stream,x).
printTypedVar(t,Stream):- !, write(Stream,'B').
printTypedVar(type(e,t),Stream):- !, write(Stream,'p').
printTypedVar(type(type(type(e,t),t),type(type(e,t),t)),Stream):- !, write(Stream,'v').
printTypedVar(type(type(e,t),t),Stream):- !, write(Stream,'n').
printTypedVar(_,Stream):- write(Stream,u).


/*========================================================================
   Add Types
========================================================================*/

%addTypes(Exp,Type,L-L,_):- write(hey:Exp:Type:L),nl,fail.

addTypes('$VAR'(Id),Type,L-L,t(Type,'$VAR'(Id))):-
    member('$VAR'(Id):Type,L), !.

addTypes(merge(B1,B2),t,L1-L3,merge(C1,C2)):- !, 
   addTypes(B1,t,L1-L2,C1),
   addTypes(B2,t,L2-L3,C2).

addTypes(smerge(B1,B2),t,L1-L3,smerge(C1,C2)):- !, 
   addTypes(B1,t,L1-L2,C1),
   addTypes(B2,t,L2-L3,C2).

addTypes(alfa(Type,B1,B2),t,L1-L3,alfa(Type,C1,C2)):- !, 
   addTypes(B1,t,L1-L2,C1),
   addTypes(B2,t,L2-L3,C2).

addTypes(drs([],C1),t,L-L,drs([],C2)):- !,
   addTypesConds(C1,L,C2).

addTypes(drs([_:D|D1],C1),t,L1-L2,drs([t(e,D)|D2],C2)):- !,
   addTypes(drs(D1,C1),t,[D:e|L1]-L2,drs(D2,C2)).

addTypes(lam(X,B1),type(Alfa,Beta),L1-L2,lam(t(Alfa,X),B2)):- !,
   addTypes(B1,Beta,[X:Alfa|L1]-L2,B2).

addTypes(app(E1,F1),Beta,L-L,app(E2,F2)):- !,
   addTypes(E1,type(Alfa,Beta),L-_,E2),
   addTypes(F1,Alfa,L-_,F2).

addTypes(Exp,Type,L-L,_):- !,
   write('%%% unknown (addTypes/4): '), write(Exp), tab(1), write(Type), tab(1), write(L), nl,
   fail.

addTypesConds([],_,[]):- !.

addTypesConds([_:C1|L1],L,[C2|L2]):-
   addTypesCond(C1,L,C2),
   addTypesConds(L1,L,L2).

addTypesCond(pos(B1),L,pos(B2)):- !, addTypes(B1,t,L-_,B2).
addTypesCond(nec(B1),L,nec(B2)):- !, addTypes(B1,t,L-_,B2).
addTypesCond(not(B1),L,not(B2)):- !, addTypes(B1,t,L-_,B2).
addTypesCond(prop(X,B1),L,prop(Y,B2)):- !, addTypeVar(X,L,Y), addTypes(B1,t,L-_,B2).
addTypesCond(or(B1,B2),L,or(B3,B4)):- !, addTypes(B1,t,L-_,B3), addTypes(B2,t,L-_,B4).
addTypesCond(imp(B1,B2),L,imp(B3,B4)):- !, addTypes(B1,t,L-L1,B3), addTypes(B2,t,L1-_,B4).
addTypesCond(whq(B1,B2),L,whq(B3,B4)):- !, addTypes(B1,t,L-L1,B3), addTypes(B2,t,L1-_,B4).
addTypesCond(duplex(X,B1,Y,B2),L,duplex(X,B3,Y,B4)):- !, addTypes(B1,t,L-L1,B3), addTypes(B2,t,L1-_,B4).

addTypesCond(card(X,A,B),L,card(Y,A,B)):- !, addTypeVar(X,L,Y).
addTypesCond(named(X,A,B,C),L,named(Y,A,B,C)):- !, addTypeVar(X,L,Y).
addTypesCond(timex(X,A),L,timex(Y,A)):- !, addTypeVar(X,L,Y).
addTypesCond(eq(X,Y),L,eq(X1,Y1)):- !, addTypeVar(X,L,X1), addTypeVar(Y,L,Y1).
addTypesCond(rel(X,Y,A,B),L,rel(X1,Y1,A,B)):- !, addTypeVar(X,L,X1), addTypeVar(Y,L,Y1).
addTypesCond(role(X,Y,A,B),L,role(X1,Y1,A,B)):- !, addTypeVar(X,L,X1), addTypeVar(Y,L,Y1).
addTypesCond(pred(X,A,B,C),L,pred(Y,A,B,C)):- !, addTypeVar(X,L,Y).

addTypeVar(X,L,t(T,X)):- member(X:T,L), !.
addTypeVar(X,_,t(u,X)).


/*========================================================================
   Latex Lambda Expression (Add Types first)
========================================================================*/

drs2tex(Drs,Type,Stream):-
   addTypes(Drs,Type,[]-_,TypedDrs), !, 
   drs2tex(TypedDrs,Stream).

drs2tex(X,_,Stream):-
   write(Stream,unknown), nl(Stream),
   write(Stream,'%%% unknown (drs2tex/3): '), write(Stream,X), nl(Stream).


/*========================================================================
   Latex DRSs
========================================================================*/

drs2tex(t(Type,'$VAR'(Id)),Stream):- 
   printTypedVar(Type,Stream),
   write(Stream,'$_{'), write(Stream,Id), write(Stream,'}$').

drs2tex(smerge(B1,B2),Stream):- !, 
   drs2tex(merge(B1,B2),Stream).

drs2tex(alfa(_,B1,B2),Stream):- !, 
   write(Stream,'('), drs2tex(B1,Stream), 
   write(Stream,'$\\alpha$'), drs2tex(B2,Stream), write(Stream,')').

drs2tex(merge(B1,B2),Stream):- !, 
   write(Stream,'('), drs2tex(B1,Stream),
   write(Stream,';'), drs2tex(B2,Stream), write(Stream,')').

drs2tex(drs(D,C),Stream):- !,
   write(Stream,'\\drs{'), refs2tex(D,Stream),
   write(Stream,'{'), conds2tex(C,Stream), write(Stream,'}').

drs2tex(lam(X,B),Stream):- !, 
   write(Stream,'$\\lambda$'), drs2tex(X,Stream),
   write(Stream,'.'), drs2tex(B,Stream).

drs2tex(app(B1,B2),Stream):- !, 
   write(Stream,'('), drs2tex(B1,Stream),
   write(Stream,'@'), drs2tex(B2,Stream), write(Stream,')').

drs2tex(X,Stream):- !,
   write(Stream,unknown), nl(Stream),
   write(Stream,'%%% unknown (drs2tex/2): '), write(Stream,X), nl(Stream).


/*========================================================================
   Tex DRS-referents
========================================================================*/

refs2tex([],Stream):- !, write(Stream,'}').

refs2tex([C],Stream):- !, drs2tex(C,Stream), write(Stream,'}').

refs2tex([C|L],Stream):- drs2tex(C,Stream), write(Stream,' '), refs2tex(L,Stream).


/*========================================================================
   Tex DRS-Conditions
========================================================================*/

conds2tex([],_):- !.

conds2tex([C],Stream):- !,
   cond2tex(C,Stream,_), 
   write(Stream,'\\\\[-7pt]').

conds2tex([C|Cs],Stream):-
   cond2tex(C,Stream,N), 
   format(Stream,'\\\\[~ppt]~n',[N]),
   conds2tex(Cs,Stream).


/*========================================================================
   Tex DRS-Condition
========================================================================*/

cond2tex(not(Drs),Stream,9):- !,
   write(Stream,'$\\lnot$'),
   drs2tex(Drs,Stream).

cond2tex(pos(Drs),Stream,9):- !,
   write(Stream,'$\\Diamond$'),
   drs2tex(Drs,Stream).

cond2tex(nec(Drs),Stream,9):- !,
   write(Stream,'$\\Box$'),
   drs2tex(Drs,Stream).
 
cond2tex(prop(X,Drs),Stream,9):- !,
   drs2tex(X,Stream),
   write(Stream,':'),
   drs2tex(Drs,Stream).

cond2tex(or(Drs1,Drs2),Stream,9):- !,
   drs2tex(Drs1,Stream),
   write(Stream,'$\\lor$'),
   drs2tex(Drs2,Stream).

cond2tex(imp(Drs1,Drs2),Stream,9):- !,
   drs2tex(Drs1,Stream),
   write(Stream,'$\\Rightarrow$'),
   drs2tex(Drs2,Stream).

cond2tex(whq(Drs1,Drs2),Stream,9):- !,
   drs2tex(Drs1,Stream),
   write(Stream,'?'),
   drs2tex(Drs2,Stream).

cond2tex(duplex(_,Drs1,_,Drs2),Stream,N):- !, 
   cond2tex(whq(Drs1,Drs2),Stream,N).

cond2tex(card(X,C,_),Stream,1):- !,
   write(Stream,'$|$'),
   drs2tex(X,Stream),
   write(Stream,'$|$ = '),
   write(Stream,C).

cond2tex(named(X,C,_Type,_),Stream,1):- !,
   write(Stream,'nam('),
   drs2tex(X,Stream),
   write(Stream,','),
   printTok(C,Stream),
%   write(Stream,','),
%   write(Stream,Type),
   write(Stream,')').

cond2tex(timex(X,D1),Stream,1):- !,
   timex(D1,D2),
   write(Stream,'time('),
   drs2tex(X,Stream),
   write(Stream,')='),
   write(Stream,D2).

cond2tex(eq(X,Y),Stream,1):-  !,
   drs2tex(X,Stream),
   write(Stream,' = '),
   drs2tex(Y,Stream).

cond2tex(pred(X,Sym,_Type,_Sense),Stream,1):- 
   printTok(Sym,Stream),
   write(Stream,'('),
   drs2tex(X,Stream),
   write(Stream,')').

cond2tex(rel(X,Y,temp_before,_),Stream,1):- !,
   drs2tex(X,Stream), write(Stream,' $<$ '), drs2tex(Y,Stream).

cond2tex(rel(X,Y,temp_included,_),Stream,1):- !,
   drs2tex(X,Stream), write(Stream,' $\\subseteq$ '), drs2tex(Y,Stream).

cond2tex(rel(X,Y,temp_abut,_),Stream,1):- !,
   drs2tex(X,Stream), write(Stream,' $\\supset$\\hspace*{-2pt}$\\subset$ '), drs2tex(Y,Stream).

cond2tex(rel(X,Y,temp_overlap,_),Stream,1):- !,
   drs2tex(X,Stream), write(Stream,' $\\bigcirc$ '), drs2tex(Y,Stream).

cond2tex(role(X,Y,Sym,1),Stream,1):- !, cond2tex(rel(X,Y,Sym,1),Stream,1).

cond2tex(role(X,Y,Sym,-1),Stream,1):- !, cond2tex(rel(Y,X,Sym,1),Stream,1).

cond2tex(rel(X,Y,Sym,_Sense),Stream,1):- !,
   printTok(Sym,Stream),
   write(Stream,'('),
   drs2tex(X,Stream),
   write(Stream,','),
   drs2tex(Y,Stream),
   write(Stream,')').


/*========================================================================
   Time Expressions
========================================================================*/

timex(date(_:_,_:Y,_:M,_:D),Timex):- !, timex(date(Y,M,D),Timex).

timex(date(_:Y,_:M,_:D),Timex):- !, timex(date(Y,M,D),Timex).

timex(time(_:H,_:M,_:S),Timex):- !, timex(time(H,M,S),Timex).

timex(date(Y,M,D),Timex):-
   year(Y,[Y1,Y2,Y3,Y4]),
   month(M,[M1,M2]),
   day(D,[D1,D2]),
   name(Timex,[Y1,Y2,Y3,Y4,M1,M2,D1,D2]).

timex(time(H,M,S),Timex):-
   hour(H,[H1,H2]),
   minute(M,[M1,M2]),
   second(S,[S1,S2]),
   name(Timex,[H1,H2,M1,M2,S1,S2]).

year(Y,C):- var(Y), !, name('XXXX',C).
year(Y,C):- name(Y,C).

month(Y,C):- var(Y), !, name('XX',C).
month(Y,C):- name(Y,C).

day(Y,C):- var(Y), !, name('XX',C).
day(Y,C):- name(Y,C).

hour(A,C):- day(A,C).
minute(A,C):- day(A,C).
second(A,C):- day(A,C).


/*========================================================================
   Header
========================================================================*/

printHeader(Stream):-
   write(Stream,'\\documentclass[10pt]{article}'), nl(Stream),
   nl(Stream),
   write(Stream,'\\usepackage{a4wide}'),           nl(Stream),
   write(Stream,'\\usepackage{rotating}'),         nl(Stream),
   write(Stream,'\\usepackage{latexsym}'),         nl(Stream),
   write(Stream,'\\usepackage{hyperref}'),         nl(Stream),
   nl(Stream),
   write(Stream,'\\newcommand{\\drs}[2]'),         nl(Stream),
   write(Stream,'{'),                              nl(Stream),
   write(Stream,'   \\begin{tabular}{|l|}'),       nl(Stream),
   write(Stream,'   \\hline'),                     nl(Stream),
   write(Stream,'   #1'),                          nl(Stream),
   write(Stream,'   \\\\'),                        nl(Stream),
   write(Stream,'   ~ \\vspace{-2ex} \\\\'),       nl(Stream),
   write(Stream,'   \\hline'),                     nl(Stream),
   write(Stream,'   ~ \\vspace{-2ex} \\\\'),       nl(Stream),
   write(Stream,'   #2'),                          nl(Stream),
   write(Stream,'   \\\\'),                        nl(Stream),
   write(Stream,'   \\hline'),                     nl(Stream),
   write(Stream,'   \\end{tabular}'),              nl(Stream),
   write(Stream,'}'),                              nl(Stream),
   nl(Stream),
   write(Stream,'\\parindent 0pt'),                nl(Stream),
   write(Stream,'\\parskip 7pt'),                  nl(Stream),
   write(Stream,'\\tabcolsep 1pt'),                nl(Stream),
   write(Stream,'%\\pdfpageheight 250mm'),         nl(Stream),
   write(Stream,'%\\pdfpagewidth  700mm'),         nl(Stream),
   write(Stream,'\\textheight 240mm'),             nl(Stream),
   write(Stream,'\\textwidth 184mm'),              nl(Stream),
   write(Stream,'\\topmargin -12mm'),              nl(Stream),
   write(Stream,'\\oddsidemargin -10mm'),          nl(Stream),
   write(Stream,'\\evensidemargin -10mm'),         nl(Stream),
   write(Stream,'\\pagestyle{myheadings}'),        nl(Stream), nl(Stream),
   write(Stream,'\\makeindex  %% makeindex derivation.idx'),   nl(Stream),
   write(Stream,'\\title{A large-scale, formal semantic lexicon (appendix)}'), nl(Stream),
   write(Stream,'\\author{Johan Bos}'),            nl(Stream),
   write(Stream,'\\begin{document}'),              nl(Stream),
   write(Stream,'\\maketitle'),                    nl(Stream),   
   write(Stream,'\\thispagestyle{empty}'),         nl(Stream),
   write(Stream,'%\\tableofcontents'),             nl(Stream),
   write(Stream,'\\input{intro}'),                 nl(Stream),
   nl(Stream).

printFooter(Stream):-
   write(Stream,'\\markright{\\rm Appendix: Index \\hfill Page~}'), nl(Stream),
   write(Stream,'\\input{derivation.ind}'),        nl(Stream),
   write(Stream,'\\end{document}'),                nl(Stream).   


/*========================================================================
   Cat2Type
========================================================================*/

cat2type(t:_,    t):- !.
cat2type(n,      type(e,t)):- !.
cat2type(n:_,    type(e,t)):- !.
cat2type(pp,     type(e,t)):- !.
cat2type(np,     type(type(e,t),t)):- !.
cat2type(np_exp, type(type(e,t),t)):- !.
cat2type(np_thr, type(type(e,t),t)):- !.
cat2type(s:_,    type(type(e,t),t)):- !.
cat2type(comma,  type(t,type(t,t))):- !.
cat2type(semi,   type(t,type(t,t))):- !.
cat2type(conj,   type(t,type(t,t))):- !.

cat2type(conj:app, type(X,type(X,X))):- !, cat2type(np,X).

cat2type('\\'(A,B), type(BType,AType) ):- !, cat2type(A,AType), cat2type(B,BType).
cat2type('/'(A,B), type(BType,AType) ):- !, cat2type(A,AType), cat2type(B,BType).


/*========================================================================
   Clean up CCG Cat
========================================================================*/

cleanCat('\\'(A1,B1), '\\'(A2,B2) ):- !, cleanCat(A1,A2), cleanCat(B1,B2).
cleanCat('/'(A1,B1),  '/'(A2,B2)  ):- !, cleanCat(A1,A2), cleanCat(B1,B2).
cleanCat(n:_,n):- !.
cleanCat(comma,conj):- !.
cleanCat(semi,conj):- !.
%cleanCat(conj:_,conj):- !.
cleanCat(X,X).


/*========================================================================
   Typeset Category
========================================================================*/

printCat('\\'(A,B),Stream):- !,
   write(Stream,'('),
   printCat(A,Stream),
   write(Stream,'$\\backslash$'),
   printCat(B,Stream),
   write(Stream,')').

printCat('/'(A,B),Stream):- !,
   write(Stream,'('),
   printCat(A,Stream),
   write(Stream,'/'),
   printCat(B,Stream),
   write(Stream,')').

printCat(np_thr,Stream):- !, write(Stream,'NP[thr]').

printCat(np_exp,Stream):- !, write(Stream,'NP[exp]').

printCat(comma,Stream):- !, write(Stream,'CONJ').

printCat(semi,Stream):- !, write(Stream,'CONJ').

printCat(n:_,Stream):- !,
   write(Stream,'N').

printCat(Cat:Fea,Stream):- 
   var(Fea), !, Fea = 'X', 
   printCat(Cat:Fea,Stream).

printCat(Cat:Fea,Stream):- 
   atom(Cat), atom(Fea), !,
   upcase_atom(Cat,Up),
   write(Stream,Up), write(Stream,'['),
   write(Stream,Fea), write(Stream,']').

printCat(Cat,Stream):- 
   atom(Cat), !,
   upcase_atom(Cat,Up),
   write(Stream,Up).

printCat(Cat,Stream):- 
   write(Stream,Cat).


/*========================================================================
   Typeset Semantic Type
========================================================================*/

printType(type(A,B),Stream):- !,
   write(Stream,'$\\langle$'),
   printType(A,Stream),
   write(Stream,','),
   printType(B,Stream),
   write(Stream,'$\\rangle$').

printType(A,Stream):- write(Stream,A).


/*========================================================================
   Typeset Sentence
========================================================================*/

printExample(Id,Stream,W):-
  ex(Id,Ex,_),
  write(Stream,'('), write(Stream,Id), write(Stream,') '),
  printSentence(Ex,Stream,W).

printSentence([W|Words],Stream,W):- !,
   write(Stream,'\\textrm{\\underline{'),
   printTok(W,Stream), write(Stream,'}'),
   printSentence1(Words,Stream,W).

printSentence([X|Words],Stream,W):-
   write(Stream,'\\textrm{'),
   printTok(X,Stream),
   printSentence1(Words,Stream,W).

printSentence1([],Stream,_):- 
   write(Stream,'}'), nl(Stream).

printSentence1([W|L],Stream,W):- 
   member(W,['.', ',', '?', '\'re', '\'s', '\'ve', 'n\'t', '%']), !,
   write(Stream,'\\underline{'), printTok(W,Stream), write(Stream,'}'),
   printSentence1(L,Stream,W).

printSentence1([W|L],Stream,W):- !,
   write(Stream,' \\underline{'), printTok(W,Stream), write(Stream,'}'),
   printSentence1(L,Stream,W).

printSentence1([X|L],Stream,W):- 
   member(X,['.', ',', '?', '\'re', '\'s', '\'ve', 'n\'t', '%']), !,
   printTok(X,Stream), 
   printSentence1(L,Stream,W).

printSentence1([X|L],Stream,W):-
   write(Stream,' '),
   printTok(X,Stream), 
   printSentence1(L,Stream,W).


/*========================================================================
   Typeset Tokens
========================================================================*/

printTokens(Id,Stream):-
   findall(t(F,Tok),tok(Id,Tok,F),L),
   sort(L,Sorted), reverse(Sorted,Ordered),
   printTok(Ordered,20,Stream).

printTok([t(F,X)|_],1,Stream):- !,
   printIndex(X,Stream),
   write(Stream,'\\textrm{'), printTok(X,Stream), write(Stream,'}$^{('),
   write(Stream,F), write(Stream,')}$.'),
   nl(Stream).

printTok([t(F,X)],_,Stream):- !,
   printIndex(X,Stream),
   write(Stream,'\\textrm{'), printTok(X,Stream),  write(Stream,'}$^{('),
   write(Stream,F), write(Stream,')}$.'),
   nl(Stream).

printTok([t(F,X)|L],N,Stream):- !,
   printIndex(X,Stream),
   write(Stream,'\\textrm{'), printTok(X,Stream),  write(Stream,'}$^{('),
   write(Stream,F), write(Stream,')}$, '),
   M is N - 1,
   printTok(L,M,Stream).


printIndex(Tok,_):-
   atom(Tok),
   atom_chars(Tok,Chars),
   special(Special),
   member(Special,Chars), !.

printIndex(Tok,Stream):-
   write(Stream,'\\index{\\textrm{'), printTok(Tok,Stream), write(Stream,'}}').


printTok(Tok,Stream):-
   atom(Tok),
   atom_chars(Tok,Chars),
   special(Special),
   member(Special,Chars), !,
   printChars(Chars,Stream).

printTok(Tok,Stream):-
   write(Stream,Tok).

printToks([],_):- !.
printToks([X],Stream):- !, printTok(X,Stream).
printToks([X|L],Stream):- !, printTok(X,Stream), write(Stream,' '), printToks(L,Stream).

printChars([],_).
printChars([Char|L],Stream):- special(Char), !, write(Stream,'\\'), write(Stream,Char), printChars(L,Stream).
printChars([X|L],Stream):- !, write(Stream,X), printChars(L,Stream).


/*========================================================================
   Special Characters (for Latex)
========================================================================*/

special('$').
special('%').
special('&').
special('}').
special('{').
special('#').
special('_').


/*========================================================================
   Compute Frequency
========================================================================*/

computeF:-
   sem(_,_,Id),
   \+ freq(Id,_), !,
   findall(F,tok(Id,_,F),L),
   sum(L,0,Sum),
   assert(freq(Id,Sum)),
   computeF.

computeF.

sum([],Sum,Sum).

sum([X|L],Acc,Sum):-
   New is Acc+X,
   sum(L,New,Sum).


/*========================================================================
   Print Derivation
========================================================================*/

printDer(Comb,Stream,Tok3):- 
   ( Comb = fa(Cat,Sem,L,R), !, Rule = '[$>$]'
   ; Comb = fc(Cat,Sem,L,R), !, Rule = '[FC]'
   ; Comb = gfc(Cat,Sem,L,R), !, Rule = '[GFC]'
   ; Comb = gbc(Cat,Sem,L,R), !, Rule = '[GBC]'
   ; Comb = bc(Cat,Sem,L,R), !, Rule = '[BC]'
   ; Comb = fs(Cat,Sem,L,R), !, Rule = '[FS]'
   ; Comb = bs(Cat,Sem,L,R), !, Rule = '[BS]'
   ; Comb = conj(Cat,Sem,L,R), !, Rule = '[CONJ]'
   ; Comb = bxc(Cat,Sem,L,R), !, Rule = '[BXC]'
   ; Comb = fxc(Cat,Sem,L,R), !, Rule = '[FXC]'
   ; Comb = gbxc(Cat,Sem,L,R), !, Rule = '[GBXC]'
   ; Comb = gfxc(Cat,Sem,L,R), !, Rule = '[GFXC]'
   ; Comb = fxs(Cat,Sem,L,R), !, Rule = '[BXS]'
   ; Comb = bxs(Cat,Sem,L,R), !, Rule = '[FXS]'
   ; Comb = ba(Cat,Sem,L,R), !, Rule = '[$<$]' ), 
   write(Stream,'\\begin{tabular}[b]{ll}'), nl(Stream),
   printDer(L,Stream,Tok1), write(Stream,'&'),
   printDer(R,Stream,Tok2), write(Stream,'\\\\'), nl(Stream),
   append(Tok1,Tok2,Tok3),
   write(Stream,'\\multicolumn{2}{c}{\\hrulefill '), write(Stream,Rule), write(Stream,'}\\\\'), nl(Stream),
   betaConvert(Sem,Red),
   cat2type(Cat,Type),
   numbervars(Red,1,_),
   write(Stream,'\\multicolumn{2}{c}{\\begin{tabular}[b]{l}'),
   write(Stream,'\\textbf{'), printToks(Tok3,Stream), write(Stream,'}\\\\'), nl(Stream), 
   printCat(Cat,Stream), write(Stream,'\\\\'), nl(Stream), 
   printType(Type,Stream), write(Stream,'\\\\'), nl(Stream), 
%   format(Stream,'%%%~p~n',[Red]),
   drs2tex(Red,Type,Stream), 
   !,
   write(Stream,'\\end{tabular}}'), nl(Stream),
   write(Stream,'\\end{tabular}'), nl(Stream).

printDer(Comb,Stream,Tok):- 
   ( Comb = tc(Cat,Sem,T), !, Rule = '[TC]'
   ; Comb = tr(Cat,Sem,T), !, Rule = '[TR]' ),
   write(Stream,'\\begin{tabular}[b]{c}'), nl(Stream),
   printDer(T,Stream,Tok), write(Stream,'\\\\'), nl(Stream),
   write(Stream,'\\hrulefill '), write(Stream,Rule), write(Stream,'\\\\'), nl(Stream),
   betaConvert(Sem,Red),
   cat2type(Cat,Type),
   numbervars(Red,1,_),
   write(Stream,'\\begin{tabular}[b]{l}'), nl(Stream),
   write(Stream,'\\textbf{'), printToks(Tok,Stream), write(Stream,'}\\\\'), nl(Stream), 
   printCat(Cat,Stream), write(Stream,'\\\\'), nl(Stream), 
   printType(Type,Stream), write(Stream,'\\\\'), nl(Stream), 
%   format(Stream,'%%%~p~n',[Red]),
   drs2tex(Red,Type,Stream), 
   write(Stream,'\\\\'), nl(Stream), 
   !,
   write(Stream,'\\end{tabular}'), nl(Stream),
   write(Stream,'\\end{tabular}'), nl(Stream).

printDer(t(Sem,Cat,Tok,_Pos),Stream,[Tok]):- 
   cat2type(Cat,Type),  
   betaConvert(Sem,Red),
   numbervars(Red,1,_),
   write(Stream,'\\begin{tabular}[b]{l}'), nl(Stream),
   write(Stream,'\\textbf{'), printTok(Tok,Stream), write(Stream,'}\\\\'), nl(Stream), 
   printCat(Cat,Stream), write(Stream,'\\\\'), nl(Stream), 
   printType(Type,Stream), write(Stream,'\\\\'), nl(Stream), 
   drs2tex(Red,Type,Stream), 
   write(Stream,'\\\\'), nl(Stream), 
   !, 
   write(Stream,'\\end{tabular}'), nl(Stream).

printDer(X,Stream,[]):- !,
   write(Stream,unknown), nl(Stream),
   write(Stream,'%%% unknown (printDer): '),
   write(Stream,X),nl(Stream),nl(Stream).


/*========================================================================
   Analysis of Derivation
========================================================================*/

analyse(t(Sem,Cat,Tok,Pos),Index,[Tok|L]-L):- !,
   cleanCat(Cat,CleanCat),
    (    member(Pos,['NNP','NNPS']), Sym = Tok
    ; \+ member(Pos,['NNP','NNPS']), downcase_atom(Tok,Sym) ),
   betaConvert(Sem,Converted),
   add(CleanCat,Sym,Tok,Pos,Converted,Index).

analyse(Tree,I,L1-L3):-
   Tree =.. [_Name,_Cat,_Sem,T1,T2], !,
   analyse(T1,I,L1-L2),
   analyse(T2,I,L2-L3).

analyse(Tree,I,L1-L2):-
   Tree =.. [_Name,_Cat,_Sem,T], !,
   analyse(T,I,L1-L2).

analyse(Tree,I,L1-L4):-
   Tree =.. [_Name,_Cat,_Sem,T1,T2,T3], !,
   analyse(T1,I,L1-L2),
   analyse(T2,I,L2-L3),
   analyse(T3,I,L3-L4).


/*========================================================================
   Derivations
========================================================================*/

derivations:-
   der(N,Der), 
   analyse(Der,N,S-[]), 
   length(S,Len), 
   assert(ex(N,S,Len)),
   fail.

derivations:- 
   computeF, !.


/*========================================================================
   Print Derivations
========================================================================*/

printDerivations(Stream):-
   der(N,Der), 
   \+ blocked(N),
   minlen(MinLen),
   maxlen(MaxLen),
   ex(N,S,Len), Len < MaxLen, Len > MinLen,
   write(N:Len:S), nl,
   write(Stream,'\\clearpage'), nl(Stream),
   write(Stream,'\\begin{sidewaystable}\\scriptsize'), nl(Stream),
   printDer(Der,Stream,_),
   format(Stream,'\\caption{\\label{ex:~p}',[N]),
   printToks(S,Stream), format(Stream,' (~p)}~n',[N]),
   write(Stream,'\\end{sidewaystable}'), nl(Stream),
   fail.

printDerivations(Stream):- 
   nl(Stream).


/*========================================================================
   Add to temp database
========================================================================*/

add(Cat,Sym,Tok,Pos,X1,Sen):-                      
   sem(Cat,X2,Id),                       
   similar(X1,X2), !,                 % similar sem present
   assert(inex(Id,Sen,Tok:Sym)),
   (  pos(Id,Pos), !
   ;  assert(pos(Id,Pos))
   ),
   (  tok(Id,Sym,M), !,               % tok already present
      retract(tok(Id,Sym,M)),
      N is M + 1,
      assert(tok(Id,Sym,N)),
      ( tok(Id,_,Higher), Higher > N, !
      ; retract(sem(Cat,_,Id)),
        assert(sem(Cat,X1,Id))
      )
   ;  assert(tok(Id,Sym,1))           % new tok
   ).
   
add(Cat,Sym,Tok,Pos,X,Sen):-          % sem new, but
   \+ sem(_,_,_), !,                  % nothing in database yet
   assert(sem(Cat,X,1)),
   assert(inex(1,Sen,Tok:Sym)),
   assert(pos(1,Pos)),
   assert(tok(1,Sym,1)).

add(Cat,Sym,Tok,Pos,X,Sen):-          % sem new
   sem(_,_,I),                        % get new id
   \+ (sem(_,_,J), J > I), !,         % increase highest previous id
   N is I + 1,
   assert(sem(Cat,X,N)),
   assert(inex(N,Sen,Tok:Sym)),
   assert(pos(N,Pos)),
   assert(tok(N,Sym,1)).


/*========================================================================
   Similar Semantics
========================================================================*/

similar(X,Y):- var(X), !, var(Y).
similar(X,Y):- var(Y), !, var(X).
similar('$VAR'(X),'$VAR'(X)).

similar(lam(_,A),      lam(_,B)):-      similar(A,B).
similar(app(A1,A2),    app(B1,B2)):-    similar(A1,B1), similar(A2,B2).
similar(smerge(A1,A2), smerge(B1,B2)):- similar(A1,B1), similar(A2,B2).
similar(merge(A1,A2),  merge(B1,B2)):-  similar(A1,B1), similar(A2,B2).
similar(alfa(T,A1,A2), alfa(T,B1,B2)):- similar(A1,B1), similar(A2,B2).

similar(drs(D1,C1), drs(D2,C2)):- length(D1,Len), length(D2,Len), similar(C1,C2).

similar([],[]).
similar([_:C1|L1],[_:C2|L2]):- similar(C1,C2), similar(L1,L2).

similar(not(A),    not(B)):-    similar(A,B).
similar(pos(A),    pos(B)):-    similar(A,B).
similar(nec(A),    nec(B)):-    similar(A,B).
similar(prop(_,A), prop(_,B)):- similar(A,B).

similar(imp(A1,A2),     imp(B1,B2)):-     similar(A1,B1), similar(A2,B2).
similar(or(A1,A2),      or(B1,B2)):-      similar(A1,B1), similar(A2,B2).
similar(whq(A1,A2),     whq(B1,B2)):-     similar(A1,B1), similar(A2,B2).
similar(duplex(_,A1,_,A2), duplex(_,B1,_,B2)):- similar(A1,B1), similar(A2,B2).

similar(card(_,_,_),    card(_,_,_)).
similar(timex(_,_),     timex(_,_)).
similar(eq(_,_),        eq(_,_)).
similar(pred(_,_,T,_),  pred(_,_,T,_)).
similar(rel(_,_,_,_),   rel(_,_,_,_)).
similar(role(_,_,_,_),  role(_,_,_,_)).
similar(named(_,_,_,_), named(_,_,_,_)).



/*========================================================================
   Blocked examples
========================================================================*/

blocked(201).
blocked(211).
blocked(382).
blocked(472).
blocked(522).
blocked(526).
blocked(576).
blocked(579).
blocked(616).
blocked(606).
blocked(638).
blocked(743).
blocked(772).
blocked(830).
blocked(1033).
blocked(1036).
blocked(1148).
blocked(1321).
blocked(1328).
blocked(1342).
blocked(1353).
blocked(1424).
blocked(1449).
blocked(1499).
blocked(1532).
blocked(1540).
blocked(1709).

/*========================================================================
   Min Length for Example
========================================================================*/

minlen(3).
maxlen(10).

/*========================================================================
   Get best example
========================================================================*/

getExample(Id,ExId,Tok,Len):-
   minlen(Min),
   inex(Id,ExId,Tok:_),
   \+ blocked(ExId),
   ex(ExId,Ex,Len), Len > Min, 
   \+ ( select(Tok,Ex,NewEx), member(Tok,NewEx) ),
   \+ ( inex(Id,ExId1,_), \+ blocked(ExId1), ex(ExId1,_,Len1), Len1 > Min, Len1 < Len).

getExample(Id,ExId,Tok,Len):-
   minlen(Min),
   inex(Id,ExId,Tok:_),
   \+ blocked(ExId),
   ex(ExId,_,Len), Len > Min,
   \+ ( inex(Id,ExId1,_), \+ blocked(ExId), ex(ExId1,_,Len1), Len1 > Min, Len1 < Len).

getExample(Id,ExId,Tok,Len):-
   inex(Id,ExId,Tok:_),
   ex(ExId,_,Len),
   \+ blocked(ExId),
   \+ ( inex(Id,ExId1,_), \+ blocked(ExId), ex(ExId1,_,Len1), Len1 < Len).

getExample(Id,ExId,Tok,Len):-
   inex(Id,ExId,Tok:_),
   ex(ExId,_,Len),
   \+ ( inex(Id,ExId1,_), ex(ExId1,_,Len1), Len1 < Len).


/*========================================================================
   Main
========================================================================*/

body([],_,Stream):- !, nl(Stream).

body([s(F,Id)|L1],N1,Stream):-
   F > 0,
   sem(Cat,X,Id), 
   cat2type(Cat,Type),

   Remainder is mod(N1,2),
   ( Remainder = 1, write(Stream,'\\clearpage'), nl(Stream)
   ; Remainder = 0 ),

%   write(Stream,'\\addcontentsline{toc}{subsection}{'),
   write(Stream,'\\index{\\textsf{'),
   printCat(Cat,Stream),    
   write(Stream,'}}'),nl(Stream),

   write(Stream,'\\begin{tabular}[t]{rl}'), nl(Stream),
   write(Stream,'\\textbf{Category}: & \\textsf{'),
   printCat(Cat,Stream),    
   write(Stream,'}\\\\[7pt]'), nl(Stream),

   write(Stream,'\\textbf{Type}: & '),
   printType(Type,Stream),   
   write(Stream,'\\\\[7pt]'), nl(Stream), 

   write(Stream,'\\textbf{PoS}: & {\\small '),
   findall(POS,pos(Id,POS),POSs),
   printToks(POSs,Stream),    
   write(Stream,'}\\\\[7pt]'), nl(Stream), 

   write(Stream,'\\textbf{Semantics}: & \\\\'), nl(Stream),        
   format(Stream,'\\end{tabular}\\hfill F=~p~n~n',[F]),

   numbervars(X,1,_),
   format(Stream,'%%%~p~n',[X]), 
   write(Stream,'\\textsf{'), 
   drs2tex(X,Type,Stream), 
   write(Stream,'}'), nl(Stream), nl(Stream), 

   write(Stream,'\\textbf{Most frequent lexical entries}:\\\\[2pt]'), nl(Stream),
   printTokens(Id,Stream),      nl(Stream), nl(Stream),        

   write(Stream,'\\textbf{Corpus examples}:\\\\[2pt]'),   nl(Stream),
   printExample1(Stream,Id,Ex),
   printExample2(Stream,Id,Ex),

   write(Stream,'\\vfill'),
   nl(Stream), nl(Stream), !,

   ( select(s(F2,Id2),L1,L2), sem(Cat,_,Id2), !, L3=[s(F2,Id2)|L2] 
   ; L3 = L1 ),
   N2 is N1 + 1, 
   body(L3,N2,Stream).

body([_|L],N,Stream):- !,
   body(L,N,Stream).


/*========================================================================
   Print Page Ref
========================================================================*/

printPageRef(Len,Stream,N):-
   \+ blocked(N),
   maxlen(MaxLen),
   minlen(MinLen),
   Len < MaxLen, 
   Len > MinLen, !, 
   format(Stream,'(see p. \\pageref{ex:~p})~n~n',[N]).

printPageRef(_,Stream,_):-
   nl(Stream).


/*========================================================================
   Print Example 1
========================================================================*/

printExample1(Stream,Id,N):-
   tok(Id,Sym,F), 
   \+ (tok(Id,_,Higher), Higher > F), 
   inex(Id,N,Tok:Sym), 
   ex(N,_,Len), Len < 18, Len > 4,
   printExample(N,Stream,Tok), !,
   printPageRef(Len,Stream,N).

printExample1(_,_,0).


/*========================================================================
   Print Example 2
========================================================================*/

printExample2(Stream,Id,N):-
   getExample(Id,Ex,Tok,Len), \+ Ex=N,
   printExample(Ex,Stream,Tok), !,
   printPageRef(Len,Stream,Ex).

printExample2(Stream,_,_):- 
   nl(Stream).


/*========================================================================
   Start
========================================================================*/

go:-
   setDefaultOptions(boxer), 
   setOption(boxer,'--warnings',true),
   setOption(boxer,'--roles',verbnet),

   derivations,
   findall(s(F,Id),freq(Id,F),All),
   sort(All,Sorted),
   reverse(Sorted,Ordered),

   open('working/doc/derivation.tex',write,Stream),

   printHeader(Stream),
   write(Stream,'\\markright{\\rm Appendix: Lexicon \\hfill Page~}'),  nl(Stream),
   write(Stream,'\\addcontentsline{toc}{section}{2. Lexicon}'),        nl(Stream),
   body(Ordered,1,Stream),
   write(Stream,'\\clearpage'),   nl(Stream),
   write(Stream,'\\markright{\\rm Appendix: Derivations \\hfill Page~}'), nl(Stream),
   write(Stream,'\\addcontentsline{toc}{section}{3. Derivations}'),       nl(Stream),
   printDerivations(Stream),
   printFooter(Stream),
   close(Stream).

:- go.

