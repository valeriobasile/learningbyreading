% lex2tex.pl, by Johan Bos

:- dynamic sem/3, tok/3, freq/2, ex/4.

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

:- use_module(boxer(drs2fdrs),[instDrs/1,elimEqDrs/2]).
:- use_module(boxer(lexicon),[semlex/5,semlex/6]).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).

:- use_module(library(lists),[member/2]).

:- use_module(semlib(semlex),[lexcall/6]).
:- use_module(semlib(drs2fol),[symbol/4]).
:- use_module(semlib(options),[option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).


/*========================================================================
   Latex Var
========================================================================*/

printTypedVar(V,Stream):- var(V), !, write(Stream,'X').
printTypedVar(e,Stream):- !, write(Stream,x).
printTypedVar(type(e,t),Stream):- !, write(Stream,'p').
printTypedVar(type(type(type(e,t),t),type(type(e,t),t)),Stream):- !, write(Stream,'v').
printTypedVar(type(type(e,t),t),Stream):- !, write(Stream,'n').
printTypedVar(_,Stream):- write(Stream,u).


/*========================================================================
   Latex DRSs
========================================================================*/

drs2tex('$VAR'(Id),Type,L-L,Stream):- 
   member('$VAR'(Id):Type,L), !,
   printTypedVar(Type,Stream),
   write(Stream,'$_{'),
   write(Stream,Id),
   write(Stream,'}$').

drs2tex('$VAR'(Id),_Type,L-L,Stream):- !,
   write(Stream,'U'),
   write(Stream,'$_{'),
   write(Stream,Id),
   write(Stream,'}$').
   
drs2tex(smerge(B1,B2),T,L,Stream):- !, drs2tex(merge(B1,B2),T,L,Stream).

drs2tex(alfa(_,B1,B2),t,L1-L3,Stream):- !, 
   write(Stream,'('),
   drs2tex(B1,t,L1-L2,Stream),
   write(Stream,'$\\alpha$'),
   drs2tex(B2,t,L2-L3,Stream),
   write(Stream,')').

drs2tex(merge(B1,B2),t,L1-L3,Stream):- !, 
   write(Stream,'('),
   drs2tex(B1,t,L1-L2,Stream),
   write(Stream,';'),
   drs2tex(B2,t,L2-L3,Stream),
   write(Stream,')').

drs2tex(drs(D,C),t,L1-L2,Stream):- !,
   write(Stream,'\\drs{'),
   refs2tex(D,L1-L2,Stream),
   write(Stream,'{'),
   conds2tex(C,L2,Stream),
   write(Stream,'}').

drs2tex(lam(X,B),type(TX,T),L1-L2,Stream):- !, 
% write('lam ':L1),nl,
% write(type(TX,T):lam(X,B)),nl,nl,
   write(Stream,'$\\lambda$'),
   drs2tex(X,TX,[X:TX|L1]-L2,Stream),
   write(Stream,'.'),
   drs2tex(B,T,L2-_,Stream).

drs2tex(app(B1,B2),T2,L1-L1,Stream):- !, 
% write('app ':L1),nl,
% write(T2:app(B1,B2)),nl,nl,
   write(Stream,'('),
   drs2tex(B1,type(T1,T2),L1-_,Stream),
   write(Stream,' @ '),
   drs2tex(B2,T1,L1-_,Stream),
   write(Stream,')').

drs2tex(X,_Type,L-L,Stream):- 
   write(Stream,unknown(X)).

/*========================================================================
   Tex DRS-referents
========================================================================*/

refs2tex([],L-L,Stream):- !,
   write(Stream,'}').

refs2tex([_:C],L1-L2,Stream):- !,
   drs2tex(C,e,[C:e|L1]-L2,Stream),
   write(Stream,'}').

refs2tex([_:C|L],L1-L3,Stream):-
   drs2tex(C,e,[C:e|L1]-L2,Stream),
   write(Stream,' '),
   refs2tex(L,L2-L3,Stream).


/*========================================================================
   Tex DRS-Conditions
========================================================================*/

conds2tex([],_,_):- !.

conds2tex([_:C],L,Stream):- !,
   cond2tex(C,L,Stream,_), 
   write(Stream,'\\\\[-3pt]').

conds2tex([_:C|Cs],L,Stream):-
   cond2tex(C,L,Stream,N), 
   format(Stream,'\\\\[~ppt]~n',[N]),
   conds2tex(Cs,L,Stream).


/*========================================================================
   Tex DRS-Condition
========================================================================*/

cond2tex(not(Drs),L,Stream,9):- !,
   write(Stream,'$\\lnot$'),
   drs2tex(Drs,t,L-_,Stream).

cond2tex(nec(Drs),L,Stream,9):- !,
   write(Stream,'$\\Diamond$'),
   drs2tex(Drs,t,L-_,Stream).

cond2tex(pos(Drs),L,Stream,9):- !,
   write(Stream,'$\\Box$'),
   drs2tex(Drs,t,L-_,Stream).
 
cond2tex(prop(X,Drs),L,Stream,9):- !,
   drs2tex(X,e,L-_,Stream),
   write(Stream,':'),
   drs2tex(Drs,t,L-_,Stream).

cond2tex(or(Drs1,Drs2),L,Stream,9):- !,
   drs2tex(Drs1,t,L-_,Stream),
   write(Stream,'$\\lor$'),
   drs2tex(Drs2,t,L-_,Stream).

cond2tex(imp(Drs1,Drs2),L,Stream,9):- !,
   drs2tex(Drs1,t,L-L1,Stream),
   write(Stream,'$\\Rightarrow$'),
   drs2tex(Drs2,t,L1-_,Stream).

cond2tex(whq(Drs1,Drs2),L,Stream,9):- !,
   drs2tex(Drs1,t,L-L1,Stream),
   write(Stream,'?'),
   drs2tex(Drs2,t,L1-_,Stream).

cond2tex(duplex(_,Drs1,_,Drs2),L,Stream,N):- !, 
   cond2tex(whq(Drs1,Drs2),L,Stream,N).

cond2tex(card(X,C,_),L,Stream,1):- !,
   write(Stream,'$|$'),
   drs2tex(X,e,L-_,Stream),
   write(Stream,'$|$ = '),
   write(Stream,C).

cond2tex(named(X,C,Type,Sense),L,Stream,1):- !,
   sym2tex(C,Type,Sense,New), !,
   write(Stream,'named('),
   drs2tex(X,e,L-_,Stream),
   write(Stream,','),
   write(Stream,New),
   write(Stream,')').

cond2tex(timex(X,D1),L,Stream,1):- !,
   sym2tex(D1,t,_Sense,D2),
   write(Stream,D2),
   write(Stream,'('),
   drs2tex(X,e,L-_,Stream),
   write(Stream,')').

cond2tex(eq(X,Y),L,Stream,1):-  !,
   drs2tex(X,e,L-_,Stream),
   write(Stream,' = '),
   drs2tex(Y,e,L-_,Stream).

cond2tex(pred(X,Sym1,Type,Sense),L,Stream,1):- 
   sym2tex(Sym1,Type,Sense,Sym2), !,
   write(Stream,Sym2),
   write(Stream,'('),
   drs2tex(X,e,L-_,Stream),
   write(Stream,')').

cond2tex(rel(X,Y,Sym1,Sense),L,Stream,1):- 
   sym2tex(Sym1,r,Sense,Sym2), !,
   write(Stream,Sym2),
   write(Stream,'('),
   drs2tex(X,e,L-_,Stream),
   write(Stream,','),
   drs2tex(Y,e,L-_,Stream),
   write(Stream,')').

cond2tex(role(X,Y,Sym1,1),L,Stream,1):- 
   sym2tex(Sym1,r,1,Sym2), !,
   write(Stream,Sym2),
   write(Stream,'('),
   drs2tex(X,e,L-_,Stream),
   write(Stream,','),
   drs2tex(Y,e,L-_,Stream),
   write(Stream,')').

cond2tex(role(X,Y,Sym1,-1),L,Stream,1):- 
   sym2tex(Sym1,r,1,Sym2), !,
   write(Stream,Sym2),
   write(Stream,'('),
   drs2tex(Y,e,L-_,Stream),
   write(Stream,','),
   drs2tex(X,e,L-_,Stream),
   write(Stream,')').


/*========================================================================
   Tex non-logical sym2texs
========================================================================*/

sym2tex(Sym1,Type,Sense,Sym3):-
   symbol(Type,Sym1,Sense,Sym2),
   name(Sym2,Codes2),
   underscore(Codes2,Codes3),
   name(Sym3,Codes3).


/*========================================================================
   Deal with underscores
========================================================================*/

underscore([],[]).

underscore([95|L1],[92,95|L2]):- !, underscore(L1,L2).

underscore([X|L1],[X|L2]):- !, underscore(L1,L2).


/*========================================================================
   Header
========================================================================*/

printHeader(Stream):-
   write(Stream,'\\documentclass[10pt]{article}'), nl(Stream),
   nl(Stream),
   write(Stream,'\\usepackage{a4wide}'),           nl(Stream),
   nl(Stream),
   write(Stream,'\\newcommand{\\drs}[2]'),         nl(Stream),
   write(Stream,'{  '),                        nl(Stream),
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
   write(Stream,'\\parskip 10pt'),                 nl(Stream),
   nl(Stream),
   write(Stream,'\\makeindex'),                    nl(Stream),
   write(Stream,'\\begin{document}'),              nl(Stream),
   nl(Stream).

printFooter(Stream):-
   write(Stream,'\\input{synsem.ind}'),            nl(Stream),
   write(Stream,'\\end{document}'),                nl(Stream).   


/*========================================================================
   Cat2Type
========================================================================*/

cat2type(t,      t):- !.
cat2type(n,      type(e,t)):- !.
cat2type(n:_,    type(e,t)):- !.
cat2type(pp,     type(e,t)):- !.
cat2type(np,     type(type(e,t),t)):- !.
cat2type(np_exp, type(type(e,t),t)):- !.
cat2type(np_thr, type(type(e,t),t)):- !.
cat2type(s:_,    type(type(e,t),t)):- !.
cat2type(Other, 'X'):- atom(Other), !.

cat2type('\\'(A,B), type(BType,AType) ):- cat2type(A,AType), cat2type(B,BType).
cat2type('/'(A,B), type(BType,AType) ):- cat2type(A,AType), cat2type(B,BType).


/*========================================================================
   Clean up CCG Cat
========================================================================*/

cleanCat('\\'(A1,B1), '\\'(A2,B2) ):- !, cleanCat(A1,A2), cleanCat(B1,B2).
cleanCat('/'(A1,B1),  '/'(A2,B2)  ):- !, cleanCat(A1,A2), cleanCat(B1,B2).
cleanCat(n:_,n):- !.
cleanCat(X,X).


/*========================================================================
   Generate Semantic Representation
========================================================================*/

getsem(CleanCat,Token,Converted,Start,Tok):- 
    lexcall(Cat,Sym,Lem,Pos:Ner,Start,Tok), 
    semlex(Cat,Sym,Lem,Pos:Ner,[1],Sem),
    betaConvert(Sem,Converted),
    cleanCat(Cat,CleanCat),
    (    member(Pos,['NNP','NNPS']), Token = Sym
    ; \+ member(Pos,['NNP','NNPS']), downcase_atom(Sym,Token) ).


/*========================================================================
   Get example sentence
========================================================================*/

getexample(I1,I2,L,Len):- getexample1(I1,I2,L,Len), !.
getexample(I,_,[],0).

getexample1(I1,I2,[Sym|L],I):-
   lexcall(_,Sym,_,_,I1,I2), !,
   I3 is I2 + 1,
   getexample1(I1,I3,L,I).

getexample1(I1,I2,['.'],I2):-
   \+ (lexcall(_,_,_,_,I1,I3), I2 < I3), !.


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

printCat(np_thr,Stream):- !,
    write(Stream,np:thr).

printCat(np_exp,Stream):- !,
    write(Stream,np:exp).

printCat(Cat:Fea,Stream):- 
    var(Fea), !, Fea = 'X', 
    printCat(Cat:Fea,Stream).

printCat(A,Stream):- 
   write(Stream,A).


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

printSentence([],Stream,_,_):- nl(Stream).

printSentence([X|L],Stream,W,W):- !,
   write(Stream,'\\textbf{'), printTok(X,Stream), write(Stream,'} '),
   N is W + 1,
   printSentence(L,Stream,N,W).

printSentence([X|L],Stream,M,W):-
   printTok(X,Stream), write(Stream,' '),
   N is M + 1,
   printSentence(L,Stream,N,W).




/*========================================================================
   Typeset Tokens
========================================================================*/

printTokens(Id,Stream):-
   findall(t(F,Tok),tok(Id,Tok,F),L),
   sort(L,Sorted), reverse(Sorted,Ordered),
   printTok(Ordered,10,Stream).

printTok([t(F,X)|_],1,Stream):- !,
   printIndex(X,Stream),
   write(Stream,'\\textit{'), printTok(X,Stream), write(Stream,'} ('),
   write(Stream,F), write(Stream,').'),
   nl(Stream).

printTok([t(F,X)],_,Stream):- !,
   printIndex(X,Stream),
   write(Stream,'\\textit{'), printTok(X,Stream),  write(Stream,'} ('),
   write(Stream,F), write(Stream,').'),
   nl(Stream).

printTok([t(F,X)|L],N,Stream):- !,
   printIndex(X,Stream),
   write(Stream,'\\textit{'), printTok(X,Stream),  write(Stream,'} ('),
   write(Stream,F), write(Stream,'), '),
   M is N - 1,
   printTok(L,M,Stream).


printIndex(Tok,_):-
   atom(Tok),
   atom_chars(Tok,Chars),
   special(Special),
   member(Special,Chars), !.

printIndex(Tok,Stream):-
   write(Stream,'\\index{'), printTok(Tok,Stream), write(Stream,'}').


printTok(Tok,Stream):-
   atom(Tok),
   atom_chars(Tok,Chars),
   special(Special),
   member(Special,Chars), !,
   printChars(Chars,Stream).

printTok(Tok,Stream):-
   write(Stream,Tok).

printChars([],_).

printChars([Char|L],Stream):- 
   special(Char), !,
   write(Stream,'\\'), write(Stream,Char),
   printChars(L,Stream).

printChars([X|L],Stream):- !,
   write(Stream,X),
   printChars(L,Stream).


/*========================================================================
   Special Charachters (for Latex)
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
   Init
========================================================================*/

init:-
   getsem(Cat,Tok,X,Sen,Pos),
   add(Cat,Tok,X,Sen,Pos),
   fail.

init:-
   computeF.


/*========================================================================
   Add to temp database
========================================================================*/

add(Cat,_,_,_,_):- 
   member(Cat,[conj,comma,semi]), !.

add(Cat,Tok,X1,Sen,Pos):-                      
   sem(Cat,X2,Id),                       
   similar(X1,X2), !,                 % similar sem present
   getexample(Sen,1,Ex,Len),
   addExample(Id,Ex,Len,Pos),
   (
      tok(Id,Tok,M), !,               % tok already present
      retract(tok(Id,Tok,M)),
      N is M + 1,
      assert(tok(Id,Tok,N))
   ;
      assert(tok(Id,Tok,1))           % new tok
   ).
   
add(Cat,Tok,X,Sen,Pos):-           % sem new, but
   \+ sem(_,_,_), !,                  % nothing in database yet
   assert(sem(Cat,X,1)),
   getexample(Sen,1,Ex,Len),
   addExample(1,Ex,Len,Pos),
   assert(tok(1,Tok,1)).

add(Cat,Tok,X,Sen,Pos):-                   % sem new
   sem(_,_,I),                        % get new id
   \+ (sem(_,_,J), J > I), !,         % increase highest previous id
   N is I + 1,
   assert(sem(Cat,X,N)),
   getexample(Sen,1,Ex,Len),
   addExample(N,Ex,Len,Pos),
   assert(tok(N,Tok,1)).


/*========================================================================
   Add example
========================================================================*/

addExample(Id,_,Len,_):-          %%% same length, hence
   ex(Id,_,Len,_), !.             %%% no need to add

addExample(Id,_,Len,_):-          %%% already found shorter example
   ex(Id,_,Shorter,_),            %%% hence no need to add
   Shorter < Len,                 %%% (but not too short!)
   Shorter > 5, !.

addExample(Id,Ex,Len,I):-         %%% replace previous example
   ex(Id,_,_,_), !, 
   retract(ex(Id,_,_,_)), 
   assert(ex(Id,Ex,Len,I)).

addExample(Id,Ex,Len,I):-         %%% add first example
   assert(ex(Id,Ex,Len,I)).


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
similar(named(_,_,T,_), named(_,_,T,_)).
similar(timex(_,_),     timex(_,_)).
similar(eq(_,_),        eq(_,_)).
similar(pred(_,_,T,_),  pred(_,_,T,_)).
similar(rel(_,_,_,_),   rel(_,_,_,_)).
similar(role(_,_,_,_),  role(_,_,_,_)).


/*========================================================================
   Main
========================================================================*/

body([s(F,Id)|L],Stream):-
   F > 10, !,
   sem(Cat,X,Id),
   cat2type(Cat,Type),
   write(Stream,'\\clearpage'),   nl(Stream),
   write(Stream,'Category: \\textbf{' ),
   printCat(Cat,Stream),    
   format(Stream,'} \\hfill F=~p~n~n',[F]),
   write(Stream,'Type: ' ),
   printType(Type,Stream),      nl(Stream), nl(Stream),        
   write(Stream,'DRS:'),        nl(Stream), nl(Stream),        
   numbervars(X,1,_),
   format(Stream,'%%%~p~n',[X]),
   drs2tex(X,Type,[]-_,Stream), nl(Stream), nl(Stream),
   write(Stream,'Entries: '),
   printTokens(Id,Stream),      nl(Stream), nl(Stream),        
   write(Stream,'Example: '),   nl(Stream), nl(Stream),
   ex(Id,Ex,_,W),
   printSentence(Ex,Stream,1,W),     nl(Stream), nl(Stream),
   body(L,Stream).

body([_|L],Stream):-
   body(L,Stream).

body([],Stream):- 
   nl(Stream).


/*========================================================================
   Start
========================================================================*/

go:-
   setDefaultOptions(boxer), 
   init,
   open('working/synsem.tex',write,Stream),
   printHeader(Stream),
   findall(s(F,Id),freq(Id,F),All),
   sort(All,Sorted),
   reverse(Sorted,Ordered),
   body(Ordered,Stream),
   printFooter(Stream),
   close(Stream).

:- go.

