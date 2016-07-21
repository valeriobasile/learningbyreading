:- module(xdrs2xml,[drs2xml/2,
                    der2xml/3,
                    xdrs2xml/2]).

:- use_module(semlib(errors),[warning/2]).
:- use_module(library(lists),[member/2,append/3]).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).
:- use_module(boxer(alphaConversionDRT),[alphaConvertDRS/2]).
:- use_module(boxer(drs2fdrs),[instDrs/1]).


/*========================================================================
   Converting DRSs to XML
========================================================================*/

drs2xml(DRS,Stream):- drs2xml(DRS,Stream,1,[]).

der2xml(Der,I,Stream):- 
   format(Stream,' <der id="~p">~n',[I]),
   deri2xml(Der,Stream,2),
   format(Stream,' </der>~n',[]).

xdrs2xml(XDRS,Stream):-
   XDRS=xdrs(Tags,DRS),
   write(Stream,' <taggedtokens>'), nl(Stream),
   tokentags2xml(Tags,Stream),
   write(Stream,' </taggedtokens>'), nl(Stream),
   drs2xml(DRS,Stream,1,Tags).


/*========================================================================
   Converting CCG derivation to XML (with tab insertion)
========================================================================*/

deri2xml(t(Cat,Token,Sem,Att,Index),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<lex id="~p">~n',[Index]),
   NewTab is Tab + 2,
   symbol(Token,NiceToken),
   tab(Stream,Tab), format(Stream,' <token>~p</token>~n',[NiceToken]), 
   tags2xml(Att,Stream,Tab),
   tab(Stream,Tab), format(Stream,' <cat>~n',[]),
   numbervars(Cat,23,_),
   cat2xml(Cat,Stream,NewTab),
   tab(Stream,Tab), format(Stream,' </cat>~n',[]),  
   tab(Stream,Tab), format(Stream,' <sem>~n',[]),  
   betaConvert(Sem,Red),
   \+ \+ (instDrs(Red),
          drs2xml(Red,Stream,NewTab,[])), 
   tab(Stream,Tab), format(Stream,' </sem>~n',[]),  
   tab(Stream,Tab), format(Stream,'</lex>~n',[]).

deri2xml(Der,Stream,Tab):- 
   Der =.. [Rule,Cat,_,Sem,_,Tokens,Under], 
   member(Rule,[tc,ftr,btr]), !,
   NewTab is Tab + 1,
   NextTab is Tab + 1,
   tab(Stream,Tab), urule2xml(Stream,Rule),
   tab(Stream,Tab), format(Stream,' <cat>~n',[]),  
   numbervars(Cat,23,_),
   cat2xml(Cat,Stream,NewTab),
   tab(Stream,Tab), format(Stream,' </cat>~n',[]),  
   tab(Stream,Tab), format(Stream,' <sem>~n',[]),  
   betaConvert(Sem,Red),
   \+ \+ (instDrs(Red),
          drs2xml(Red,Stream,NewTab,[])),
   tab(Stream,Tab), format(Stream,' </sem>~n',[]),  
   deri2xml(Under,Stream,NewTab),
   tab(Stream,NewTab), format(Stream,'<tokens>~n',[]),  
   tokens2xml(Tokens,NextTab,Stream),
   tab(Stream,NewTab), format(Stream,'</tokens>~n',[]),  
   tab(Stream,Tab), format(Stream,'</unaryrule>~n',[]).

deri2xml(Der,Stream,Tab):- 
   Der =.. [conj,Cat,_,Sem,_,Tokens,Left,Right], !,
   NewTab is Tab + 1,
   NextTab is Tab + 2,
   tab(Stream,Tab), brule2xml(Stream,conj),
   tab(Stream,NewTab), format(Stream,'<cat>~n',[]),  
   numbervars(Cat,23,_),
   cat2xml(Cat,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</cat>~n',[]),  
   tab(Stream,NewTab), format(Stream,'<sem>~n',[]),  
   betaConvert(Sem,Red), 
   \+ \+ (instDrs(Red),
          drs2xml(Red,Stream,NewTab,[])),
   tab(Stream,NewTab), format(Stream,'</sem>~n',[]),  
   deri2xml(Left,Stream,NewTab),   
   deri2xml(Right,Stream,NewTab),
   tab(Stream,NewTab), format(Stream,'<tokens>~n',[]),  
   tokens2xml(Tokens,NextTab,Stream),
   tab(Stream,NewTab), format(Stream,'</tokens>~n',[]), 
   tab(Stream,Tab),  format(Stream,'</binaryrule>~n',[]).

deri2xml(Der,Stream,Tab):- 
   Der =.. [Rule,Cat,Sem,_,Tokens,Left,Right], !,
   NewTab is Tab + 1,
   NextTab is Tab + 2,
   tab(Stream,Tab), brule2xml(Stream,Rule),
   tab(Stream,NewTab), format(Stream,'<cat>~n',[]),  
   numbervars(Cat,23,_),
   cat2xml(Cat,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</cat>~n',[]),  
   tab(Stream,NewTab), format(Stream,'<sem>~n',[]),  
   betaConvert(Sem,Red1),
   alphaConvertDRS(Red1,Red), % needed for functions introduced by NN compounds...
   \+ \+ (instDrs(Red),
          drs2xml(Red,Stream,NewTab,[])),
   tab(Stream,NewTab), format(Stream,'</sem>~n',[]),  
   deri2xml(Left,Stream,NewTab),   
   deri2xml(Right,Stream,NewTab),
   tab(Stream,NewTab), format(Stream,'<tokens>~n',[]),  
   tokens2xml(Tokens,NextTab,Stream),
   tab(Stream,NewTab), format(Stream,'</tokens>~n',[]), 
   tab(Stream,Tab),  format(Stream,'</binaryrule>~n',[]).

deri2xml(_,_,_).


/*========================================================================
   Producing CCG unary rules in XML
========================================================================*/

urule2xml(Stream,ftr):- !,
   format(Stream,'<unaryrule type="ftr" description="Forward Type Raising">~n',[]).

urule2xml(Stream,btr):- !,
   format(Stream,'<unaryrule type="btr" description="Backward Type Raising">~n',[]).

urule2xml(Stream,tc):- !,
   format(Stream,'<unaryrule type="tc" description="Type Changing">~n',[]).

urule2xml(Stream,Type):-
   format(Stream,'<unaryrule type="~p">~n',[Type]).


/*========================================================================
   Producing CCG binary rules in XML
========================================================================*/

brule2xml(Stream,Type):- Type = fa, !,
   format(Stream,'<binaryrule type="~p" description="Forward Application">~n',[Type]).

brule2xml(Stream,Type):- Type = ba, !,
   format(Stream,'<binaryrule type="~p" description="Backward Application">~n',[Type]).

brule2xml(Stream,Type):- member(Type,[fc,gfc]), !,
   format(Stream,'<binaryrule type="~p" description="Forward Composition">~n',[Type]).

brule2xml(Stream,Type):- member(Type,[bc,gbc]), !,
   format(Stream,'<binaryrule type="~p" description="Backward Composition">~n',[Type]).

brule2xml(Stream,Type):- member(Type,[fxc,gfxc]), !,
   format(Stream,'<binaryrule type="~p" description="Forward Crossed Composition">~n',[Type]).

brule2xml(Stream,Type):- member(Type,[bxc,gbxc]), !,
   format(Stream,'<binaryrule type="~p" description="Backward Crossed Composition">~n',[Type]).

brule2xml(Stream,Type):- Type = fxs, !,
   format(Stream,'<binaryrule type="~p" description="Forward Crossed Substitution">~n',[Type]).

brule2xml(Stream,Type):- Type = bxs, !,
   format(Stream,'<binaryrule type="~p" description="Backward Crossed Substitution">~n',[Type]).

brule2xml(Stream,Type):- Type = fs, !,
   format(Stream,'<binaryrule type="~p" description="Forward Substitution">~n',[Type]).

brule2xml(Stream,Type):- Type = bs, !,
   format(Stream,'<binaryrule type="~p" description="Backward Substitution">~n',[Type]).

brule2xml(Stream,Type):- Type = conj, !,
   format(Stream,'<binaryrule type="conj" description="Conjunction">~n',[]).

brule2xml(Stream,Type):-
   format(Stream,'<binaryrule type="~p">~n',[Type]).


/*========================================================================
   Converting CCG categories to XML (with tab insertion)
========================================================================*/

cat2xml(Cat,Stream,Tab):- 
   var(Cat), !,
   tab(Stream,Tab), format(Stream,' <atomic>~p</atomic>~n',[Cat]).

cat2xml('/'(L,R),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<forward>~n',[]),
   NewTab is Tab + 1,
   cat2xml(L,Stream,NewTab),
   cat2xml(R,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</forward>~n',[]).

cat2xml('\\'(L,R),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<backward>~n',[]),
   NewTab is Tab + 1,
   cat2xml(L,Stream,NewTab),
   cat2xml(R,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</backward>~n',[]).

cat2xml(Cat:_,Stream,Tab):-
   member(Cat,[n,conj]), !,  %%% Do not output features on N, conj
   cat2xml(Cat,Stream,Tab).

cat2xml(Cat:Feature,Stream,Tab):-
   atom(Cat), !,       
   upcase_atom(Cat,Up),
   tab(Stream,Tab), format(Stream,' <atomic feature="',[]),
   write_term(Stream,Feature,[numbervars(true)]),
   format(Stream,'">~p</atomic>~n',[Up]).

cat2xml(Cat,Stream,Tab):- 
   member(Cat:New,[conj:conj,comma:',',semi:';']), !,
   tab(Stream,Tab), format(Stream,' <atomic>~p</atomic>~n',[New]).

cat2xml(Cat,Stream,Tab):- !,
   upcase_atom(Cat,Up),
   tab(Stream,Tab), format(Stream,' <atomic>~p</atomic>~n',[Up]).


/*========================================================================
   Guess Sentence ID (a bit of a hack, obviously!)
========================================================================*/

getIDs(L:drs([_:I1:_|Dom],Conds),I3):- append(I1,I2,I3), getIDs(L:drs(Dom,Conds),I2).
getIDs(_:drs([],Conds),I):- getIDs(Conds,I).

getIDs([],[]).
getIDs([_:I1:Cond|Conds],I3):- !, append(I1,I2,I3), getIDs([Cond|Conds],I2).
getIDs([prop(_,B)|Conds],I3):- !, getIDs(B,I1), append(I1,I2,I3), getIDs(Conds,I2).
getIDs([not(B)|Conds],I3):- !, getIDs(B,I1), append(I1,I2,I3), getIDs(Conds,I2).
getIDs([pos(B)|Conds],I3):- !, getIDs(B,I1), append(I1,I2,I3), getIDs(Conds,I2).
getIDs([nec(B)|Conds],I3):- !, getIDs(B,I1), append(I1,I2,I3), getIDs(Conds,I2).
getIDs([or(B1,B2)|Conds],I5):- !, getIDs(B1,I1), getIDs(B2,I2), append(I1,I2,I3), append(I3,I4,I5), getIDs(Conds,I4).
getIDs([imp(B1,B2)|Conds],I5):- !, getIDs(B1,I1), getIDs(B2,I2), append(I1,I2,I3), append(I3,I4,I5), getIDs(Conds,I4).
getIDs([duplex(_,B1,_,B2)|Conds],I5):- !, getIDs(B1,I1), getIDs(B2,I2), append(I1,I2,I3), append(I3,I4,I5), getIDs(Conds,I4).
getIDs([_|Conds],I):- !, getIDs(Conds,I).
getIDs(_,[]).


/*========================================================================
   Converting DRSs to XML (with tab insertion)
========================================================================*/

drs2xml(Var,Stream,Tab,_):- 
   var(Var), !,
   tab(Stream,Tab), format(Stream,'<var>~p</var>~n',Var).

drs2xml(Var,Stream,Tab,_):- 
   atom(Var), !,
   tab(Stream,Tab), format(Stream,'<var>~p</var>~n',Var).

drs2xml(Var,Stream,Tab,_):- 
   Var =.. ['$VAR',_], !,
   tab(Stream,Tab), format(Stream,'<var>~p</var>~n',Var).

drs2xml(drs(D,C),Stream,Tab,Words):- !,
   drs2xml(l:drs(D,C),Stream,Tab,Words).

drs2xml(Label:drs(D,C),Stream,Tab,[]):- !,
   NewTab is Tab + 1,
   NextTab is Tab + 2,
   tab(Stream,Tab),    format(Stream,'<drs type="normal" label="~p">~n',[Label]),
   tab(Stream,NewTab), format(Stream,'<domain>~n',[]),
   dom2xml(D,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</domain>~n',[]),
   tab(Stream,NewTab), format(Stream,'<conds>~n',[]),
   conds2xml(C,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</conds>~n',[]),
   tab(Stream,Tab),    format(Stream,'</drs>~n',[]).

drs2xml(Label:drs(D,C),Stream,Tab,Words):- !,
   NewTab is Tab + 1,
   NextTab is Tab + 2,
   tab(Stream,Tab),
   format(Stream,'<drs type="sentence" label="~p">~n',[Label]),
   tab(Stream,NewTab), format(Stream,'<tokens>~n',[]),
   getIDs(Label:drs(D,C),IDs), 
   sort(IDs,SortedIDs),
   tokens2xml(SortedIDs,Words,NextTab,Stream),
   tab(Stream,NewTab), format(Stream,'</tokens>~n',[]),
   tab(Stream,NewTab), format(Stream,'<domain>~n',[]),
   dom2xml(D,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</domain>~n',[]),
   tab(Stream,NewTab), format(Stream,'<conds>~n',[]),
   conds2xml(C,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</conds>~n',[]),
   tab(Stream,Tab),    format(Stream,'</drs>~n',[]).

drs2xml(alfa(Type,B1,B2),Stream,Tab,_):- !,
   tab(Stream,Tab), format(Stream,'<alfa type="~p">~n',[Type]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</alfa>~n',[]).

drs2xml(lam(X,B),Stream,Tab,_):- !,
   tab(Stream,Tab), format(Stream,'<lam>~n',[]),
   NewTab is Tab + 1,
   tab(Stream,Tab), format(Stream,' <var>~p</var>~n',X),
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</lam>~n',[]).

drs2xml(app(B1,B2),Stream,Tab,_):- !,
   tab(Stream,Tab), format(Stream,'<app>~n',[]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</app>~n',[]).

drs2xml(merge(B1,B2),Stream,Tab,_):- !,
   tab(Stream,Tab), format(Stream,'<merge>~n',[]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</merge>~n',[]).

drs2xml(sdrs(Labs,Rels),Stream,Tab,Words):- !,
   NewTab is Tab + 1,
   NewerTab is NewTab + 1,
   tab(Stream,Tab), format(Stream,'<sdrs>~n',[]),
   tab(Stream,NewTab), format(Stream,'<constituents>~n',[]),
   sdrs2xml(Labs,Stream,NewerTab,Words,Rels),
   tab(Stream,NewTab), format(Stream,'</constituents>~n',[]),
   tab(Stream,NewTab), format(Stream,'<relations>~n',[]),
   relations2xml(Rels,Stream,NewerTab),
   tab(Stream,NewTab), format(Stream,'</relations>~n',[]),
   tab(Stream,Tab), format(Stream,'</sdrs>~n',[]).

drs2xml(Error,_,_,_):- !,
   warning('cannot print DRS in XML: ~p',[Error]).


/*========================================================================
   Converting SDRS to XML
========================================================================*/

sdrs2xml([],_,_,_,_):- !.

sdrs2xml([lab(K,B)|L],Stream,Tab,Words,Rel):- !,
   label2xml(K,B,Stream,Tab,Words),
   sdrs2xml(L,Stream,Tab,Words,Rel).   

sdrs2xml([sub(lab(K1,B1),lab(K2,B2))|L],Stream,Tab,Words,Rel):-
   tab(Stream,Tab), format(Stream,'<sub>~n',[]),
   label2xml(K1,B1,Stream,Tab,Words),
   label2xml(K2,B2,Stream,Tab,Words),
   tab(Stream,Tab), format(Stream,'</sub>~n',[]),
   sdrs2xml(L,Stream,Tab,Words,Rel).   


/*========================================================================
   Converting SDRS constituent to XML
========================================================================*/

label2xml(K,B,Stream,Tab,Words):-
   tab(Stream,Tab), format(Stream,'<constituent label="~p">~n',[K]),
   NewTab is Tab + 1, 
   drs2xml(B,Stream,NewTab,Words),
   tab(Stream,Tab), format(Stream,'</constituent>~n',[]).


/*========================================================================
   Converting SDRS relations to XML
========================================================================*/

relations2xml([],_,_).

relations2xml([Index:rel(K1,K2,Rel)|L],Stream,Tab):-
   tab(Stream,Tab), format(Stream,'<drel arg1="~p" arg2="~p" sym="~p">~n',[K1,K2,Rel]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</drel>~n',[]),
   relations2xml(L,Stream,Tab).   


/*========================================================================
   Converting DRS-domains to XML (with tab insertion)
========================================================================*/

dom2xml([],_,_).

dom2xml([Label:Index:X|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<dr label="~p" name="~p">~n',[Label,X]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),
   format(Stream,'</dr>~n',[]),
   dom2xml(L,Stream,Tab).

dom2xml([Index:X|L],Stream,Tab):- !,
   dom2xml([l:Index:X|L],Stream,Tab).

dom2xml([X|L],Stream,Tab):-
   warning('cannot print referent in XML: ~p',[X]),
   dom2xml(L,Stream,Tab).



/*========================================================================
   Converting DRS-conditions to XML (with tab insertion)
========================================================================*/

conds2xml([],_,_).

conds2xml([Label:Index:Cond|L],Stream,Tab):-
   tab(Stream,Tab), format(Stream,'<cond label="~p">~n',[Label]),
   NewTab is Tab + 1,
   cond2xml(Index:Cond,Stream,NewTab), !,
   tab(Stream,Tab), format(Stream,'</cond>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:Cond|L],Stream,Tab):- !,
   conds2xml([l:Index:Cond|L],Stream,Tab).

conds2xml([X|L],Stream,Tab):-
   warning('cannot print condition in XML: ~p',[X]),
   format(Stream,'</cond>~n',[]),
   conds2xml(L,Stream,Tab).


/*========================================================================
   Converting DRS-condition to XML (with tab insertion)
========================================================================*/

cond2xml(Index:not(B),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<not>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</not>~n',[]).

cond2xml(Index:nec(B),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<nec>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</nec>~n',[]).

cond2xml(Index:pos(B),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<pos>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</pos>~n',[]).

cond2xml(Index:prop(X,B),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<prop argument="~p">~n',[X]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</prop>~n',[]).

cond2xml(Index:or(B1,B2),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<or>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</or>~n',[]).

cond2xml(Index:imp(B1,B2),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<imp>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</imp>~n',[]).

cond2xml(Index:duplex(Type,B1,Var,B2),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<duplex type="~p" var="~p">~n',[Type,Var]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,'</duplex>~n',[]).

cond2xml(Index:pred(Arg,X,Type,Sense),Stream,Tab):- !,
   symbol(X,Y),
   tab(Stream,Tab), format(Stream,'<pred arg="~p" symbol="~w" type="~p" sense="~p">~n',[Arg,Y,Type,Sense]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</pred>~n',[]).

cond2xml(Index:role(Arg2,Arg1,X,-1),Stream,Tab):- !,
   symbol(X,Y),
   tab(Stream,Tab), format(Stream,'<rel arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Arg1,Arg2,Y,1]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</rel>~n',[]).

cond2xml(Index:role(Arg1,Arg2,X,1),Stream,Tab):- !,
   symbol(X,Y),
   tab(Stream,Tab), format(Stream,'<rel arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Arg1,Arg2,Y,1]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</rel>~n',[]).

cond2xml(Index:rel(X,Y,Sym,0),Stream,Tab):-
   symbol(Sym,=), !,
   cond2xml(Index:eq(X,Y),Stream,Tab).

cond2xml(Index:rel(Arg1,Arg2,X,Sense),Stream,Tab):- !,
   symbol(X,Y),
   tab(Stream,Tab), format(Stream,'<rel arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Arg1,Arg2,Y,Sense]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</rel>~n',[]).

cond2xml(Index:named(Arg,X,Class,Type),Stream,Tab):- !,
   symbol(X,Y),
   tab(Stream,Tab), format(Stream,'<named arg="~p" symbol="~w" class="~p" type="~p">~n',[Arg,Y,Class,Type]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</named>~n',[]).

cond2xml(Index:card(X,Y,Type),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<card arg="~p" value="~p" type="~p">~n',[X,Y,Type]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</card>~n',[]).

cond2xml(Index:timex(X,Y),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<timex arg="~p">~n',[X]),
   index2xml(Index,Stream,Tab),
   timex2xml(Y,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</timex>~n',[]).

cond2xml(Index:eq(X,Y),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<eq arg1="~p" arg2="~p">~n',[X,Y]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</eq>~n',[]).


/*========================================================================
   Timex
========================================================================*/

timex2xml(date(_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<date>~w~w~w</date>~n',[A,B,C]).

timex2xml(date(_:Z,_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<date>~w~w~w~w</date>~n',[Z,A,B,C]).

timex2xml(time(_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<time>~w~w~w</time>~n',[A,B,C]).

timex2xml(X,Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<unknown>~p</unknown>~n',[X]).


/*========================================================================
   Tokens (already known)
========================================================================*/

tokens2xml([],_,_).

tokens2xml([Token|L],Tab,Stream):-
   symbol(Token,NiceToken),
   tab(Stream,Tab), format(Stream,'<token>~w</token>~n',[NiceToken]),
   tokens2xml(L,Tab,Stream).


/*========================================================================
   Tokens (from list of words)
========================================================================*/

tokens2xml(I,[presup|Words],Tab,Stream):- !, tokens2xml(I,Words,Tab,Stream,presup).
tokens2xml(I,Words,Tab,Stream):- !, tokens2xml(I,Words,Tab,Stream,sentence).

tokens2xml([],_,_,_,_).

tokens2xml([Index],Words,Tab,Stream,_):-
   member(Index:[tok:Tok|_],Words), !,
   symbol(Tok,NiceToken),
   tab(Stream,Tab), format(Stream,'<token>~w</token>~n',[NiceToken]).

tokens2xml([Index1,Index2|L],Words,Tab,Stream,Type):-
   Type = presup,
   member(Index1:[tok:Tok|_],Words), !,
   symbol(Tok,NiceToken),
   tab(Stream,Tab), format(Stream,'<token>~w</token>~n',[NiceToken]),
   ( Dif is Index2 - Index1, 
     Dif > 1, tab(Stream,Tab), format(Stream,'<token>|</token>~n',[])
   ; true ),
   tokens2xml([Index2|L],Words,Tab,Stream,Type).

tokens2xml([Index1,Index2|L1],Words,Tab,Stream,Type):-
   Type = sentence,
   member(Index1:[tok:Tok|_],Words), !,
   symbol(Tok,NiceToken),
   tab(Stream,Tab), format(Stream,'<token>~w</token>~n',[NiceToken]),
   ( S is Index1-mod(Index1,1000), 
     S is Index2-mod(Index2,1000),
     Dif is Index2 - Index1, Dif > 1, !, Index is Index1 + 1,
     L2 = [Index,Index2|L1]
   ; L2 = [Index2|L1] ),
   tokens2xml(L2,Words,Tab,Stream,Type).

tokens2xml([_|L],Words,Tab,Stream,Type):-
   tokens2xml(L,Words,Tab,Stream,Type).


/*========================================================================
   Check whether word is part of sentence
========================================================================*/

wordInSentence(N1,N2):- 
   X is (N1-(mod(N1,1000)))/1000,
   X is (N2-(mod(N2,1000)))/1000.


/*========================================================================
   Token Tags
========================================================================*/

tokentags2xml([],_).

tokentags2xml([Index:Tags|L],Stream):-
   format(Stream,'  <tagtoken xml:id="i~p">~n',[Index]),
   format(Stream,'   <tags>~n',[]),
   tags2xml(Tags,Stream,4),
   format(Stream,'   </tags>~n',[]),
   format(Stream,'  </tagtoken>~n',[]),
   tokentags2xml(L,Stream).


/*========================================================================
   Producing tags in XML
========================================================================*/

tags2xml([],_,_):- !.

tags2xml([lemma:Lemma|L],Stream,Tab):- !,
   symbol(Lemma,NiceLemma),
   tab(Stream,Tab), format(Stream,' <tag type="lemma">~p</tag>~n',[NiceLemma]),  
   tags2xml(L,Stream,Tab).

tags2xml([tok:Tok|L],Stream,Tab):- !,
   symbol(Tok,NiceTok),
   tab(Stream,Tab), format(Stream,' <tag type="tok">~p</tag>~n',[NiceTok]),  
   tags2xml(L,Stream,Tab).

tags2xml([verbnet:Roles|L],Stream,Tab):- !,
   length(Roles,N),
   tab(Stream,Tab), format(Stream,' <tag type="verbnet" n="~p">~p</tag>~n',[N,Roles]),  
   tags2xml(L,Stream,Tab).

tags2xml([Feature:Value|L],Stream,Tab):-   
   tab(Stream,Tab), format(Stream,' <tag type="~p">~p</tag>~n',[Feature,Value]),  
   tags2xml(L,Stream,Tab).


/*========================================================================
   Indexes
========================================================================*/

index2xml(I,Stream,Tab):-
   tab(Stream,Tab), format(Stream,'<indexlist>~n',[]),
   index2xml2(I,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</indexlist>~n',[]).

index2xml2([],_,_):- !.

index2xml2([X|L],Stream,Tab):-
   number(X), !,
   Pos is mod(X,1000),
   tab(Stream,Tab), format(Stream,'<index pos="~p">i~p</index>~n',[Pos,X]),
   index2xml2(L,Stream,Tab).

index2xml2([_|L],Stream,Tab):-
   index2xml2(L,Stream,Tab).


/*========================================================================
   Deal with special symbols
========================================================================*/

symbol(f(_,_,V1),V2):- !, V1 = V2.

symbol(N1,N2):- number(N1), !, N2 = N2.

symbol(S1,S2):- atom_codes(S1,C1), check(C1,C2), atom_codes(S2,C2).


/*========================================================================
   Check special characters
========================================================================*/

check([],[]).

%%% Special character &
%%%
check([38|L1],[38,97,109,112,59|L2]):- !,
   check(L1,L2).

%%% Special character <
%%%
check([60|L1],[38,108,116,59|L2]):- !,
   check(L1,L2).

%%% Special character >
%%%
check([62|L1],[38,103,116,59|L2]):- !,
   check(L1,L2).

%%% Special character '
%%%
check([62|L1],[38,97,112,111,115,59|L2]):- !,
   check(L1,L2).

%%% Special character "
%%%
check([34|L1],[38,113,117,111,116,59|L2]):- !,
   check(L1,L2).

check([X|L1],[X|L2]):-
   check(L1,L2).

   

