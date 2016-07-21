
:- module(xdrs2html,[drs2html/2,
                     xfdrs2html/2,
                     xdrs2html/2]).

:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2]).


/*========================================================================
   Converting DRSs to XML
========================================================================*/

drs2html(DRS,Stream):-
   drs2html(DRS,Stream,3).

xdrs2html(XDRS,Stream):-
   XDRS=xdrs(_Tags,DRS),
   drs2html(DRS,Stream).

xfdrs2html(XDRS,Stream):-
   XDRS=xdrs(Tags,Cons),
   write(Stream,' <tags>'), nl(Stream),
   tags2html(Tags,Stream),
   write(Stream,' </tags>'), nl(Stream),
   write(Stream,' <cons>'), nl(Stream),
   cons2html(Cons,Stream),
   write(Stream,' </cons>'), nl(Stream).


/*========================================================================
   Converting DRSs to XML (with tab insertion)
========================================================================*/

drs2html(drs(D,C),Stream,Tab):-
   tab(Stream,Tab), format(Stream,'<table>~n',[]),
   tab(Stream,Tab), format(Stream,'<tr id="dom"><td>~n',[]),
   NewTab is Tab + 2,
   dom2html(D,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td></tr>~n',[]),
   tab(Stream,Tab), format(Stream,'<tr id="con"><td>~n',[]),
   conds2html(C,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td></tr>~n',[]),
   tab(Stream,Tab), format(Stream,'</table>~n',[]).

drs2html(alfa(_Type,B1,B2),Stream,Tab):- !,
   NewTab is Tab + 1,
   tab(Stream,Tab), format(Stream,'<table><tr><td>(</td><td>',[]),
   drs2html(B1,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td><td>&alpha;</td><td>',[]),
   drs2html(B2,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td><td>)</td></tr></table>~n',[]).

drs2html(merge(B1,B2),Stream,Tab):- !,
   NewTab is Tab + 1,
   tab(Stream,Tab), format(Stream,'<table><tr><td>(</td><td>',[]),
   drs2html(B1,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td><td>;</td><td>',[]),
   drs2html(B2,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td><td>)</td></tr></table>~n',[]).


/*========================================================================
   Converting DRS-domains to XML (with tab insertion)
========================================================================*/

dom2html([],_,_).

dom2html([_Index:X|L],Stream,Tab):- !,
   tab(Stream,Tab),   
%   format(Stream,'<dr name="~p">~n',[X]),
   format(Stream,'~p~n',[X]),
%   index2html(Index,Stream,Tab),
%   tab(Stream,Tab),
%   format(Stream,'</dr>~n',[]),
   dom2html(L,Stream,Tab).

dom2html([X|L],Stream,Tab):-
   option('--warnings',true), !,
   warning('cannot print DR ~p',[X]),
   dom2html(L,Stream,Tab).

dom2html([_|L],Stream,Tab):-
   dom2html(L,Stream,Tab).


/*========================================================================
   Converting DRS-conditions to XML (with tab insertion)
========================================================================*/

conds2html([],_,_).

conds2html([_Index:not(B)|L],Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<table><tr><td>&not;</td><td>~n',[]),
%   index2html(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2html(B,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td></tr></table>~n',[]),
   conds2html(L,Stream,Tab).

conds2html([_Index:nec(B)|L],Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<table><tr><td>[]</td><td>~n',[]),
%   index2html(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2html(B,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td></tr></table>~n',[]),
   conds2html(L,Stream,Tab).

conds2html([_Index:pos(B)|L],Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<table><tr><td>&loz;</td><td>~n',[]),
%   index2html(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2html(B,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td></tr></table>~n',[]),
   conds2html(L,Stream,Tab).

conds2html([_Index:prop(X,B)|L],Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<table><tr><td>~p:</td><td>~n',[X]),
%   index2html(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2html(B,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td></tr></table>~n',[]),
   conds2html(L,Stream,Tab).

conds2html([_Index:or(B1,B2)|L],Stream,Tab):- !,
   NewTab is Tab + 1,
   tab(Stream,Tab), format(Stream,'<table><tr><td>~n',[]),
%  index2html(Index,Stream,Tab),
   drs2html(B1,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td><td>V</td><td>~n',[]),
   drs2html(B2,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td></tr></table>~n',[]),
   conds2html(L,Stream,Tab).

conds2html([_Index:imp(B1,B2)|L],Stream,Tab):- !,
   NewTab is Tab + 1,
   tab(Stream,Tab), format(Stream,'<table><tr><td>~n',[]),
%  index2html(Index,Stream,Tab),
   drs2html(B1,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td><td>&rArr;</td><td>~n',[]),
   drs2html(B2,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</td></tr></table>~n',[]),
   conds2html(L,Stream,Tab).

conds2html([Index:duplex(_,B1,_,B2)|L],Stream,Tab):- !,
   conds2html([Index:imp(B1,B2)|L],Stream,Tab).

conds2html([_Index:pred(Arg,X,_Type,_Sense)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'~w(~p)<br>~n',[Y,Arg]),
%  index2html(Index,Stream,Tab),
   conds2html(L,Stream,Tab).

conds2html([_Index:rel(Arg1,Arg2,X,_Sense)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'~w(~p,~p)<br>~n',[Y,Arg1,Arg2]),
%   index2html(Index,Stream,Tab),
   conds2html(L,Stream,Tab).

conds2html([_Index:role(Arg1,Arg2,X,1)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'~w(~p,~p)<br>~n',[Y,Arg1,Arg2]),
%   index2html(Index,Stream,Tab),
   conds2html(L,Stream,Tab).

conds2html([_Index:role(Arg1,Arg2,X,-1)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'~w(~p,~p)<br>~n',[Y,Arg2,Arg1]),
%   index2html(Index,Stream,Tab),
   conds2html(L,Stream,Tab).

conds2html([_Index:named(Arg,X,Type,_)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'named(~w,~p,~p)<br>~n',[Y,Arg,Type]),
%  index2html(Index,Stream,Tab),
   conds2html(L,Stream,Tab).

conds2html([_Index:card(X,Y,_Type)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'|~p| = ~p<br>~n',[X,Y]),
%   index2html(Index,Stream,Tab),
   conds2html(L,Stream,Tab).

conds2html([_Index:timex(X,Y)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
%   index2html(Index,Stream,Tab),
   format(Stream,'timex(~p,',[X]),
   timex2html(Y,Stream),
   format(Stream,')<br>~n',[]),
   conds2html(L,Stream,Tab).

conds2html([_Index:eq(X,Y)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'~p=~p<br>~n',[X,Y]),
%   index2html(Index,Stream,Tab),
   conds2html(L,Stream,Tab).

conds2html([X|L],Stream,Tab):-
   option('--warnings',true), !,
   warning('cannot print DR-Condition ~p',[X]),
   conds2html(L,Stream,Tab).

conds2html([_|L],Stream,Tab):-
   conds2html(L,Stream,Tab).


/*========================================================================
   Timex
========================================================================*/

timex2html(date(_:A,_:B,_:C),Stream):- !,
   format(Stream,'~w~w~w',[A,B,C]).

timex2html(date(_:Z,_:A,_:B,_:C),Stream):- !,
   format(Stream,'~w~w~w~w',[Z,A,B,C]).

timex2html(time(_:A,_:B,_:C),Stream):- !,
   format(Stream,'~w~w~w',[A,B,C]).

timex2html(X,Stream):- !,
   format(Stream,'timex',[X]).


/*========================================================================
   Words
========================================================================*/

words2html([],_).

words2html([word(Index,Word)|L],Stream):-
   symbol(Word,Word1),
   format(Stream,'  <word xml:id="i~p">~w</word>~n',[Index,Word1]),
   words2html(L,Stream).


/*========================================================================
   POS tags
========================================================================*/

tags2html([],_).

tags2html([pos(Index,POS)|L],Stream):-
   format(Stream,'  <postag index="i~p">~w</postag>~n',[Index,POS]),
   tags2html(L,Stream).

tags2html([ne(Index,NE)|L],Stream):-
   format(Stream,'  <netag index="i~p">~w</netag>~n',[Index,NE]),
   tags2html(L,Stream).


/*========================================================================
   Flat DRSs
========================================================================*/

cons2html([],_).

cons2html([Label:alfa(Type,L1,L2)|Cons],Stream):- !,
   format(Stream,'  <alfa label="~p" type="~p"><label>~p</label><label>~p</label></alfa>~n',[Label,Type,L1,L2]),
   cons2html(Cons,Stream).

cons2html([Label:merge(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <merge label="~p"><label>~p</label><label>~p</label></merge>~n',[Label,L1,L2]),
   cons2html(Cons,Stream).

cons2html([Label:drs(D,Labels)|Cons],Stream):- !,
   format(Stream,'  <drs label="~p">~n',[Label]),
   dom2html(D,Stream,3),
   labels2html(Labels,Stream),
   format(Stream,'  </drs>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:named(Arg,X,Type,_)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <named label="~p" arg="~p" symbol="~w" type="~p">~n',[Label,Arg,Y,Type]),
   index2html(Index,Stream,2),
   format(Stream,'  </named>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:pred(Arg,X,Type,Sense)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <pred label="~p" arg="~p" symbol="~w" type="~p" sense="~p">~n',[Label,Arg,Y,Type,Sense]),
   index2html(Index,Stream,2),
   format(Stream,'  </pred>~n',[]), 
   cons2html(Cons,Stream).

cons2html([Label:Index:rel(Arg1,Arg2,X,Sense)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <rel label="~p" arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Label,Arg1,Arg2,Y,Sense]),
   index2html(Index,Stream,2),
   format(Stream,'  </rel>~n',[]), 
   cons2html(Cons,Stream).

cons2html([Label:Index:role(Arg1,Arg2,X,1)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <rel label="~p" arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Label,Arg1,Arg2,Y,1]),
   index2html(Index,Stream,2),
   format(Stream,'  </rel>~n',[]), 
   cons2html(Cons,Stream).

cons2html([Label:Index:role(Arg2,Arg2,1,-1)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <rel label="~p" arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Label,Arg1,Arg2,Y,1]),
   index2html(Index,Stream,2),
   format(Stream,'  </rel>~n',[]), 
   cons2html(Cons,Stream).

cons2html([Label:Index:card(X,Y,Type)|Cons],Stream):- !,
   format(Stream,'  <card label="~p" arg="~p" value="~p" type="~p">~n',[Label,X,Y,Type]),
   index2html(Index,Stream,2),
   format(Stream,'  </card>~n',[]), 
   cons2html(Cons,Stream).

cons2html([Label:Index:timex(X,Y)|Cons],Stream):- !,
   format(Stream,'  <timex label="~p" arg="~p">~n',[Label,X]),
   timex2html(Y,Stream),
   index2html(Index,Stream,2),
   format(Stream,'  </timex>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:eq(X,Y)|Cons],Stream):- !,
   format(Stream,'  <eq label="~p" arg1="~p" arg2="~p">~n',[Label,X,Y]),
   index2html(Index,Stream,2),
   format(Stream,'  </eq>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:not(L)|Cons],Stream):- !,
   format(Stream,'  <not label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2html(Index,Stream,2),
   format(Stream,'  </not>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:nec(L)|Cons],Stream):- !,
   format(Stream,'  <nec label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2html(Index,Stream,2),
   format(Stream,'  </nec>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:pos(L)|Cons],Stream):- !,
   format(Stream,'  <pos label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2html(Index,Stream,2),
   format(Stream,'  </pos>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:prop(X,L)|Cons],Stream):- !,
   format(Stream,'  <prop label="~p" argument="~p">~n',[Label,X]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2html(Index,Stream,2),
   format(Stream,'  </prop>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:or(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <or label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n  <label>~p</label>~n',[L1,L2]),
   index2html(Index,Stream,2),
   format(Stream,'  </or>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:imp(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <imp label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n  <label>~p</label>~n',[L1,L2]),
   index2html(Index,Stream,2),
   format(Stream,'  </imp>~n',[]),
   cons2html(Cons,Stream).

cons2html([Label:Index:duplex(_,L1,_,L2)|Cons],Stream):- !,
   cons2html([Label:Index:imp(L1,L2)|Cons],Stream).


/*========================================================================
   Labels
========================================================================*/

labels2html([],_).

labels2html([Label|L],Stream):-
   format(Stream,'   <label>~w</label>~n',[Label]),
   labels2html(L,Stream).


/*========================================================================
   Indexes
========================================================================*/

index2html([],_,_):- !.

index2html([X|L],Stream,Tab):-
   number(X), !,
   Pos is mod(X,1000),
   tab(Stream,Tab), 
   format(Stream,'<index pos="~p">i~p</index>~n',[Pos,X]),
   index2html(L,Stream,Tab).

index2html([_|L],Stream,Tab):-
   index2html(L,Stream,Tab).


/*========================================================================
   Deal with special symbols
========================================================================*/

symbol(S1,S2):-
   name(S1,C1),
   check(C1,C2),
   name(S2,C2).

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

   

