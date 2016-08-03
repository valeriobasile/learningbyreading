
:- module(drs2tacitus,[drs2tac/4,printTAC/2,label/4]).

:- use_module(library(lists),[select/3,member/2]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).


/* ========================================================================
   Main Predicate
======================================================================== */

drs2tac(DRS,Tags,N,TAC):- 
   drs2tac(DRS,Tags,[],TAC0,N-_,_),
   replace(TAC0,TAC).


/* ========================================================================
   Replacing equality statements
======================================================================== */

replace(TAC1,TAC4):-
   select(replace(Old,New),TAC1,TAC2), !,
   replace(TAC2,Old,New,TAC3),
   replace(TAC3,TAC4).

replace(TAC,TAC).

replace(Atom,_,_,Atom):- atomic(Atom), !.

replace([],_,_,[]):- !.

replace([Var|L1],Old,New,[Var|L2]):- 
   var(Var), !,
   replace(L1,Old,New,L2).

replace([Old|L1],Old,New,[New|L2]):- !,
   replace(L1,Old,New,L2).

replace([Term1|L1],Old,New,[Term2|L2]):- 
   Term1 =.. [F|Args1], !,
   replace(Args1,Old,New,Args2),
   Term2 =.. [F|Args2],
   replace(L1,Old,New,L2).


/* ========================================================================
   Label
======================================================================== */

label(X,Label,Var,Y):-
   var(Var), number(X),
   atom_codes(Label,[Code]), !,
   number_codes(X,Codes),
   atom_codes(Var,[Code|Codes]),
   Y is X + 1.

label(X,_,_,X).


/* ========================================================================
   Translate DRSs into TACITUS formulas 
======================================================================== */

drs2tac(alfa(_,B1,B2),P,T1,T2,N,H):- !, 
   drs2tac(merge(B1,B2),P,T1,T2,N,H).

drs2tac(drs(_,Conds),P,T1,T2,N,H):- !, 
   conds2tac(Conds,P,T1,T2,N,H).

drs2tac(merge(B1,B2),P,T1,T3,N1-N3,H2):- !, 
   drs2tac(B1,P,T1,T2,N1-N2,_), 
   drs2tac(B2,P,T2,T3,N2-N3,H2).

drs2tac(sdrs([],Rel),P,T1,T2,N,Head):- !,
   conds2tac(Rel,P,T1,T2,N,Head).

drs2tac(sdrs([D|L],R),P,T1,T3,N1-N3,H):- !,
   drs2tac(D,P,T1,T2,N1-N2,_),
   drs2tac(sdrs(L,R),P,T2,T3,N2-N3,H).

drs2tac(lab(K,B),P,T1,[replace(K,H)|T2],N1-N2,H):- !,
   drs2tac(B,P,T1,T2,N1-N2,H).

drs2tac(sub(B1,B2),P,T1,T3,N1-N3,H):-
   drs2tac(B1,P,T1,T2,N1-N2,H),
   drs2tac(B2,P,T2,T3,N2-N3,_).


/* ========================================================================
   Translate DRS-Conditions into TACITUS formulas (wrapper)
======================================================================== */

conds2tac(Conds,P,T1,T2,N,Head):- 
   roles(Conds,Roles,NewConds),   
   conds2tac(NewConds,P,Roles,T1,T2,N,[],Head).
 

/* ========================================================================
   Translate DRS-Conditions into TACITUS formulas 
======================================================================== */
  
conds2tac([],_,_,T1,T2,N-N,Heads,Head):- !,
   adjustMods(Heads,T1,T2),
   pickHead(Heads,Head).

conds2tac([Cond|L],P,Roles,T1,T3,N1-N3,Heads,Head):-
   cond2tac(Cond,P,Roles,T1,T2,N1-N2,E), 
   conds2tac(L,P,Roles,T2,T3,N2-N3,[E|Heads],Head).


/* ========================================================================
   Make a guess as to what the head of a list of DRS-conditions is...
======================================================================== */

pickHead(Heads,Event):- 
   member(closing:_:Event,Heads), !.

pickHead(Heads,Event):- 
   member(event:[I]:Event,Heads),
   \+ (member(event:[J]:_,Heads), J < I), !.

pickHead([_:_:Head|_],Head):- !.

pickHead(_,_).


/* ========================================================================
   Adjust modifier modifiers...
======================================================================== */

adjustMods(Heads,T1,T3):-
   select(mod:[P1]:E1,Heads,Rest),
   member(mod:[P2]:E2,Rest), 
   P1 < P2,
   select(Mod1,T1,T2), Mod1 =.. [Sym,E1,E],
   member(Mod2,T2),    Mod2 =.. [_,E2,E], !,
   Mod =.. [Sym,E1,E2],
   adjustMods(Heads,[Mod|T2],T3).

adjustMods(_,T,T).
   

/* ========================================================================
   Separate roles from other DRS-conditions
======================================================================== */

roles([],[],[]).
roles([_:R|L1],[R|Roles],L2):- R = rel(_,_,Role,_), member(Role,[experiencer,topic,agent,patient,theme,recipient]), !, roles(L1,Roles,L2).
roles([Cond|L1],Roles,[Cond|L2]):- roles(L1,Roles,L2).


/* ========================================================================
   Translate a DRS-Condition into TACITUS formulas 
======================================================================== */

cond2tac(I:nec(Drs),P,_,T1,[I:nec(E1,E2)|T2],N1-N3,complex:I:E1):- !,
   label(N1,e,E1,N2), 
   drs2tac(Drs,P,T1,T2,N2-N3,E2).

cond2tac(I:pos(Drs),P,_,T1,[I:pos(E1,E2)|T2],N1-N3,complex:I:E1):- !,
   label(N1,e,E1,N2), 
   drs2tac(Drs,P,T1,T2,N2-N3,E2).

cond2tac(I:not(Drs),P,_,T1,[I:not(E1,E2)|T2],N1-N3,complex:I:E1):- !,
   label(N1,e,E1,N2), 
   drs2tac(Drs,P,T1,T2,N2-N3,E2).

cond2tac(I:prop(E,Drs),P,_,T1,[replace(E,H)|T2],N,complex:I:E):- !,
   drs2tac(Drs,P,T1,T2,N,H).

cond2tac(I:or(Drs1,Drs2),P,_,T1,[I:or(E,H1,H2)|T3],N1-N4,complex:I:E):- !,
   label(N1,e,E,N2),
   drs2tac(Drs1,P,T1,T2,N2-N3,H1),
   drs2tac(Drs2,P,T2,T3,N3-N4,H2).

cond2tac(I:imp(Drs1,Drs2),P,_,T1,[I:imp(E,H1,H2)|T3],N1-N4,complex:I:E):- !,
   label(N1,e,E,N2),
   drs2tac(Drs1,P,T1,T2,N2-N3,H1),
   drs2tac(Drs2,P,T2,T3,N3-N4,H2).

cond2tac(I:whq(Drs1,Drs2),P,_,T1,[I:whq(E,H1,H2)|T3],N1-N4,complex:I:E):- !,
   label(N1,e,E,N2),
   drs2tac(Drs1,P,T1,T2,N2-N3,H1),
   drs2tac(Drs2,P,T2,T3,N3-N4,H2).

cond2tac(I:duplex(_,Drs1,_,Drs2),P,_,T1,[I:whq(E,H1,H2)|T3],N1-N4,complex:I:E):- !,
   label(N1,e,E,N2),
   drs2tac(Drs1,P,T1,T2,N2-N3,H1),
   drs2tac(Drs2,P,T2,T3,N3-N4,H2).

cond2tac(I:card(X,C,_),_,_,T,[I:card(E,X,C)|T],N1-N2,card:I:E):- !,
   label(N1,e,E,N2).      

cond2tac(I:named(X,S1,Type,_),L,_,T,[I:F1,I:F2|T],N1-N3,named:I:E1):- !,
   label(N1,e,E1,N2),      
   label(N2,e,E2,N3),      
   pos(I,L,Pos),
   atom_concat(S1,Pos,S2),
   F1 =.. [S2,E1,X],
   F2 =.. [Type,E2,X].

cond2tac(I:timex(X,D1),_,_,T,[I:F|T],N1-N2,timex:I:E):-
   timex(D1,D2),
   label(N1,e,E,N2),      
   F =.. [D2,E,X], !.

cond2tac(I:eq(X,Y),_,_,T,[I:equal(E,X,Y)|T],N1-N2,equal:I:E):- !,
   label(N1,e,E,N2).      

cond2tac(I:pred(X,closing,v,99),_,_,T,T,N-N,closing:I:X):- !.

cond2tac(I:pred(X,S1,r,_),L,_,T,[I:F|T],N1-N2,mod:I:E):- !,
   pos(I,L,Pos),
   label(N1,e,E,N2),      
   atom_concat(S1,Pos,S2),
   F =.. [S2,E,X].

cond2tac(I:pred(X,S1,n,_),L,_,T,[I:F|T],N1-N2,noun:I:E):- !,
   pos(I,L,Pos),
   label(N1,e,E,N2),      
   atom_concat(S1,Pos,S2),
   F =.. [S2,E,X].

cond2tac(I:pred(E,S1,v,_),L,Roles,T,[I:F|T],N,event:I:E):- 
   pos(I,L,Pos), member(Pos,['-a','-r']), !,
   atom_concat(S1,Pos,S2),
   F =.. [S2,E,_,_,_],
   addRoles(Roles,E,F,N).

cond2tac(I:pred(E,S1,v,_),L,Roles,T,[I:F|T],N,event:I:E):- !, 
   pos(I,L,Pos),
   atom_concat(S1,Pos,S2),
   F =.. [S2,E,_,_,_],
   addRoles(Roles,E,F,N).

cond2tac(I:pred(E,S1,a,_),L,Roles,T,[I:F|T],N,mod:I:E):- 
   pos(I,L,Pos),
   atom_concat(S1,Pos,S2),
   F =.. [S2,E,_],
   addRoles(Roles,E,F,N), !.

cond2tac(I:pred(X,S1,a,_),L,_,T,[I:F|T],N1-N2,mod:I:E):- !,
   pos(I,L,Pos),
   label(N1,e,E,N2),      
   atom_concat(S1,Pos,S2),
   F =.. [S2,E,X].

cond2tac(I:rel(X,Y,P1,_),L,_,T,[I:F|T],N1-N2,rel:I:E):- !,
   pos(I,L,Pos),
   label(N1,e,E,N2),      
   atom_concat(P1,Pos,P2),
   F=..[P2,E,X,Y].

cond2tac(I:rel(X,Y,P1),L,_,T,[I:F|T],N1-N2,rel:I:E):- !,
   pos(I,L,Pos),
   label(N1,e,E,N2),      
   atom_concat(P1,Pos,P2),
   F=..[P2,E,X,Y].

cond2tac(I:X,_,_,T,T,N-N,unknown:I:_):-
   warning('cond2tac/3 failed for ~p',[X]).


/* ========================================================================
   Add roles as arguments
======================================================================== */

addRoles([],_,F,N1-N4):- 
   F =.. [_,_,A1,A2,A3], !,
   label(N1,u,A1,N2),   
   label(N2,u,A2,N3),   
   label(N3,u,A3,N4).

addRoles([],_,F,N1-N2):- 
   F =.. [_,_,A], !,
   label(N1,u,A,N2).

addRoles([rel(E,X,agent,0)|L],E,F,N):-
   F =.. [_,E,X,_,_], !,
   addRoles(L,E,F,N).

addRoles([rel(E,X,patient,0)|L],E,F,N):-
   F =.. [_,E,_,X,_], !,
   addRoles(L,E,F,N).

addRoles([rel(E,X,recipient,0)|L],E,F,N):-
   F =.. [_,E,_,X,_], !,
   addRoles(L,E,F,N).

addRoles([rel(E,X,topic,0)|L],E,F,N):-
   F =.. [_,E,_,_,X], !,
   addRoles(L,E,F,N).

addRoles([rel(E,X,theme,0)|L],E,F,N):-
   F =.. [_,E,_,_,X], !,
   addRoles(L,E,F,N).

addRoles([rel(E,X,Role,0)|L],E,F,N):-
   member(Role,[experiencer,topic,agent,patient,theme,recipient]),
   F =.. [_,E,X], !,
   addRoles(L,E,F,N).

addRoles([_|L],E,F,N):-
   addRoles(L,E,F,N).


/*========================================================================
   Time Expressions
========================================================================*/

timex(date(_:_,_:Y,_:M,_:D),Timex):- !,
   timex(date(Y,M,D),Timex).

timex(date(_:Y,_:M,_:D),Timex):- !,
   timex(date(Y,M,D),Timex).

timex(time(_:H,_:M,_:S),Timex):- !,
   timex(time(H,M,S),Timex).

timex(date(Y,M,D),Timex):-
   year(Y,[Y1,Y2,Y3,Y4]),
   month(M,[M1,M2]),
   day(D,[D1,D2]),
   name(Timex,[116,95,Y1,Y2,Y3,Y4,M1,M2,D1,D2]).

timex(time(H,M,S),Timex):-
   hour(H,[H1,H2]),
   minute(M,[M1,M2]),
   second(S,[S1,S2]),
   name(Timex,[116,95,H1,H2,M1,M2,S1,S2]).


/* ========================================================================
   Time Expressions (year)
======================================================================== */

year(Y,C):- var(Y), !, name('XXXX',C).
year(Y,C):- name(Y,C).


/* ========================================================================
   Time Expressions (month)
======================================================================== */

month(Y,C):- var(Y), !, name('XX',C).
month(Y,C):- name(Y,C).


/* ========================================================================
   Time Expressions (day)
======================================================================== */

day(Y,C):- var(Y), !, name('XX',C).
day(Y,C):- name(Y,C).


/* ========================================================================
   Time Expressions (other)
======================================================================== */

hour(A,C):- day(A,C).
minute(A,C):- day(A,C).
second(A,C):- day(A,C).


/* ========================================================================
   Determine POS
======================================================================== */

pos(Is,T,POS):-
   member(I,Is),
   member(I:Tags,T),
   member(pos:Tag,Tags),
   mappos(Tag,POS), !.

pos(_,_,'').


/* ========================================================================
   Map POS tags
======================================================================== */

mappos('NN',  '-n').
mappos('NNS', '-n').
mappos('NNP', '-n').
mappos('NNPS','-n').

mappos('VB',  '-v').
mappos('VBD', '-v').
mappos('VBG', '-v').
mappos('VBN', '-v').
mappos('VBP', '-v').
mappos('VBZ', '-v').
mappos('EX',  '-v').

mappos('JJ',  '-a').
mappos('JJR', '-a').
mappos('JJS', '-a').
mappos('SO',  '-a').

mappos('IN',  '-p').
mappos('POS', '-p').

mappos('RB',  '-r').
mappos('RBR', '-r').
mappos('RBS', '-r').
mappos('RP',  '-r').


/* ========================================================================
   Print TACITUS formula
======================================================================== */

printTAC([],Stream):- !, nl(Stream).
printTAC([X],Stream):- !, write(Stream,X), nl(Stream).
printTAC([X,Y|L],Stream):- write(Stream,X), write(Stream,' & '), printTAC([Y|L],Stream).
