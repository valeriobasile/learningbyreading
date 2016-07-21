
:- module(printDrs,[printDrs/1,printDrs/2,printDrs/3]).

:- use_module(library(lists),[append/3]).

/* ========================================================================
     Counter for discourse referents
======================================================================== */

:- dynamic counter/1. counter(0).


/* ========================================================================
   Main Predicate
======================================================================== */

printDrs(B):- 
   printDrs(user_output,B).

printDrs(Stream,B):-
   LeftMargin = '%%% ',
   printDrs(Stream,B,LeftMargin).

printDrs(Stream,xdrs(_,B),LeftMargin):- !, 
   printDrs(Stream,B,LeftMargin).

printDrs(Stream,Drs,LeftMargin):- 
   retract(counter(_)), 
   assert(counter(1)),
   \+ \+ (formatDrs(Drs,Lines,_), 
          printDrsLines(Lines,Stream,LeftMargin)),
   nl(Stream).


/* ========================================================================
     Print DRS Lines
======================================================================== */

printDrsLines([],_,_):- !.

printDrsLines([Line|Rest],Stream,LeftMargin):-
   write(Stream,LeftMargin),
   atom_codes(L,Line), 
   write(Stream,L), 
   nl(Stream),
   printDrsLines(Rest,Stream,LeftMargin).


/* ========================================================================
    Dealing with a variable
======================================================================== */

avar(Var):- var(Var), !.
avar(Var):- atom(Var), !.
avar(Var):- nonvar(Var), functor(Var,'$VAR',1), !.


/* ========================================================================

     formatDrs(+DRS,    % any DRS exression
               +Lines,  % List of lists of character codes of equal length
               +Width)  % Length of the lines

======================================================================== */

formatDrs(Var,[Line,Line,Line,Codes,Line],N):- 
   avar(Var), !,
   makeConstant(Var,Codes),
   length(Codes,N),
   length(Line,N),
   append(Line,_,[32,32,32,32,32,32,32,32,32,32]).

formatDrs(sdrs(Conds,Rel),Lines,Width):- !,
   cleanConds(Rel,CleanRel),
   formatConds(CleanRel,[]-ConLines0,0-RelLength),
   formatCond(cons(Conds),[]-DrsLines0,RelLength-ConLength),
   Length is max(ConLength,RelLength),
   closeConds(ConLines0,ConLines1,Length),
   closeConds(DrsLines0,DrsLines1,Length),
   Width is Length + 2,
   formatLine(95,Length,[32]-Top),
   formatLine(32,Length,[124]-Middle),
   append([[32|Top]|DrsLines1],[[124|Middle]|ConLines1],Lines).

formatDrs(_:drs(D,C),Codes,Width):- !, formatDrs(drs(D,C),Codes,Width).

formatDrs(drs(Dom,Conds),[[32|Top],Refs3,[124|Line]|CondLines2],Width):- !,
   cleanConds(Conds,CleanConds), sortConds(CleanConds,SortedConds),
   formatConds(SortedConds,[]-CondLines1,0-CondLength),
   formatRefs(Dom,Refs1),
   length(Refs1,RefLength),
   Length is max(RefLength,CondLength),
   closeConds(CondLines1,CondLines2,Length),
   Width is Length + 2,
   closeLine(Width,[124|Refs1],[124],Refs3),
   formatLine(95,Length,[32]-Top),   
   formatLine(46,Length,[124]-Line). 

formatDrs(Complex,Lines3,Length):- 
   complexDrs(Complex,Op,Drs1,Drs2), !,
   atom_codes(Op,OpCode),
   length(OpCode,OpLen),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   combLinesDrs(Lines1,Lines2,Lines3,Op,N1,N2),
   Length is N1 + N2 + 2 + OpLen.

formatDrs(lam(X,Drs),Lines3,Length):- !,
   formatLambda(X,Lines1,N1),
   formatDrs(Drs,Lines2,N2),
   M1 is N1 + 3, M2 is N2 + 3,
   combLinesDrs(Lines1,Lines2,Lines3,'.',M1,M2),
   Length is N1 + N2 + 3.

formatDrs(lab(X,Drs),Lines4,Length):- !,
   LeftMargin = 32,      % space
   makeConstant(X,Var),
   length(Var,VarLen),
   OpWidth is VarLen + 2,  % +1 for colon
   length(Dummy,OpWidth),
   formatDrs(Drs,Lines1,DrsWidth),
   addLeftMargin(Lines1,Lines2,LeftMargin,OpWidth),
   Lines2=[Line1,Line2,Line3|Rest],
   append(Dummy,Tail,Line3),
   append([32|Var],[58|Tail],Line4),
   Lines4=[Line1,Line2,Line4|Rest],
   Length is DrsWidth + OpWidth.

formatDrs(sub(Drs1,Drs2),Lines,Length):- !,
   formatDrs(Drs1,Lines1,Length1),
   formatDrs(Drs2,Lines2,Length2),
   Length is max(Length1,Length2),
   append(Lines1,Lines2,Lines).


/*========================================================================
     Format Complex DRSs
========================================================================*/

complexDrs(merge(Drs1,Drs2),'+',Drs1,Drs2):- !.
complexDrs(alfa(_,Drs1,Drs2),'*',Drs1,Drs2):- !.
complexDrs(app(Drs1,Drs2),'@',Drs1,Drs2):- !.


/*========================================================================
     Format Discourse Referents
========================================================================*/

formatRefs([],[]):- !.

formatRefs([X],Code):- !, 
   ( nonvar(X), X=_:_:Ref, !
   ; nonvar(X), X=_:Ref, !
   ; var(X), X=Ref ),
   makeConstant(Ref,Code).

formatRefs([X,Ref2|Rest],Out):- 
   ( nonvar(X), X=_:_:Ref1, !
   ; nonvar(X), X=_:Ref1, !
   ; var(X), X=Ref1 ),
   makeConstant(Ref1,Code),
   append(Code,[32|Codes],Out), 
   formatRefs([Ref2|Rest],Codes).


/*========================================================================
     Format Lambda bound Variable
========================================================================*/

formatLambda(Var,Out,N):-
   makeConstant(Var,Code), 
   Lambda = [92|Code],
   length(Lambda,N),
   length(Line,N),
   append(Line,_,[32,32,32,32,32,32,32,32,32,32]),
   Out=[Line,Line,Line,Lambda,Line]. 


/*========================================================================
   Turn a discourse referent into a Prolog constant
========================================================================*/

makeConstant(X,Code):- !,
   makeConst(X,Code,120).

makeConstant(X,CodeTail,Tail):- !,
   makeConst(X,Code,120),
   append([32|Code],Tail,CodeTail).

makeConst(X,Code,_):- 
   atom(X), !,
   atom_codes(X,Code).

makeConst(X,[Var|Codes],Var):-
   nonvar(X),
   functor(X,'$VAR',1),
   arg(1,X,Number),
   number(Number), !,
   number_codes(Number,Codes).

makeConst(X,[Var|Number],Var):- 
   var(X), !,
   retract(counter(N)),
   number_codes(N,Number), 
   atom_codes(X,[Var|Number]),
   M is N+1,
   assert(counter(M)).


/*========================================================================
     Format a Line
========================================================================*/

formatLine(_,0,L-L):- !.

formatLine(Code,N,In-[Code|Out]):-
   M is N - 1, 
   formatLine(Code,M,In-Out).


/*========================================================================
     Clean DRS-Conditions
========================================================================*/

cleanConds(C1,C3):-
   select(_:_:C,C1,C2), !,
   cleanConds([C|C2],C3).

cleanConds(C1,C3):-
   select(_:C,C1,C2), !,
   cleanConds([C|C2],C3).

cleanConds(C1,C3):-
   select(_:C,C1,C2), !,
   cleanConds([C|C2],C3).

cleanConds(C,C).


/*========================================================================
    Sort DRS-Conditions
========================================================================*/

sortConds(C1,[named(A,B,C,D)|C3]):-
   select(named(A,B,C,D),C1,C2), !,
   sortConds(C2,C3).

sortConds(C1,[pred(A,B,C,D),Mod1,Mod2,Mod3,Mod4|C7]):-
   select(pred(A,B,C,D),C1,C2),
   selectModifier(A,Mod1,C2,C3),
   selectModifier(A,Mod2,C3,C4),
   selectModifier(A,Mod3,C4,C5),
   selectModifier(A,Mod4,C5,C6), !,
   sortConds(C6,C7).

sortConds(C1,[pred(A,B,C,D),Mod1,Mod2,Mod3|C6]):-
   select(pred(A,B,C,D),C1,C2),
   selectModifier(A,Mod1,C2,C3),
   selectModifier(A,Mod2,C3,C4),
   selectModifier(A,Mod3,C4,C5), !,
   sortConds(C5,C6).

sortConds(C1,[pred(A,B,C,D),Mod1,Mod2|C5]):-
   select(pred(A,B,C,D),C1,C2),
   selectModifier(A,Mod1,C2,C3),
   selectModifier(A,Mod2,C3,C4), !,
   sortConds(C4,C5).

sortConds(C1,[pred(A,B,C,D),Mod|C4]):-
   select(pred(A,B,C,D),C1,C2),
   selectModifier(A,Mod,C2,C3), !,
   sortConds(C3,C4).

sortConds(C1,[pred(A,B,C,D)|C3]):-
   select(pred(A,B,C,D),C1,C2), !,
   sortConds(C2,C3).

sortConds(C,C).


selectModifier(E,role(E,Y,S,1),C1,C2):- select(role(E,Y,S,1),C1,C2), !.
selectModifier(E,role(E,Y,S,1),C1,C2):- select(role(Y,E,S,-1),C1,C2), !.
selectModifier(E,rel(E,Y,S,T),C1,C2):- select(rel(E,Y,S,T),C1,C2), !.
selectModifier(E,card(E,Y,S),C1,C2):- select(card(E,Y,S),C1,C2), !.


/*========================================================================
     Formatting DRS-Conditions
========================================================================*/

formatConds([],L-L,N-N):- !.

formatConds([X|Rest],L1-L3,N1-N3):-
   formatCond(X,L2-L3,N1-N2), !,
   formatConds(Rest,L1-L2,N2-N3).


/*========================================================================
     Formatting Condition
========================================================================*/

formatCond(cons([C]),L1-L2,N1-N3):- !,
   formatDrs(C,Lines,N2),
   append(Lines,L1,L2),
   N3 is max(N2,N1).

formatCond(cons([C|Cs]),L1-L2,N0-N4):- !,
   formatDrs(C,Lines1,N1),
   formatCond(cons(Cs),[]-Lines2,0-N2),
   combLinesDrs(Lines1,Lines2,Lines3,N1,N2),
   append(Lines3,L1,L2),
   Length is N1 + N2 + 3,
   N4 is max(Length,N0).

formatCond(Complex,L1-L2,N0-N4):- 
   complexCond(Complex,Op,Drs1,Drs2), !,
   atom_codes(Op,OpCode),
   length(OpCode,OpLen),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   combLinesConds(Lines1,Lines2,Lines3,OpCode,N1,N2),
   append(Lines3,L1,L2),
   Length is N1 + N2 + OpLen + 2,
   N4 is max(Length,N0).

formatCond(Basic,L-[Line|L],N1-N2):-
   formatBasic(Basic,Line), !,
   length(Line,Length),
   N2 is max(Length,N1).

formatCond(Cond,L1-L2,N0-N3):- 
   member(Cond:[O1,O2],[not(Drs):[32,172],
                        pos(Drs):[60,62],
                        nec(Drs):[91,93]]), !,
   OpWidth = 4,
   LeftMargin = 32,
   formatDrs(Drs,Lines1,N2),
   addLeftMargin(Lines1,Lines2,LeftMargin,OpWidth),
   Lines2=[Line1,Line2,[_,_,_,_|Line3]|Rest],    
   Lines4=[Line1,Line2,[32,O1,O2,32|Line3]|Rest],
   append(Lines4,L1,L2),
   Length is N2 + OpWidth,
   N3 is max(Length,N0).

formatCond(prop(X,Drs),L1-L2,N0-N3):- !,
   LeftMargin = 32,      % space
   makeConstant(X,Var),
   length(Var,VarLen),
   OpWidth is VarLen + 2,  % one extra for colon, one for space
   length(Dummy,OpWidth),
   formatDrs(Drs,Lines1,DrsWidth),
   addLeftMargin(Lines1,Lines2,LeftMargin,OpWidth),
   Lines2=[Line1,Line2,Line3|Rest],  
   append(Dummy,Tail,Line3),
   append([32|Var],[58|Tail],Line4),
   Lines4=[Line1,Line2,Line4|Rest],
   append(Lines4,L1,L2),
   Length is DrsWidth + OpWidth,
   N3 is max(Length,N0).

formatCond(eq(A,B),L-[Line|L],N0-N2):- !,
   makeConstant(A,L1),
   makeConstant(B,L2),
   append(L1,[32,61,32|L2],Line),
   length(Line,Length),
   N2 is max(Length,N0).

formatCond(card(Arg,Integer,Type),L-[Line|L],N0-N2):- !,
   makeConstant(Arg,A),
   ( number(Integer), number_codes(Integer,D) ;
     \+ number(Integer), makeConstant(Integer,D) ),
   ( Type = eq, !, 
     append([32,32,124|A],[124,32,61,32|D],Line)            %%% =
   ; Type = ge, !, 
     append([32,32,124|A],[124,32,62,61,32|D],Line)         %%% >=
   ; Type = le, !, 
     append([32,32,124|A],[124,32,61,60,32|D],Line)         %%% =<
   ),
   length(Line,Length),
   N2 is max(Length,N0).

formatCond(timex(Arg,Timex),L-[Line|L],N0-N2):- !,
   atom_codes('timex',F),          
   makeConstant(Arg,A),
   timex(Timex,D),
   append(F,[40|A],T),
   append(T,[41,61|D],Line),
   length(Line,Length),
   N2 is max(Length,N0).


/*========================================================================
     Formatting Complex Conditions
========================================================================*/

complexCond(imp(Drs1,Drs2), '>' ,Drs1,Drs2).
complexCond(or(Drs1,Drs2),  'V' ,Drs1,Drs2).
complexCond(duplex(most, Drs1,_,Drs2), 'M' ,Drs1,Drs2).
complexCond(duplex(two,  Drs1,_,Drs2), '2' ,Drs1,Drs2).
complexCond(duplex(three,Drs1,_,Drs2), '3' ,Drs1,Drs2).
complexCond(duplex(Type, Drs1,_,Drs2), '?' ,Drs1,Drs2):- 
   \+ member(Type,[most,two,three]).


/*========================================================================
     Formatting Constant Relations
========================================================================*/

specialRel(temp_before,   60):- !.          %%% <
specialRel(temp_included, 91):- !.          %%% [ 
specialRel(temp_includes, 93):- !.          %%% ]
specialRel(temp_abut,    124):- !.          %%% |
specialRel(temp_overlap,  79):- !.          %%% O
specialRel(member_of,    101):- !.          %%% e
specialRel(subset_of,     67):- !.          %%% C


/*========================================================================
     Formatting Basic Conditions
========================================================================*/

%formatBasic(pred(Arg,Functor,a,_),Line):- !,
%   atom_codes(Functor,F),
%   makeConstant(Arg,A),   
%   append([98,101,45|F],[40|A],T),
%   append(T,[41],Line).

formatBasic(pred(Arg,Functor,_,_),Line):- !,
   atom_codes(Functor,F),
   makeConstant(Arg,A),   
   append(F,[40|A],T),
   append(T,[41],Line).
   
formatBasic(role(Arg1,Arg2,Rel,1),Line):- !,
   formatBasic(rel(Arg1,Arg2,Rel,0),Line).

formatBasic(role(Arg1,Arg2,Rel,-1),Line):- !,
   formatBasic(rel(Arg2,Arg1,Rel,0),Line).

formatBasic(rel(Arg1,Arg2,Rel,1),Line):-
   specialRel(Rel,Sym), !, 
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   append(A1,[32,Sym,32|A2],Line).

formatBasic(rel(Arg1,Arg2,Functor,_),Line):- !,
   atom_codes(Functor,F),
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   append([32,32|F],[40|A1],T1),
   append(T1,[44|A2],T2),
   append(T2,[41],Line).

formatBasic(rel(Arg1,Arg2,Functor),Line):- !,
   atom_codes(Functor,F),
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   append(F,[40|A1],T1),
   append(T1,[44|A2],T2),
   append(T2,[41],Line).

formatBasic(named(Arg,Sym,Type,_),Line):- !,
   atom_codes(named,F),
   makeConstant(Arg,A),
   makeConstant(Sym,S),
   makeConstant(Type,T),
   append(F,[40|A],T1),
   append(T1,[44|S],T2),
   append(T2,[44|T],T3),
   append(T3,[41],Line).
 

/*========================================================================
   Combining Lines of Characters (Complex DRS-Conditions)
========================================================================*/
    
combLinesConds([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N1,N2):-
   combLinesDrs([A1,B1,C1],[A2,B2,C2],Firsts,N1,N2),
   append([32|D1],Op,D3), append(D3,D2,D4),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2),
   append(Firsts,[D4|Rest],Result).


/*========================================================================
   Add Left Margin
========================================================================*/

addLeftMargin([],[],_,_):- !.

addLeftMargin([A2|Rest1],[A|Rest2],LeftMargin,Width):- !,
   closeLine(Width,[LeftMargin],[],A1),
   append(A1,A2,A),
   addLeftMargin(Rest1,Rest2,LeftMargin,Width).


/*========================================================================
   Combining Lines of Characters (Complex DRSs)
========================================================================*/
    
combLinesDrs([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N1,N2):-
   combLinesDrs([A1,B1,C1],[A2,B2,C2],Firsts,N1,N2),
   atom_codes(Op,Code),
   append(Code,D2,T1),
   append(T1,[41],T2),
   append([40|D1],T2,D),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2),
   append(Firsts,[D|Rest],Result).

combLinesDrs([],[],[],_,_):- !.

combLinesDrs([],[A2|Rest2],[A3|Rest],N1,N2):- !,
   N is N1+N2+3,
   append(A2,[32],A4),
   closeLine(N,[32],A4,A3),
   combLinesDrs([],Rest2,Rest,N1,N2).

combLinesDrs([A1|Rest1],[],[Closed|Rest],N1,N2):- !,
   N is N1+N2+3,
   closeLine(N,[32|A1],[],Closed),
   combLinesDrs(Rest1,[],Rest,N1,N2).

combLinesDrs([A1|Rest1],[A2|Rest2],[A3|Rest],N1,N2):- !,
   N is N1+N2+3,
   append(A2,[32],A4),
   closeLine(N,[32|A1],A4,A3),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2).


/*========================================================================
     Close Conditions (add '|')
========================================================================*/

closeConds([],[[124|Bottom]],Width):- !,
   formatLine(95,Width,[124]-Bottom).

closeConds([Line|Rest1],[[124|New]|Rest2],Width):-
   Length is Width+1,  %% add right margin
   closeLine(Length,Line,[124],New),
   closeConds(Rest1,Rest2,Width).


/*========================================================================
     Close Line 
========================================================================*/

closeLine(Number,Left,Right,Result):-
   length(Left,N1),
   length(Right,N2),
   N is Number-(N1+N2),
   closeLine2(N,Left,Right,Result).

closeLine2(N,Left,Right,New):- 
   N < 1, !, 
   append(Left,Right,New).

closeLine2(N,Left,Right,New):- 
   M is N - 1, !,
   closeLine2(M,Left,[32|Right],New).


/*========================================================================
   Time Expressions
========================================================================*/

timex(date(_:Y,_:M,_:D),Timex):- !,
   timex(date('+',Y,M,D),Timex).

timex(date(_:C,_:Y,_:M,_:D),Timex):- !,
   timex(date(C,Y,M,D),Timex).

timex(time(_:Y,_:M,_:D),Timex):- !,
   timex(time(Y,M,D),Timex).

timex(date(C,Y,M,D),Timex):- !,
   plusminus(C,[PM]),
   year(Y,[Y1,Y2,Y3,Y4]),
   month(M,[M1,M2]),
   day(D,[D1,D2]),
   Timex = [PM,Y1,Y2,Y3,Y4,M1,M2,D1,D2].

timex(time(H,M,S),Timex):-
   day(H,[H1,H2]),
   day(M,[M1,M2]),
   day(S,[S1,S2]),
   Timex = [H1,H2,58,M1,M2,58,S1,S2].

plusminus(Y,C):- var(Y), !, C = [88].
plusminus(Y,C):- atom_codes(Y,C).

year(Y,C):- var(Y), !, C = [88,88,88,88].
year(Y,C):- atom_codes(Y,C).

month(Y,C):- var(Y), !, C = [88,88].
month(Y,C):- atom_codes(Y,C).

day(Y,C):- var(Y), !, C = [88,88].
day(Y,C):- atom_codes(Y,C).
