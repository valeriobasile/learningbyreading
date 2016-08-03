
:- module(drs2amr,[drs2amr/4,printAMR/2]).

:- use_module(library(lists),[select/3,member/2,append/3]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(drs2tacitus),[label/4]).
:- use_module(knowledge(amrmfs),[mfs/2]).
:- use_module(knowledge('verbalization-list-v1.01'),[verbalize/2,verbalize/3,verbalize/4,verbalize/6]).
:- use_module(knowledge(role91),[role91/3]).
:- use_module(knowledge(negprefix),[negprefix/4]).
:- use_module(knowledge(negsuffix),[negsuffix/4]).
:- use_module(knowledge(nationality),[nationality/5]).
:- use_module(knowledge(wiki),[wiki/3]).

/* ========================================================================
   Main Predicate
======================================================================== */

drs2amr(DRS,Tags,N,AMR5):- 
   drs2amr(DRS,Tags,[],AMR1,N-_,_Root),
   pp(AMR1,AMR2),
   fixRoots(AMR2,AMR3), 
   replace(AMR3,AMR4),
   addins(AMR4,AMR5), !.

drs2amr(_,_,_,[[]:ins(x,and)]):-
   warning('unable to build AMR',[]).


/* ========================================================================
   Compute all reachable nodes (from an arbitrary starting point)
======================================================================== */

reachable(X,_,R,R):- member(X,R), !.
reachable(X,AMR,R1,R2):-
   findall(Y,(member(_:arg(_,X,Y),AMR);member(_:arg(_,Y,X),AMR)),L),
   reachableL(L,AMR,[X|R1],R2).

reachableL([],_,R,R).
reachableL([X|L],AMR,R1,R3):- 
   reachable(X,AMR,R1,R2),
   reachableL(L,AMR,R2,R3).


/* ========================================================================
   Count Vertices
======================================================================== */

countVertices([],C,N):-
   length(C,N).

countVertices([_:arg(_,X,Y)|L],C1,N):-
   addV(X,C1,C2), addV(Y,C2,C3),
   countVertices(L,C3,N).

countVertices([_:ins(X,_)|L],C1,N):-
   addV(X,C1,C2),
   countVertices(L,C2,N).


/* ========================================================================
   Add vertice to list if not seen before
======================================================================== */

addV(X,L,L):- member(X,L), !.
addV(X,L,[X|L]).


/* ========================================================================
   Fix Roots 
======================================================================== */

fixRoots(AMR1,AMR2):-
   getRoots(AMR1,Roots),
%  write(roots:Roots),nl,
   select(Root,Roots,FixRoots),
   inverseRoles(FixRoots,AMR1,AMR2), 
   checkInstance(Root,AMR2,[]), !.

fixRoots(AMR1,AMR2):-
   getRoots(AMR1,[R1,R2]), !, I=[],
   AMR2=[I:ins(root,'multi-sentence'),I:arg(snt1,root,R1),I:arg(snt2,root,R2)|AMR1],
   warning('cannot inverse roles for AMR with roots ~p and ~p, added artificial root',[R1,R2]).

fixRoots(AMR1,AMR2):-
   getRoots(AMR1,[R1,R2,R3]), !, I=[],
   AMR2=[I:ins(root,'multi-sentence'),I:arg(snt1,root,R1),I:arg(snt2,root,R2),I:arg(snt3,root,R3)|AMR1],
   warning('cannot inverse roles for AMR with roots ~p, ~p, and ~p, added artificial root',[R1,R2,R3]).

fixRoots(AMR1,AMR2):-
   getRoots(AMR1,[R1,R2,R3,R4]), !, I=[],
   AMR2=[I:ins(root,'multi-sentence'),I:arg(snt1,root,R1),I:arg(snt2,root,R2),I:arg(snt3,root,R3),I:arg(snt4,root,R4)|AMR1],
   warning('cannot inverse roles for AMR with roots ~p, ~p, ~p, and ~p, added artificial root',[R1,R2,R3,R4]).

fixRoots(AMR1,AMR2):-
   getRoots(AMR1,[]), 
   member(_:ins(X,_),AMR1),
   AMR2=[[]:ins(root,and),[]:arg(op1,root,X)|AMR1],
   checkInstance(root,AMR2,[]), !,
   warning('cyclic AMR, added new root to ~p',[X]).

fixRoots(AMR,AMR):-
   getRoots(AMR,Roots), !,
   warning('cannot inverse roles for AMR with roots ~p',[Roots]).


/* ========================================================================
   Ensure that an AMR has exactly one root. Do this by inversing roles.
======================================================================== */

inverseRoles([],AMR,AMR).

inverseRoles([Root|Roots],AMR1,AMR3):-
   inverseR(ARG,INV),
   select(I:arg(ARG,Root,X),AMR1,AMR2),
   inverseRoles(Roots,[I:arg(INV,X,Root)|AMR2],AMR3).

inverseRoles([Root|Roots],AMR1,AMR4):-
   inverseR(ARG1,INV1),
   select(I1:arg(ARG1,Root,X),AMR1,AMR2),
   inverseR(ARG2,INV2),
   select(I2:arg(ARG2,X,Y),AMR2,AMR3),
   inverseRoles(Roots,[I1:arg(INV1,X,Root),I2:arg(INV2,Y,X)|AMR3],AMR4).


/* ========================================================================
   Roles that can be inverted
======================================================================== */

inverseR('ARG0','ARG0-of').
inverseR('ARG1','ARG1-of').
inverseR('ARG2','ARG2-of').
inverseR('op1', 'op1-of').
inverseR('op2', 'op2-of').
inverseR('quant', 'quant-of').
inverseR('domain', 'domain-of').
inverseR('mod', 'mod-of').
inverseR('in', 'in-of').


/* ========================================================================
   replace(+AMR,-AMR)
   Replacing equality statements (introduced by prop/2 or lab/2 conditions)
======================================================================== */

replace(AMR1,AMR4):-
   select(replace(Old,New),AMR1,AMR2), !,
   replace(AMR2,Old,New,AMR3),
   replace(AMR3,AMR4).

replace(A,A).


/* ========================================================================
   addins(+AMR,-AMR)
   Adding ins/2 if not there
======================================================================== */

addins(AMR1,AMR2):- 
   member(_:arg(_,X,_),AMR1), 
   \+ member(_:ins(X,_),AMR1), !,
%  warning('added ins/2 thing to AMR for ~p on position ~p',[X,I]),
   addins([[]:ins(X,thing)|AMR1],AMR2).

addins(AMR1,AMR2):- 
   member(I:arg(_,_,X),AMR1),
   variable(X),
   \+ member(_:ins(X,_),AMR1), !,
   warning('added ins/2 thing to AMR for ~p on position ~p',[X,I]),
   addins([[]:ins(X,thing)|AMR1],AMR2).

addins(A,A).


/* ========================================================================
   Checking for a variable
======================================================================== */

variable(X):- 
   \+ member(X,[imperative]), 
   atom(X), name(X,[C|_]),
   C > 96,  C < 123.


/* ========================================================================
   Replacing equality statements
======================================================================== */

replace(Atom,_,_,Atom):- atomic(Atom), !.
replace([],_,_,[]):- !.
replace([Var|L1],Old,New,[Var|L2]):- var(Var), !, replace(L1,Old,New,L2).
replace([Old|L1],Old,New,[New|L2]):- !, replace(L1,Old,New,L2).
replace([Term1|L1],Old,New,[Term2|L2]):- 
   Term1 =.. [F|Args1], !, replace(Args1,Old,New,Args2),
   Term2 =.. [F|Args2],    replace(L1,Old,New,L2).


/* ========================================================================
   Post-processing
======================================================================== */

pp(AMR1,AMR4):-
%  printAMR(AMR1,user_output),nl,
   rule(Selects,Members,Adds,Conds),
   selectall(Selects,AMR1,AMR2),
   memberall(Members,AMR2), condsall(Conds), !,
   addall(Adds,AMR2,AMR3),
   pp(AMR3,AMR4).
   
pp(AMR,AMR).


/* ========================================================================
   Auxiliary predicates used by post-processing
======================================================================== */

selectall([],A,A).
selectall([X|L],A1,A3):- select(X,A1,A2), selectall(L,A2,A3).

memberall([],_).
memberall([not(X)|L],A):- \+ member(X,A), memberall(L,A).
memberall([X|L],A):- member(X,A), memberall(L,A).

condsall([]).
condsall([X|L]):- call(X), condsall(L).

addall([],A,A).
addall([X|L],A1,[X|A2]):- addall(L,A1,A2).


/* ========================================================================
   Rewriting Rules for AMRs
   rule(+Delete,+Check,+Add,+Conditions)
======================================================================== */

% more/less than X N
rule([J:ins(S,more),_:arg(than,S,X),_:arg('ARG1-of',Y,S),I:arg(quant,X,Q)],[],[I:arg(quant,Y,S),J:ins(S,'more-than'),[]:arg('op1',S,Q)],[]).
rule([J:ins(S,less),_:arg(than,S,X),_:arg('ARG1-of',Y,S),I:arg(quant,X,Q)],[],[I:arg(quant,Y,S),J:ins(S,'less-than'),[]:arg('op1',S,Q)],[]).

% fix adverbs
rule([I:arg(manner,E,A)],[_:ins(A,also)],[I:arg(mod,E,A)],[]).     
rule([I:arg(manner,E,A),J:ins(A,ful)],[],[I:arg(degree,E,A),J:ins(A,full)],[]).     

% fix double prepositions
%rule([[I]:arg(Prep,X,Y),_:ins(Y,thing),[J]:arg(Prep2,Y,Z)],[],[[I]:arg(Prep,X,Z)],[J is I + 1, warning('removed double preposition ~p',[Prep2])]).     
rule([_:ins(X,such),I:arg(as,X,Y)],[_:ins(X,_)],[I:arg(example,X,Y)],[]).

% next/last year
rule([[I]:ins(X,next)],[_:arg(time,X,Y),_:ins(X,_)],[I:ins(New,next),[]:arg(mod,Y,New)],[name(I,L),name(New,[117|L])]).
rule([[I]:ins(X,last)],[_:arg(time,X,Y),_:ins(X,_)],[I:ins(New,last),[]:arg(mod,Y,New)],[name(I,L),name(New,[117|L])]).
rule([_:arg(manner,E,X)],[[I]:ins(X,last),[J]:ins(Y,_),_:arg(time,E,Y)],[[]:arg(mod,Y,X)],[J is I + 1]).
rule([_:arg(manner,E,X)],[[I]:ins(X,next),[J]:ins(Y,_),_:arg(time,E,Y)],[[]:arg(mod,Y,X)],[J is I + 1]).

% fix intensifiers
rule([I:arg(manner,E,V)],[_:ins(V,very),_:arg(manner,E,M)],[I:arg(degree,M,V)],[]).     
rule([I:arg(manner,E,V)],[_:ins(V,too), _:arg(manner,E,M)],[I:arg(degree,M,V)],[]).     
rule([I:arg(manner,E,V)],[_:ins(V,so), _:arg(manner,E,M)],[I:arg(degree,M,V)],[]).     
rule([I:arg(manner,E,V)],[_:ins(V,very)],[I:arg(degree,E,V)],[]).     
rule([I:arg(manner,E,V)],[_:ins(V,less)],[I:arg(degree,E,V)],[]).     
rule([I:arg(manner,E,V)],[_:ins(V,more)],[I:arg(degree,E,V)],[]).     
rule([I:arg(manner,E,V)],[_:ins(V,most)],[I:arg(degree,E,V)],[]).     
rule([I:arg(mod,E,V)],[_:ins(V,more)],[I:arg(degree,E,V)],[]).     
rule([I:arg(mod,E,V)],[_:ins(V,so)],[I:arg(degree,E,V)],[]).     
rule([I:arg(ord,E,S)],[_:arg(degree,X,M),_:ins(M,most),_:arg(domain,X,E)],[I:arg(ord,M,S)],[]).
rule([I:arg(manner,X,S)],[_:ins(S,so),_:arg(quant,X,M),_:ins(M,much)],[I:arg(degree,M,S)],[]).     
rule([I:arg(mod,X,S)],[_:ins(S,great),_:arg(quant,X,M),_:ins(M,many)],[I:arg(mod,M,S)],[]).     

% fix adjectives
rule([I:arg(mod,E,V)],[_:ins(V,overseas)],[I:arg(location,E,V)],[]).     
rule([I:arg(mod,E,V)],[_:ins(V,rural)],[I:arg(location,E,V)],[]).     
rule([I:arg(manner,E,V)],[_:ins(V,overseas)],[I:arg(location,E,V)],[]).     

% fix ordinals
rule([I:arg(mod,E,O),J:ins(O,first)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,1)],[]).
rule([I:arg(mod,E,O),J:ins(O,second)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,2)],[]).
rule([I:arg(mod,E,O),J:ins(O,third)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,3)],[]).
rule([I:arg(mod,E,O),J:ins(O,fourth)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,4)],[]).
rule([I:arg(mod,E,O),J:ins(O,fifth)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,5)],[]).
rule([I:arg(mod,E,O),J:ins(O,fifth)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,5)],[]).
rule([I:arg(mod,E,O),J:ins(O,sixth)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,6)],[]).
rule([I:arg(mod,E,O),J:ins(O,seventh)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,7)],[]).
rule([I:arg(mod,E,O),J:ins(O,eighth)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,8)],[]).
rule([I:arg(mod,E,O),J:ins(O,ninth)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,9)],[]).
rule([I:arg(mod,E,O),J:ins(O,tenth)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,10)],[]).
rule([I:arg(mod,E,O),J:ins(O,X)],[],[I:arg(ord,E,O),J:ins(O,'ordinal-entity'),[]:arg(value,O,Num)],
     [name(X,Codes), member(Suffix,["st","nd","rd","th"]), append(Int,Suffix,Codes), name(Num,Int), number(Num)]).

% adverbs
rule([_:arg(manner,E,V),I:ins(V,please)],[],[I:arg(polite,E,'+')],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,always)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,now)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,once)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,after)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,before)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,since)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,soon)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,then)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,today)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,ever)],[I:arg(time,E,A)],[]).    
rule([I:arg(manner,E,A)],[_:ins(A,day)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,already)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,recent)],[I:arg(time,E,A)],[]).     
rule([I:arg(manner,E,A)],[_:ins(A,sometimes)],[I:arg(frequency,E,A)],[]).     
rule([I:arg(at,E,A)],[_:ins(A,present)],[I:arg(time,E,A)],[]).     

% morpohlogy of adjectives and nouns
rule([I:ins(X,agricultural)],[],[I:ins(X,agriculture)],[]).
rule([I:ins(X,coastal)],[],[I:ins(X,coast)],[]).
rule([I:ins(X,commercial)],[],[I:ins(X,commerce)],[]).
rule([I:ins(X,cultural)],[],[I:ins(X,culture)],[]).
rule([I:ins(X,drastical)],[],[I:ins(X,drastic)],[]).
rule([I:ins(X,domestical)],[],[I:ins(X,domestic)],[]).
rule([I:ins(X,economic)],[],[I:ins(X,economy)],[]).
rule([I:ins(X,economical)],[],[I:ins(X,economy)],[]).
rule([I:ins(X,electronical)],[],[I:ins(X,electronics)],[]).
rule([I:ins(X,electrical)],[],[I:ins(X,electricity)],[]).
rule([I:ins(X,electromechanical)],[],[I:ins(X,electromechanics)],[]).
rule([I:ins(X,emphatical)],[],[I:ins(X,emphasis)],[]).
rule([I:ins(X,environmental)],[],[I:ins(X,environment)],[]).
rule([I:ins(X,experimental)],[],[I:ins(X,'experiment-01')],[]).
rule([I:ins(X,financial)],[],[I:ins(X,finance)],[]).
rule([I:ins(X,'non-financial')],[],[I:ins(X,finance),[]:arg(polarity,X,'-')],[]).
rule([I:ins(X,governmental)],[],[I:ins(X,government)],[]).
rule([I:ins(X,historical)],[],[I:ins(X,history)],[]).
rule([I:ins(X,industrial)],[],[I:ins(X,industry)],[]).
rule([I:ins(X,mechanical)],[],[I:ins(X,mechanics)],[]).
rule([I:ins(X,medical)],[],[I:ins(X,medicine)],[]).
rule([I:ins(X,meeting)],[],[I:ins(X,'meeting-03')],[]).
rule([I:ins(X,metallurgical)],[],[I:ins(X,metallurgy)],[]).
rule([I:ins(X,national)],[],[I:ins(X,nation)],[]).
rule([I:ins(X,natural)],[],[I:ins(X,'natural-03')],[]).
rule([I:ins(X,orbital)],[],[I:ins(X,'orbit-01')],[]).
rule([I:ins(X,pharmaceutical)],[],[I:ins(X,pharmacy)],[]).
rule([I:ins(X,political)],[],[I:ins(X,politics)],[]).
rule([I:ins(X,provincial)],[],[I:ins(X,province)],[]).
rule([I:ins(X,regional)],[],[I:ins(X,region)],[]).
rule([I:ins(X,racial)],[],[I:ins(X,race)],[]).
rule([I:ins(X,scientifical)],[],[I:ins(X,science)],[]).
rule([I:ins(X,social)],[],[I:ins(X,society)],[]).
rule([I:ins(X,statistics)],[],[I:ins(X,statistic)],[]).
rule([I:ins(X,statistical)],[],[I:ins(X,statistic)],[]).
rule([I:ins(X,specifical)],[],[I:ins(X,'specific-02')],[]).
rule([I:ins(X,technical)],[],[I:ins(X,technology)],[]).
rule([I:ins(X,technological)],[],[I:ins(X,technology)],[]).
rule([I:ins(X,theoretical)],[],[I:ins(X,theory)],[]).
rule([I:ins(X,territorial)],[],[I:ins(X,territory)],[]).
rule([I:ins(X,theatrical)],[],[I:ins(X,theater)],[]).
rule([I:ins(X,traditional)],[],[I:ins(X,tradition)],[]).


% time
rule([[I]:arg(after,X,Y)],[],[I:ins(A,after),[]:arg(time,X,A),[]:arg(op1,A,Y)],[name(I,Codes),append("a",Codes,NewCodes),name(A,NewCodes)]).
rule([[I]:arg(since,X,Y)],[],[I:ins(A,since),[]:arg(time,X,A),[]:arg(op1,A,Y)],[name(I,Codes),append("s",Codes,NewCodes),name(A,NewCodes)]).

% without
rule([I:arg(without,E,X)],[],[I:arg(manner,E,X),[]:arg(polarity,X,'-')],[]).

% fixing roles
rule([I:ins(E,'accord-00'),J:arg(to,E,P)],[],[I:ins(E,'say-01'),J:arg('ARG0',E,P)],[]).
rule([I:arg(manner,E,N),J:ins(N,never)],[],[I:arg(time,E,N),J:ins(N,ever),[]:arg(polarity,E,'-')],[]).     
rule([I:arg(like,E,N),J:ins(E,'be-00')],[],[I:arg('ARG1',E,N),J:ins(E,'resemble-01')],[]).     
rule([I:arg(in,E,L)],[_:ins(L,'location')],[I:arg(location,E,L)],[]).     
rule([I:arg(rel,E,L)],[_:ins(L,'location')],[I:arg(location,E,L)],[]).     
rule([I:arg(manner,E,L)],[_:ins(L,'here')],[I:arg(location,E,L)],[]).     
rule([I:arg(rel,E,L)],[_:ins(L,'date-entity')],[I:arg(time,E,L)],[]).     
rule([I:arg(on,E,L)],[_:ins(L,'date-entity')],[I:arg(time,E,L)],[]).     
rule([I:arg(in,E,L)],[_:ins(L,'date-entity')],[I:arg(time,E,L)],[]).     
rule([_:arg(time,D,X),_:ins(X,'date-entity'),I:arg(R,X,Y)],[_:ins(D,'date-entity')],[I:arg(R,D,Y)],[]).
rule([_:arg(poss,D,X),_:ins(X,'date-entity'),I:arg(year,X,Y)],[_:ins(D,'date-entity')],[I:arg(year,D,Y)],[]).

% date intervals
rule([[I]:arg(from,F,X),_:arg(to,F,Y)],[_:ins(X,'date-entity'),_:ins(Y,'date-entity')],[I:arg(time,F,D),[]:ins(D,'date-interval'),[]:arg('op1',D,X),[]:arg('op2',D,Y)],[name(I,Codes),name(D,[110|Codes])]).

% imperative
rule([I:arg(manner,E,V),_:ins(V,imperative)],[],[I:arg(mode,E,imperative)],[]).     

% sentence-initial "and" and "but"
rule([I:arg(manner,E,A)],[[1001]:ins(A,and)],[I:arg('op2-of',E,A)],[]).                        
rule([I:arg(manner,E,A),[1001]:ins(A,but)],[],[[1001]:ins(A,'contrast-01'),I:arg('ARG2-of',E,A)],[]).                        

% nested "and": tricky...
%rule([I:arg(op2,X,Y)],[_:ins(X,and),_:ins(Y,and)],[I:arg(nestedand,X,Y)],[]).

rule([_:arg(nestedand,X,Y),_:ins(Y,and),I1:arg(op1,Y,O1),I2:arg(op2,Y,O2),I3:arg(op3,Y,O3),I4:arg(op4,Y,O4),I5:arg(op5,Y,O5),I6:arg(op6,Y,O6),I7:arg(op7,Y,O7)],[],
                                       [I1:arg(op2,X,O1),I2:arg(op3,X,O2),I3:arg(op4,X,O3),I4:arg(op5,X,O4),I5:arg(op6,X,O5),I6:arg(op7,X,O6),I7:arg(op8,X,O7)],[]).

rule([_:arg(nestedand,X,Y),_:ins(Y,and),I1:arg(op1,Y,O1),I2:arg(op2,Y,O2),I3:arg(op3,Y,O3),I4:arg(op4,Y,O4),I5:arg(op5,Y,O5),I6:arg(op6,Y,O6)],[],
                                       [I1:arg(op2,X,O1),I2:arg(op3,X,O2),I3:arg(op4,X,O3),I4:arg(op5,X,O4),I5:arg(op6,X,O5),I6:arg(op7,X,O6)],[]).

rule([_:arg(nestedand,X,Y),_:ins(Y,and),I1:arg(op1,Y,O1),I2:arg(op2,Y,O2),I3:arg(op3,Y,O3),I4:arg(op4,Y,O4),I5:arg(op5,Y,O5)],[],
                                       [I1:arg(op2,X,O1),I2:arg(op3,X,O2),I3:arg(op4,X,O3),I4:arg(op5,X,O4),I5:arg(op6,X,O5)],[]).

rule([_:arg(nestedand,X,Y),_:ins(Y,and),I1:arg(op1,Y,O1),I2:arg(op2,Y,O2),I3:arg(op3,Y,O3),I4:arg(op4,Y,O4)],[],
                                       [I1:arg(op2,X,O1),I2:arg(op3,X,O2),I3:arg(op4,X,O3),I4:arg(op5,X,O4)],[]).

rule([_:arg(nestedand,X,Y),_:ins(Y,and),I1:arg(op1,Y,O1),I2:arg(op2,Y,O2),I3:arg(op3,Y,O3)],[],
                                       [I1:arg(op2,X,O1),I2:arg(op3,X,O2),I3:arg(op4,X,O3)],[]).

rule([_:arg(nestedand,X,Y),_:ins(Y,and),I1:arg(op1,Y,O1),I2:arg(op2,Y,O2)],[],
                                       [I1:arg(op2,X,O1),I2:arg(op3,X,O2)],[]).

% X / and
%  :op1 ...
%  :op2 (Y / and             :op2 (O1 /  )
%          :op1 (O1 /  )     :op3 (O2 
%          :op2 (O2 /  )

% discourse relations
rule([I:arg(op1,X,Y)],[_:ins(X,'contrast-01')],[I:arg('ARG1',X,Y)],[]).
rule([I:arg(op2,X,Y)],[_:ins(X,'contrast-01')],[I:arg('ARG2',X,Y)],[]).
rule([I:arg(op1,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt1',X,Y)],[]).
rule([I:arg(op2,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt2',X,Y)],[]).
rule([I:arg(op3,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt3',X,Y)],[]).
rule([I:arg(op4,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt4',X,Y)],[]).
rule([I:arg(op5,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt5',X,Y)],[]).
rule([I:arg(op6,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt6',X,Y)],[]).
rule([I:arg(op7,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt7',X,Y)],[]).
rule([I:arg(op8,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt8',X,Y)],[]).
rule([I:arg(op9,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt9',X,Y)],[]).
rule([I:arg(op10,X,Y)],[_:ins(X,'multi-sentence')],[I:arg('snt10',X,Y)],[]).
rule([I:ins(X,continuation)],[],[I:ins(X,'multi-sentence')],[]).

% there-insertion, pleonastic it
rule([_:ins(X,there),_:arg(domain,_,X)],[],[],[]).
rule([_:ins(X,it),_:arg(domain,E,X)],[_:arg('ARG1',E,_)],[],[]).

% put eight names together
rule([[I8]:arg(name,X,A8),[I8]:ins(A8,name),[I8]:arg('op1',A8,N8),
      [I7]:arg(name,X,A7),[I7]:ins(A7,name),[I7]:arg('op1',A7,N7),
      [I6]:arg(name,X,A6),[I6]:ins(A6,name),[I6]:arg('op1',A6,N6),
      [I5]:arg(name,X,A5),[I5]:ins(A5,name),[I5]:arg('op1',A5,N5),
      [I4]:arg(name,X,A4),[I4]:ins(A4,name),[I4]:arg('op1',A4,N4),
      [I3]:arg(name,X,A3),[I3]:ins(A3,name),[I3]:arg('op1',A3,N3),
      [I2]:arg(name,X,A2),[I2]:ins(A2,name),[I2]:arg('op1',A2,N2),
      [I1]:arg(name,X,A1),[I1]:ins(A1,name),[I1]:arg('op1',A1,N1)],
     [],
     [[I1,I2,I3,I4,I5,I6,I7,I8]:arg(name,X,A1), 
      [I1,I2,I3,I4,I5,I6,I7,I8]:ins(A1,name),
      [I1,I2,I3,I4,I5,I6,I7,I8]:arg('op1',A1,N1), 
      [I1,I2,I3,I4,I5,I6,I7,I8]:arg('op2',A1,N2),
      [I1,I2,I3,I4,I5,I6,I7,I8]:arg('op3',A1,N3),
      [I1,I2,I3,I4,I5,I6,I7,I8]:arg('op4',A1,N4),
      [I1,I2,I3,I4,I5,I6,I7,I8]:arg('op5',A1,N5),
      [I1,I2,I3,I4,I5,I6,I7,I8]:arg('op6',A1,N6),
      [I1,I2,I3,I4,I5,I6,I7,I8]:arg('op7',A1,N7),
      [I1,I2,I3,I4,I5,I6,I7,I8]:arg('op8',A1,N8)],
     [I2 is I1+1,
      I3 is I2+1,
      I4 is I3+1,
      I5 is I4+1,
      I6 is I5+1,
      I7 is I6+1,
      I8 is I7+1,warning('put 8 names together',[])]).

% put seven names together
rule([[I7]:arg(name,X,A7),[I7]:ins(A7,name),[I7]:arg('op1',A7,N7),
      [I6]:arg(name,X,A6),[I6]:ins(A6,name),[I6]:arg('op1',A6,N6),
      [I5]:arg(name,X,A5),[I5]:ins(A5,name),[I5]:arg('op1',A5,N5),
      [I4]:arg(name,X,A4),[I4]:ins(A4,name),[I4]:arg('op1',A4,N4),
      [I3]:arg(name,X,A3),[I3]:ins(A3,name),[I3]:arg('op1',A3,N3),
      [I2]:arg(name,X,A2),[I2]:ins(A2,name),[I2]:arg('op1',A2,N2),
      [I1]:arg(name,X,A1),[I1]:ins(A1,name),[I1]:arg('op1',A1,N1)],
     [],
     [[I1,I2,I3,I4,I5,I6,I7]:arg(name,X,A1), 
      [I1,I2,I3,I4,I5,I6,I7]:ins(A1,name),
      [I1,I2,I3,I4,I5,I6,I7]:arg('op1',A1,N1), 
      [I1,I2,I3,I4,I5,I6,I7]:arg('op2',A1,N2),
      [I1,I2,I3,I4,I5,I6,I7]:arg('op3',A1,N3),
      [I1,I2,I3,I4,I5,I6,I7]:arg('op4',A1,N4),
      [I1,I2,I3,I4,I5,I6,I7]:arg('op5',A1,N5),
      [I1,I2,I3,I4,I5,I6,I7]:arg('op6',A1,N6),
      [I1,I2,I3,I4,I5,I6,I7]:arg('op7',A1,N7)],
     [I2 is I1+1,
      I3 is I2+1,
      I4 is I3+1,
      I5 is I4+1,
      I6 is I5+1,
      I7 is I6+1]).

% put six names together
rule([[I6]:arg(name,X,A6),[I6]:ins(A6,name),[I6]:arg('op1',A6,N6),
      [I5]:arg(name,X,A5),[I5]:ins(A5,name),[I5]:arg('op1',A5,N5),
      [I4]:arg(name,X,A4),[I4]:ins(A4,name),[I4]:arg('op1',A4,N4),
      [I3]:arg(name,X,A3),[I3]:ins(A3,name),[I3]:arg('op1',A3,N3),
      [I2]:arg(name,X,A2),[I2]:ins(A2,name),[I2]:arg('op1',A2,N2),
      [I1]:arg(name,X,A1),[I1]:ins(A1,name),[I1]:arg('op1',A1,N1)],
     [],
     [[I1,I2,I3,I4,I5,I6]:arg(name,X,A1), 
      [I1,I2,I3,I4,I5,I6]:ins(A1,name),
      [I1,I2,I3,I4,I5,I6]:arg('op1',A1,N1), 
      [I1,I2,I3,I4,I5,I6]:arg('op2',A1,N2),
      [I1,I2,I3,I4,I5,I6]:arg('op3',A1,N3),
      [I1,I2,I3,I4,I5,I6]:arg('op4',A1,N4),
      [I1,I2,I3,I4,I5,I6]:arg('op5',A1,N5),
      [I1,I2,I3,I4,I5,I6]:arg('op6',A1,N6)],
     [I2 is I1+1,
      I3 is I2+1,
      I4 is I3+1,
      I5 is I4+1,
      I6 is I5+1]).

% put five names together
rule([[I5]:arg(name,X,A5),[I5]:ins(A5,name),[I5]:arg('op1',A5,N5),
      [I4]:arg(name,X,A4),[I4]:ins(A4,name),[I4]:arg('op1',A4,N4),
      [I3]:arg(name,X,A3),[I3]:ins(A3,name),[I3]:arg('op1',A3,N3),
      [I2]:arg(name,X,A2),[I2]:ins(A2,name),[I2]:arg('op1',A2,N2),
      [I1]:arg(name,X,A1),[I1]:ins(A1,name),[I1]:arg('op1',A1,N1)],
     [],
     [[I1,I2,I3,I4,I5]:arg(name,X,A1), 
      [I1,I2,I3,I4,I5]:ins(A1,name),
      [I1,I2,I3,I4,I5]:arg('op1',A1,N1), 
      [I1,I2,I3,I4,I5]:arg('op2',A1,N2),
      [I1,I2,I3,I4,I5]:arg('op3',A1,N3),
      [I1,I2,I3,I4,I5]:arg('op4',A1,N4),
      [I1,I2,I3,I4,I5]:arg('op5',A1,N5)],
     [I2 is I1+1,
      I3 is I2+1,
      I4 is I3+1,
      I5 is I4+1]).

% put four names together
rule([[I4]:arg(name,X,A4),[I4]:ins(A4,name),[I4]:arg('op1',A4,N4),
      [I3]:arg(name,X,A3),[I3]:ins(A3,name),[I3]:arg('op1',A3,N3),
      [I2]:arg(name,X,A2),[I2]:ins(A2,name),[I2]:arg('op1',A2,N2),
      [I1]:arg(name,X,A1),[I1]:ins(A1,name),[I1]:arg('op1',A1,N1)],
     [],
     [[I1,I2,I3,I4]:arg(name,X,A1), 
      [I1,I2,I3,I4]:ins(A1,name),
      [I1,I2,I3,I4]:arg('op1',A1,N1), 
      [I1,I2,I3,I4]:arg('op2',A1,N2),
      [I1,I2,I3,I4]:arg('op3',A1,N3),
      [I1,I2,I3,I4]:arg('op4',A1,N4)],
     [I2 is I1+1,
      I3 is I2+1,
      I4 is I3+1]).

% put three names together
rule([[I1]:arg(name,X,A1),[I1]:ins(A1,name),[I1]:arg('op1',A1,N1),
      [I2]:arg(name,X,A2),[I2]:ins(A2,name),[I2]:arg('op1',A2,N2),
      [I3]:arg(name,X,A3),[I3]:ins(A3,name),[I3]:arg('op1',A3,N3)],
     [],
     [[I1,I2,I3]:arg(name,X,A1), [I1,I2,I3]:ins(A1,name),
      [I1,I2,I3]:arg('op1',A1,N1), 
      [I1,I2,I3]:arg('op2',A1,N2),
      [I1,I2,I3]:arg('op3',A1,N3)],
     [I2 is I1+1,
      I3 is I2+1]).

% put two names together
rule([[I1]:arg(name,X,A1),[I1]:ins(A1,name),[I1]:arg('op1',A1,N1),
      [I2]:arg(name,X,A2),[I2]:ins(A2,name),[I2]:arg('op1',A2,N2)],
     [],
     [[I1,I2]:arg(name,X,A1), [I1,I2]:ins(A1,name),
      [I1,I2]:arg('op1',A1,N1), 
      [I1,I2]:arg('op2',A1,N2)],
     [I2 is I1+1]).


% time expressions
rule([_:arg(timex,T,date(_:_,[I]:Y,[J]:M,[K]:Y))],[],[I:arg(year,M,Y),J:arg(month,T,M),K:arg(day,T,Y)],[]).
rule([_:arg(timex,T,date(_:_,_:_,[J]:M,[K]:Y))],[],[J:arg(month,T,M),K:arg(day,T,Y)],[]).
rule([_:arg(timex,T,date(_:_,[I]:Y,[J]:M,_:_))],[],[I:arg(year,T,Y),J:arg(month,T,M)],[]).
rule([_:arg(timex,T,date(_:_,[I]:Y,_:_,_:_))],[],[I:arg(year,T,Y)],[]).
rule([_:arg(timex,T,date(_:_,_:_,[J]:M,_:_))],[],[J:arg(month,T,M)],[]).
rule([_:arg(timex,T,date(_:_,_:_,_:_,[K]:D))],[],[K:arg(day,T,D)],[]).

% fix months and days
rule([I:arg(month,X,'01')],[],[I:arg(month,X,1)],[]).
rule([I:arg(month,X,'02')],[],[I:arg(month,X,2)],[]).
rule([I:arg(month,X,'03')],[],[I:arg(month,X,3)],[]).
rule([I:arg(month,X,'04')],[],[I:arg(month,X,4)],[]).
rule([I:arg(month,X,'05')],[],[I:arg(month,X,5)],[]).
rule([I:arg(month,X,'06')],[],[I:arg(month,X,6)],[]).
rule([I:arg(month,X,'07')],[],[I:arg(month,X,7)],[]).
rule([I:arg(month,X,'08')],[],[I:arg(month,X,8)],[]).
rule([I:arg(month,X,'09')],[],[I:arg(month,X,9)],[]).
rule([I:arg(day,X,'01')],[],[I:arg(day,X,1)],[]).
rule([I:arg(day,X,'02')],[],[I:arg(day,X,2)],[]).
rule([I:arg(day,X,'03')],[],[I:arg(day,X,3)],[]).
rule([I:arg(day,X,'04')],[],[I:arg(day,X,4)],[]).
rule([I:arg(day,X,'05')],[],[I:arg(day,X,5)],[]).
rule([I:arg(day,X,'06')],[],[I:arg(day,X,6)],[]).
rule([I:arg(day,X,'07')],[],[I:arg(day,X,7)],[]).
rule([I:arg(day,X,'08')],[],[I:arg(day,X,8)],[]).
rule([I:arg(day,X,'09')],[],[I:arg(day,X,9)],[]).


% remove multiple instances 
rule([_:ins(X,':')],[_:ins(X,_)],[],[]).         % colon
rule([_:ins(X,namenam)],[_:ins(X,_)],[],[]).         % named entity
rule([_:ins(X,nameper)],[_:ins(X,_)],[],[]).         % named entity person
rule([_:ins(X,namegeo)],[_:ins(X,_)],[],[]).         % named entity location
rule([_:ins(X,nameorg)],[_:ins(X,_)],[],[]).         % named entity organization
rule([_:ins(X,nameart)],[_:ins(X,_)],[],[]).         % named entity product
rule([_:ins(X,nameeve)],[_:ins(X,_)],[],[]).         % named entity event
rule([_:ins(X,nametim)],[_:ins(X,_)],[],[]).         % named entity time
rule([_:ins(X,manner)],[_:ins(X,_)],[],[]).          % how to P
rule([_:ins(X,thing)],[_:ins(X,_)],[],[]).           % introduced by replace/2
rule([_:ins(X,Same)],[_:ins(X,Same)],[],[]).         % double
rule([_:ins(X,Y)],[_:ins(X,_)],[],[warning('removed instance: ~p attached to ~p',[Y,X])]).
rule([_:arg(wiki,X,'-')],[_:arg(wiki,X,'-')],[],[]).

% units of measurement (distance)
rule([[I]:ins(X,inch)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,inch),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,foot)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,foot),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,yard)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,yard),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,mile)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,mile),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,leaque)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,leaque),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,metre)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,meter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,meter)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,meter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,m)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,meter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,nanometre)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,nanometer),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,nanometer)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,nanometer),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,millimetre)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,millimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,millimeter)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,millimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,mm)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,millimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,centimetre)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,centimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,centimeter)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,centimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,cm)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,centimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,decimetre)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,decimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,decimeter)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,decimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,dm)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,decimeter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,kilometer)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,kilometer),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,kilometre)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,kilometer),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,km)],[_:arg(quant,X,_)],[[]:ins(X,'distance-quantity'),I:ins(Y,kilometer),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).

% units of measurement (monetary)
rule([[I]:ins(X,franc)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,franc),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,mark)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,mark),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,ruble)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,ruble),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,rupee)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,rupee),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,yen)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,yen),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,cent)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,cent),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,euro)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,euro),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,pound)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,pound),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,yuan)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,yuan),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,dollar)],[_:arg(quant,X,_)],[[]:ins(X,'monetary-quantity'),I:ins(Y,dollar),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).

% units of measurement (temporal)
rule([[I]:ins(X,millenium)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,millenium),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,century)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,century),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,decade)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,decade),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,year)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,year),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,month)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,month),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,week)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,week),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,day)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,day),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,night)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,night),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,minute)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,minute),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,second)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,second),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,hour)],[_:arg(quant,X,_)],[[]:ins(X,'temporal-quantity'),I:ins(Y,hour),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).

% units of measurement (mass)
rule([[I]:ins(X,kilogram)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,kilogram),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,kg)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,kilogram),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,milligram)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,milligram),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,mg)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,milligram),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,picogram)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,picogram),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,gram)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,gram),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,ton)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,ton),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,tonne)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,tonne),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,pound)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,pound),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,ounce)],[_:arg(quant,X,_)],[[]:ins(X,'mass-quantity'),I:ins(Y,ounce),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).

% units of measurement (volume)
rule([[I]:ins(X,gallon)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,gallon),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,barrel)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,barrel),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,bottle)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,bottle),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,cup)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,cup),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,'cubic-meter')],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,'cubic-meter'),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,litre)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,liter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,millilitre)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,milliliter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,milliliter)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,milliliter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,ml)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,milliliter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,centilitre)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,centiliter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,centiliter)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,centiliter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,cl)],[_:arg(quant,X,_)],[[]:ins(X,'volume-quantity'),I:ins(Y,centiliter),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).

% units of measurement (area)
rule([[I]:ins(X,hectare)],[_:arg(quant,X,_)],[[]:ins(X,'area-quantity'),I:ins(Y,hectare),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,acre)],[_:arg(quant,X,_)],[[]:ins(X,'area-quantity'),I:ins(Y,acre),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,'square-metre')],[_:arg(quant,X,_)],[[]:ins(X,'area-quantity'),I:ins(Y,'square-meter'),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,'square-meter')],[_:arg(quant,X,_)],[[]:ins(X,'area-quantity'),I:ins(Y,'square-meter'),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,'square-kilometre')],[_:arg(quant,X,_)],[[]:ins(X,'area-quantity'),I:ins(Y,'square-kilometer'),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,'square-kilometer')],[_:arg(quant,X,_)],[[]:ins(X,'area-quantity'),I:ins(Y,'square-kilometer'),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).
rule([[I]:ins(X,'square-foot')],[_:arg(quant,X,_)],[[]:ins(X,'area-quantity'),I:ins(Y,'square-foot'),[]:arg(unit,X,Y)],[name(I,L),name(Y,[117|L])]).


% percentage
rule([I:ins(X,percent),J:arg(quant,X,Y)],[],[I:ins(X,'percentage-entity'),J:arg(value,X,Y)],[]).

% us dollars
rule([[I]:arg(name,M,N),J:arg(wiki,M,'-')],[_:ins(N,name),_:arg(op1,N,'"us"'),_:ins(M,'monetary-quantity'),_:arg(unit,M,U),_:ins(U,dollar)],
     [[]:arg(mod,U,C),[]:ins(C,country),I:arg(name,C,N),J:arg(wiki,C,'"United_States"')],[name(I,Codes),name(C,[112|Codes])]).

% countries
rule([I:ins(C,location)],[_:arg(name,C,N),_:ins(N,name),_:arg(op1,N,Name)],[I:ins(C,Type)],[nationality(_,_,Country,_,Type),addQuotes(Country,Name)]).
rule([I:arg(wiki,X,'-')],[_:ins(X,Type),_:arg(name,X,N),_:ins(N,name),_:arg(op1,N,Name)],[I:arg(wiki,X,W)],[nationality(_,_,Country,Wiki,Type),addQuotes(Country,Name),addQuotes(Wiki,W)]).

% wikification
rule([I:arg(wiki,X,'-'),J:ins(X,Old)],[_:arg(op1,N,Name1),_:arg(op2,N,Name2),_:arg(op3,N,Name3),_:arg(op4,N,Name4),_:arg(op5,N,Name5),_:arg(op6,N,Name6),_:arg(name,X,N)],[I:arg(wiki,X,Wiki),J:ins(X,Type)],[wiki(Type,Wiki,[Name1,Name2,Name3,Name4,Name5,Name6]),\+ Type=Old, warning('added 6 wiki ~p ~p',[Type,Wiki])]).
rule([I:arg(wiki,X,'-'),J:ins(X,Old)],[_:arg(op1,N,Name1),_:arg(op2,N,Name2),_:arg(op3,N,Name3),_:arg(op4,N,Name4),_:arg(op5,N,Name5),_:arg(name,X,N)],[I:arg(wiki,X,Wiki),J:ins(X,Type)],[wiki(Type,Wiki,[Name1,Name2,Name3,Name4,Name5]),\+ Type=Old, warning('added 5 wiki ~p ~p',[Type,Wiki])]).
rule([I:arg(wiki,X,'-'),J:ins(X,Old)],[_:arg(op1,N,Name1),_:arg(op2,N,Name2),_:arg(op3,N,Name3),_:arg(op4,N,Name4),_:arg(name,X,N)],[I:arg(wiki,X,Wiki),J:ins(X,Type)],[wiki(Type,Wiki,[Name1,Name2,Name3,Name4]),\+ Type=Old, warning('added 4 wiki ~p ~p',[Type,Wiki])]).
rule([I:arg(wiki,X,'-'),J:ins(X,Old)],[_:arg(op1,N,Name1),_:arg(op2,N,Name2),_:arg(op3,N,Name3),_:arg(name,X,N),not(_:arg(op4,N,_))],[I:arg(wiki,X,Wiki),J:ins(X,Type)],[wiki(Type,Wiki,[Name1,Name2,Name3]),\+ Type=Old, warning('added 3 wiki ~p ~p',[Type,Wiki])]).
rule([I:arg(wiki,X,'-'),J:ins(X,Old)],[_:arg(op1,N,Name1),_:arg(op2,N,Name2),_:arg(name,X,N),not(_:arg(op3,N,_))],[I:arg(wiki,X,Wiki),J:ins(X,Type)],[wiki(Type,Wiki,[Name1,Name2]),\+ Type=Old]).
rule([I:arg(wiki,X,'-'),J:ins(X,Old)],[_:arg(op1,N,Name),_:arg(name,X,N),not(_:arg(op2,N,_))],[I:arg(wiki,X,Wiki),J:ins(X,Type)],[wiki(Type,Wiki,[Name]),\+ Type=Old]).

% modals
rule([I:arg('ARG1',E,X),J:ins(E,necessary)],[],[I:arg('ARG2',E,X),J:ins(E,'obligate-01')],[]).
rule([I:arg('ARG1',E,X),J:ins(E,obligatory)],[],[I:arg('ARG2',E,X),J:ins(E,'obligate-01')],[]).
rule([J:ins(E,permissible)],[_:arg('ARG1',E,_)],[J:ins(E,'permit-01')],[]).
rule([J:ins(E,possible)],[_:arg('ARG1',E,_)],[J:ins(E,'possible-01')],[]).
rule([J:ins(E,impossible)],[_:arg('ARG1',E,_)],[J:ins(E,'possible-01'),[]:arg('polarity',E,'-')],[]).
rule([J:ins(E,likely)],[_:arg('ARG1',E,_)],[J:ins(E,'likely-01')],[]).

% verbalization
rule([[I]:ins(X,Old)],[],[[I]:ins(X,New),[]:arg(Rel1,X,Y),[]:ins(Y,Ins1),[]:arg(Rel2,Y,Z),[]:ins(Z,Ins2)],[verbalize(Old,New,Rel1,Ins1,Rel2,Ins2),name(I,L),name(Y,[118|L]),name(Z,[119|L])]).
rule([[I]:ins(X,Old)],[],[[I]:ins(X,New),[]:arg(Rel,X,Y),[]:ins(Y,Ins)],[verbalize(Old,New,Rel,Ins),name(I,L),name(Y,[118|L])]).
rule([I:ins(X,Old)],[],[I:ins(X,New),[]:arg(polarity,X,'-')],[verbalize(Old,New,'polarity-')]).
rule([I:ins(X,Old)],[],[I:ins(X,New)],[verbalize(Old,New)]).

% affixes
rule([I:ins(X,Old)],[],[I:ins(X,New),[]:arg(polarity,X,'-')],[negprefix(_,Old,_,New),warning('added prefix ~p',[Old])]).
rule([I:ins(X,Old)],[],[I:ins(X,New),[]:arg(polarity,X,'-')],[negsuffix(_,Old,_,New),warning('added suffix ~p',[Old])]).
rule([[I]:ins(X,A)],[],[[I]:ins(X,Type),[]:arg(name,X,N),[]:ins(N,name),[]:arg(op1,N,Q),[]:arg(wiki,X,W)],[nationality(A,a,C,D,Type),name(I,L),name(N,[112|L]),addQuotes(C,Q),addQuotes(D,W)]).

% other replacements
rule([I:arg(of,X,Y)],[],[I:arg(poss,X,Y)],[]).
rule([I:ins(X,begin)],[],[I:ins(X,'begin-01')],[]).
rule([I:ins(X,end)],[],[I:ins(X,'end-01')],[]).
rule([I:ins(X,'etc.')],[],[I:ins(X,'et-cetera')],[]).
rule([I:ins(X,'etc')],[],[I:ins(X,'et-cetera')],[]).
rule([I:ins(X,'(')],[],[I:ins(X,lrb)],[]).
rule([I:ins(X,')')],[],[I:ins(X,rrb)],[]).
rule([I:ins(X,':')],[],[I:ins(X,and)],[]).
rule([I:ins(X,';')],[],[I:ins(X,and)],[]).
rule([I:ins(X,',')],[],[I:ins(X,and)],[]).
rule([I:ins(X,'/')],[],[I:ins(X,slash)],[]).
rule([I:ins(X,'/-01')],[],[I:ins(X,slash)],[]).
rule([I:ins(X,namenam)],[],[I:ins(X,thing)],[]).
rule([I:ins(X,nameper)],[],[I:ins(X,person)],[]).
rule([I:ins(X,namegeo)],[],[I:ins(X,location)],[]).
rule([I:ins(X,nameart)],[],[I:ins(X,product)],[]).
rule([I:ins(X,nameeve)],[],[I:ins(X,event)],[]).
rule([I:ins(X,nameorg)],[],[I:ins(X,organization)],[]).
rule([I:ins(X,nametim)],[],[I:ins(X,time)],[]).
rule([I:arg(':',X,Y)],[],[I:arg(mod,X,Y)],[]).
rule([I:arg('-',X,Y)],[],[I:arg(mod,X,Y)],[]).
rule([I:arg('--',X,Y)],[],[I:arg(mod,X,Y)],[]).
rule([I:ins(X,Old)],[],[I:ins(X,New)],[mfs(Old,New)]).
rule([I:ins(X,Old)],[],[I:ins(X,New)],[name(Old,Codes),append(Prefix,"-00",Codes),append(Prefix,"-01",NewCodes),name(New,NewCodes)]).

% role91
rule([[I]:ins(X,Role),J:arg(poss,X,Y)],[],[I:ins(X,person),[]:ins(E,Rel),[]:arg('ARG0-of',X,E),J:arg('ARG1',E,Y),[]:ins(Z,Role),[]:arg(Arg,E,Z)],[role91(Rel,Arg,Role),name(I,Codes),name(E,[110|Codes]),name(Z,[111|Codes])]).
rule([[I]:ins(X,Role),J:arg(for,X,Y)],[],[I:ins(X,person),[]:ins(E,Rel),[]:arg('ARG0-of',X,E),J:arg('ARG1',E,Y),[]:ins(Z,Role),[]:arg(Arg,E,Z)],[role91(Rel,Arg,Role),name(I,Codes),name(E,[110|Codes]),name(Z,[111|Codes])]).
rule([[I]:ins(X,Role)],[],[I:ins(X,person),[]:ins(E,Rel),[]:arg('ARG0-of',X,E),[]:ins(Z,Role),[]:arg(Arg,E,Z)],[role91(Rel,Arg,Role),name(I,Codes),name(E,[110|Codes]),name(Z,[111|Codes])]).

% fix etc.
%
rule([_:arg(rel,X,E),I:arg(example,S,X)],[[J]:ins(E,'et-cetera')],[I:arg(example,S,New),[]:ins(New,and),[]:arg(op1,New,X),[]:arg(op2,New,E)],[name(J,L),name(New,[97|L])]).

% remove wiki- (experimental)
%rule([_:arg(wiki,_,'-')],[],[],[]).



/* ========================================================================
   Translate DRSs into AMR formulas 
======================================================================== */

drs2amr(alfa(_,B1,B2),P,T1,T2,N,H):- !, 
   drs2amr(merge(B1,B2),P,T1,T2,N,H).

drs2amr(_:drs(_,Conds),P,T1,T3,N,H):- !, 
   conds2amr(Conds,P,[],T2,N,[],H),
   append(T1,T2,T3).

drs2amr(merge(B1,B2),P,T1,T3,N1-N3,H2):- !, 
   drs2amr(B1,P,T1,T2,N1-N2,_), 
   drs2amr(B2,P,T2,T3,N2-N3,H2).

drs2amr(sdrs([],Rel),P,T1,T2,N,Root):- !,
   conds2amr(Rel,P,T1,T2,N,[],Root).

drs2amr(sdrs([D|L],R),P,T1,T3,N1-N3,H):- !,
   drs2amr(D,P,T1,T2,N1-N2,_),
   drs2amr(sdrs(L,R),P,T2,T3,N2-N3,H).

drs2amr(lab(K,B),P,T1,T4,N1-N2,K):- !,
   drs2amr(B,P,[],T2,N1-N2,H),
   replace([replace(H,K)|T2],T3),
   append(T1,T3,T4).

drs2amr(sub(B1,B2),P,T1,T3,N1-N3,H):-
   drs2amr(B1,P,T1,T2,N1-N2,H),
   drs2amr(B2,P,T2,T3,N2-N3,_).


/* ========================================================================
   Translate DRS-Conditions into AMR formulas 
======================================================================== */
  
conds2amr([],_,T1,T3,N-N,Heads,Root):- 
   getRoots(T1,Roots),
%  write(roots:Roots),nl,
%  write(amr:T1),nl,
   determineRoot(T1,T3,Roots,Heads,Root), !.

conds2amr([],_,T,T,N-N,Heads,Root):- 
   pickHead(Heads,Root), !,
   warning('cannot determine head: ~p',[T]).

conds2amr([_:Cond|L],P,T1,T3,N1-N3,Heads,Head):-
   cond2amr(Cond,P,T1,T2,N1-N2,E), 
   conds2amr(L,P,T2,T3,N2-N3,[E|Heads],Head).

  
/* ========================================================================
   Determine Root of DRS (and fix in case of multiple roots)
======================================================================== */

determineRoot(AMR,AMR,[Root],_,Root):-   % if there is one root and it is a
   checkInstance(Root,AMR,[]), !.        % proper root, then we are happy.

determineRoot(AMR,AMR,[Root],_,Root):-   % one
   warning('proposed root ~p not covering all vertices',[Root]), !.

determineRoot(AMR1,AMR2,Roots,_Heads,Root):-
   connected(AMR1), !,
   select(Root,Roots,FixRoots),
%  write(trying:Root),nl,
   inverseRoles(FixRoots,AMR1,AMR2), 
%  write(check:AMR2),nl,
   checkInstance(Root,AMR2,[]), !.

determineRoot(AMR,AMR,[Root|_],_Heads,Root):-
   connected(AMR), !,
   warning('connected but unable to inverse roles',[]).

determineRoot(AMR1,AMR2,Roots,Heads,Root):-
   Roots=[R1,R2], !,
   (member(_:[Num]:_,Heads),!;Num=1),
   name(Num,Codes), append("root",Codes,RootCodes), name(Root,RootCodes),
   warning('fixed multiple roots: ~p with new root ~p',[Roots,Root]),
   AMR2=[[]:ins(Root,'multi-sentence'),[]:arg(snt1,Root,R1),[]:arg(snt2,Root,R2)|AMR1].

determineRoot(AMR1,AMR2,Roots,Heads,Root):-
   Roots=[R1,R2,R3], !,
   (member(_:[Num]:_,Heads),!;Num=1),
   name(Num,Codes), append("root",Codes,RootCodes), name(Root,RootCodes),
   warning('fixed multiple roots: ~p',[Roots]),
   AMR2=[[]:ins(Root,'multi-sentence'),[]:arg(snt1,Root,R1),[]:arg(snt2,Root,R2),[]:arg(snt3,Root,R3)|AMR1].

determineRoot(AMR,AMR,Roots,_,Root):-
   Roots=[Root|_], !,
   warning('unable to fix multiple roots: ~p',[Roots]).


/* ========================================================================
   Check if AMR is connected
======================================================================== */

connected(AMR):-
   countVertices(AMR,[],N),
   member(_:ins(X,_),AMR),
   reachable(X,AMR,[],Reachable),
   length(Reachable,N), !.

%connected(A):-
%   warning('not connected: ~o',[A]).


/* ========================================================================
   Make a guess as to what the head of a list of DRS-conditions is...
======================================================================== */

pickHead(Heads,Event):- member(closing:_:Event,Heads), !.
pickHead(Heads,Event):- member(event:[_]:Event,Heads), !.
pickHead(Heads,Event):- member(noun:[_]:Event,Heads), !.
pickHead(Heads,Event):- member(named:[_]:Event,Heads), !.
pickHead([_:[_]:Head|_],Head):- !.
pickHead([_:_:Head|_],Head):- !.
pickHead(_,_).


/* ========================================================================
   Translate a DRS-Condition into AMR formulas 
======================================================================== */

cond2amr(I:nec(Drs),P,T1,[I:ins(E1,'recommend-01'),I:arg('ARG1',E1,E2)|T2],N1-N3,complex:I:E1):-
   I=[Index], member(Index:F,P), member(tok:should,F), !,
   label(N1,e,E1,N2), 
   drs2amr(Drs,P,T1,T2,N2-N3,E2).

cond2amr(I:nec(Drs),P,T1,[I:ins(E1,'obligate-01'),I:arg('ARG2',E1,E2)|T2],N1-N3,complex:I:E1):- !,
   label(N1,e,E1,N2), 
   drs2amr(Drs,P,T1,T2,N2-N3,E2).

cond2amr(I:pos(Drs),P,T1,[I:ins(E1,'possible-01'),I:arg('ARG1',E1,E2)|T2],N1-N3,complex:I:E1):- !,
   label(N1,e,E1,N2), 
   drs2amr(Drs,P,T1,T2,N2-N3,E2).

cond2amr(I:not(Drs),P,T1,[I:arg(polarity,E,'-')|T2],N1-N2,complex:I:E):- !,
   drs2amr(Drs,P,T1,T2,N1-N2,E).

cond2amr(I:prop(E,Drs),P,T1,T4,N,complex:I:E):- !,
   drs2amr(Drs,P,[],T2,N,H),
   append(T1,T2,T3),
   replace([replace(H,E)|T3],T4).

cond2amr(I:or(Drs1,Drs2),P,T1,[I:ins(E,or),I:arg('op1',E,H1),I:arg('op2',E,H2)|T3],N1-N4,complex:I:E):- !,
   label(N1,e,E,N2),
   drs2amr(Drs1,P,T1,T2,N2-N3,H1),
   drs2amr(Drs2,P,T2,T3,N3-N4,H2).

cond2amr(I:imp(Drs1,Drs2),P,T1,[I:ins(E,condition),I:arg('op1',E,H1),I:arg('op2',E,H2)|T3],N1-N4,complex:I:E):- !,
   label(N1,e,E,N2),
   drs2amr(Drs1,P,T1,T2,N2-N3,H1),
   drs2amr(Drs2,P,T2,T3,N3-N4,H2).

cond2amr(I:whq(Drs1,Drs2),P,T1,[I:ins(E,question),I:arg('op1',E,H1),I:arg('op2',E,H2)|T3],N1-N4,complex:I:E):- !,
   label(N1,e,E,N2),
   drs2amr(Drs1,P,T1,T2,N2-N3,H1),
   drs2amr(Drs2,P,T2,T3,N3-N4,H2).

cond2amr(I:duplex(_,Drs1,_,Drs2),P,T1,[I:ins(E,duplex),I:arg('op1',E,H1),I:arg('op2',E,H2)|T3],N1-N4,complex:I:E):- !,
   label(N1,e,E,N2),
   drs2amr(Drs1,P,T1,T2,N2-N3,H1),
   drs2amr(Drs2,P,T2,T3,N3-N4,H2).

cond2amr(I:timex(X,Date),_,T,[I:ins(X,'date-entity'),I:arg(timex,X,Date)|T],N1-N2,timex:I:E):- !, label(N1,e,E,N2).

cond2amr(I:named(X,S1,Sort,_),_,T,[I:ins(X,Sym),I:arg(name,X,N),I:ins(N,name),I:arg(op1,N,S2),I:arg(wiki,X,'-')|T],N1-N2,named:I:N):- !, 
   addQuotes(S1,S2), label(N1,n,N,N2), name(Sort,Codes), append("name",Codes,SymCodes), name(Sym,SymCodes).

cond2amr(I:card(X,C,_),_,T,[I:arg(quant,X,C)|T],N-N,card:I:X):- !.

cond2amr(I:eq(X,Y),_,T,[I:ins(E,'equal-01'),I:arg('ARG1',E,X),I:arg('ARG2',E,Y)|T],N1-N2,equal:I:E):- !, label(N1,e,E,N2).      

cond2amr(I:pred(X,S1,n,_),_,T,[I:ins(X,S2)|T],N-N,noun:I:X):- 
   name(S1,Codes), member(47,Codes), !, addQuotes(S1,S2).

cond2amr(I:pred(X,closing,v,99),_,T,T,N-N,closing:I:X):- !.
cond2amr(I:pred(X,interrogative,r,1),_,T,[I:arg(mode,X,'interrogative')|T],N-N,mod:I:X):- !.
cond2amr(I:pred(X,S,r,2),_,T,[I:ins(E,S),I:arg(mod,X,E)|T],N1-N2,mod:I:E):- !, label(N1,e,E,N2).
cond2amr(I:pred(X,S,r,_),_,T,[I:ins(E,S),I:arg(manner,X,E)|T],N1-N2,mod:I:E):- !, label(N1,e,E,N2).
cond2amr(I:pred(X,amount,n,3),_,T,[I:ins(E,much),I:arg(quant,X,E)|T],N1-N2,mod:I:E):- !, label(N1,e,E,N2).
cond2amr(I:pred(X,quantity,n,1),_,T,[I:ins(E,many),I:arg(quant,X,E)|T],N1-N2,mod:I:E):- !, label(N1,e,E,N2).
cond2amr(I:pred(X,S,n,_),_,T,[I:ins(X,S)|T],N-N,noun:I:X):- !.
cond2amr(I:pred(E,S1,v,_),_,T,[I:ins(E,S2)|T],N-N,event:I:E):- !, atom_concat(S1,'-00',S2).
cond2amr(I:pred(E,S,a,_),_,T,[I:ins(E,S)|T],N-N,mod:I:E):- !.

%cond2amr(I:rel(X,Y,subset_of1,1),_,T,[I:ins(Y,and),I:arg('op1',Y,X)|T],N-N,rel:I:X):- !.
%cond2amr(I:rel(X,Y,subset_of2,1),_,T,[I:ins(Y,and),I:arg('op1',Y,X)|T],N-N,rel:I:X):- !.
cond2amr(I:rel(X,Y,of,1),_,T,[I:arg(poss,X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:rel(X,Y,'Holder',_),_,T,[I:arg(domain,X,Y)|T],N-N,rel:I:Y):- !.
cond2amr(I:rel(X,Y,'Actor',_),_,T,[I:arg('ARG0',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:rel(X,Y,'Theme',_),_,T,[I:arg('ARG1',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:rel(X,Y,'Holder',_),_,T,[I:arg('ARG1',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:rel(X,Y,'Topic',_),_,T,[I:arg('ARG1',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:rel(X,Y,'Recipient',_),_,T,[I:arg('ARG2',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:rel(X,Y,P,_),_,T,[I:arg(P,X,Y)|T],N-N,rel:I:X):- !.

cond2amr(I:role(X,Y,'Holder',    1),_,T,[I:arg('domain',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Actor',     1),_,T,[I:arg('ARG0',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Theme',     1),_,T,[I:arg('ARG1',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Pivot',     1),_,T,[I:arg('ARG1',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Topic',     1),_,T,[I:arg('ARG1',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Recipient', 1),_,T,[I:arg('ARG2',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Time',      1),_,T,[I:arg('time',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Location',  1),_,T,[I:arg('location',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Manner',    1),_,T,[I:arg('manner',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Pivot',    -1),_,T,[I:arg('domain-of',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Holder',   -1),_,T,[I:arg('domain-of',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Actor',    -1),_,T,[I:arg('ARG0-of',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Theme',    -1),_,T,[I:arg('ARG1-of',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Topic',    -1),_,T,[I:arg('ARG1-of',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Recipient',-1),_,T,[I:arg('ARG2-of',X,Y)|T],N-N,rel:I:X):- !.
cond2amr(I:role(X,Y,'Time',     -1),_,T,[I:arg('time-of',X,Y)|T],N-N,rel:I:X):- !.
%cond2amr(I:role(X,Y,Role,       1),_,T,[I:arg(Role,X,Y)|T],N-N,rel:I:X):- !.
%cond2amr(I:role(X,Y,Role,      -1),_,T,[I:arg(Role,X,Y)|T],N-N,rel:I:X):- !.

cond2amr(I:X,_,T,T,N-N,unknown:I:_):- warning('cond2amr/3 failed for ~p',[X]).

cond2amr(rel(X,Y,P),_,T,[[]:ins(E,P),[]:arg('op1',E,X),[]:arg('op2',E,Y)|T],N1-N2,rel:[]:E):- !, label(N1,e,E,N2).


/* ========================================================================
   Adding double quotes for names
======================================================================== */

addQuotes(Name,QuotedName):-
   name(Name,NameCodes),
   append([34|NameCodes],[34],QuotedCodes),
   name(QuotedName,QuotedCodes).


/* ========================================================================
   Print AMR formula (assumes unique head)
======================================================================== */

getRoot(AMR,E):- 
   member(_:ins(E,_),AMR), 
   \+ member(_:arg(_,_,E),AMR).

getRoot(AMR,E):- 
   member(_:arg(_,E,_),AMR), 
   \+ member(_:ins(E,_), AMR),
   \+ member(_:arg(_,_,E),AMR).

getRoots(AMR,Roots):-
   findall(H,getRoot(AMR,H),Hs),
   sort(Hs,Roots).


/* ========================================================================
   Check AMR
======================================================================== */

checkInstance(E,AMR1,AMR3):- select(_:ins(E,_),AMR1,AMR2), !, checkInstance(E,AMR2,AMR3).
checkInstance(E,AMR1,AMR2):- checkArgs(E,AMR1,AMR2).

checkArgs(E,AMR1,AMR4):- select(_:arg(_,E,X),AMR1,AMR2), !, checkInstance(X,AMR2,AMR3), checkArgs(E,AMR3,AMR4).
checkArgs(_,AMR,AMR).


/* ========================================================================
   Print AMR formula (assumes unique head)
======================================================================== */

printAMR(AMR0,Stream):- 
   sort(AMR0,AMR1),
   getRoot(AMR1,Head), !,
   printInstance(Head,AMR1,AMR2,5,Stream),
   nl(Stream),
   ( AMR2=[]; AMR2\=[], warning('parts of AMR not printed: ~p',[AMR2]) ).

printAMR(_,Stream):- 
   warning('cannot print AMR',[]),
   BackupAMR=[[]:ins(x,and)],
   printAMR(BackupAMR,Stream).


/* ========================================================================
   Print AMR instance
======================================================================== */

printInstance(E,AMR1,AMR4,Level,Stream):-
   select(_:ins(E,S),AMR1,AMR2), !,
   format(Stream,'(~p / ~p',[E,S]),
   sortArgs(AMR2,E,AMR3),
   printArgs(E,AMR3,AMR4,Level,Stream).

printInstance(E,AMR1,AMR2,_Level,Stream):-
   format(Stream,'~p',[E]),
   AMR1=AMR2.


/* ========================================================================
   Place wiki before name relation
======================================================================== */

sortArgs(AMR1,E,[I:arg(wiki,E,X)|AMR3]):-
   select(I:arg(wiki,E,X),AMR1,AMR2), !,
   sortArgs(AMR2,E,AMR3).

sortArgs(AMR1,E,[I:arg(name,E,X)|AMR3]):-
   select(I:arg(name,E,X),AMR1,AMR2), !,
   sortArgs(AMR2,E,AMR3).

sortArgs(A,_,A).


/* ========================================================================
   Print AMR arguments of an instance
======================================================================== */

printArgs(E,AMR1,AMR4,Level,Stream):-
   select(_:arg(S,E,X),AMR1,AMR2), !,
   nl(Stream), tab(Stream,Level),
   format(Stream,':~p ',[S]),
   NewLevel is Level + 5,
   printInstance(X,AMR2,AMR3,NewLevel,Stream),
   printArgs(E,AMR3,AMR4,Level,Stream).

printArgs(_,AMR,AMR,_,Stream):- 
   write(Stream,')').
