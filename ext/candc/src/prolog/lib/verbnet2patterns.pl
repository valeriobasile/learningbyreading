
:- ['working/verbnet/temp.pl'].
:- use_module(library(lists),[reverse/2,append/3,member/2,select/3]).
:- dynamic vnpattern/4, role/2.

/* ----------------------------------------------------------------------
   Compute Patterns
---------------------------------------------------------------------- */ 

patterns:-
   init(Len1,Len2),
   findall(Ma,verbnet(_,_,_,Ma,_),Roles),
   freq(Roles),
   setof(V,Pr^Pa^Ma^ID^verbnet(V,Pr,Pa,Ma,ID),L),
   reverse(L,R),
   patterns(R,1,Len1,Len2),
   write('% note: this file was created by "make working/verbnet/verbnet.pl"'),nl,nl,
   write(':- module(verbnet,[vnpattern/3]).'),nl,nl,
   factors(Max),
   printpatterns(Max),
   format('% max: ~p~n',[Max]),
   halt.


/* ----------------------------------------------------------------------
   Init VerbNet ID
---------------------------------------------------------------------- */ 

init(Len1,Len2):-
   lensepid('.',Len1),
   lensepid('-',Len2).

lensepid(Sep,Len):-   
   verbnet(_,_,_,_,ID),
   count(ID,Sep,Max),
   \+ (verbnet(_,_,_,_,ID2), \+ ID=ID2, count(ID2,Sep,Higher), Higher > Max), !,
   findall(Sep,member(Sep,ID),Seps),
   length(Seps,Len).

count([],_,0):- !.
count([X|L],X,N):- !, count(L,X,M), N is M + 1.
count([_|L],X,N):- count(L,X,N).


/* ----------------------------------------------------------------------
   Compute frequencies of roles
---------------------------------------------------------------------- */ 

freq([]).
freq([Ma|L]):- roleFreq(Ma), freq(L).

roleFreq([]).
roleFreq([_:Role|L]):- !, addRole(Role), roleFreq(L).
roleFreq([_|L]):- roleFreq(L).

addRole(R):- role(R,N), !, retract(role(R,_)), M is N + 1, assert(role(R,M)).
addRole(R):- assert(role(R,1)).


/* ----------------------------------------------------------------------
   Compute VerbNet Id
---------------------------------------------------------------------- */ 

computeID([X],[Max],Factor,X):- !,
   (Max > 9, !, Factor = 100; Factor = 10).

computeID([X|L1],[Max|L2],NewPow,Num):-
   computeID(L1,L2,Pow,Temp), 
   Num is (Pow*X)+Temp,
   (Max > 9, !, Factor = 100; Factor = 10),
   NewPow is (Pow * Factor).


/* ----------------------------------------------------------------------
   Compute Factors for VerbNet ID
---------------------------------------------------------------------- */ 

factors(Max):-
   findall(I,vnpattern(_,_,_,I),L),
   max(L,[],0,Max), !.

max([],[],Max,[Max]):- !.

max([],Acc,Max,[Max|L]):- !,
   max(Acc,[],0,L).

max([[X]|L1],L2,High,Max):- !,
   (X > High, !, New = X; New = High),
   max(L1,L2,New,Max).

max([[X|N1]|L1],L2,High,Max):-
   (X > High, !, New = X; New = High),
   max(L1,[N1|L2],New,Max).


/* ----------------------------------------------------------------------
   Print Patterns
---------------------------------------------------------------------- */ 

printpatterns(Max):-
   vnpattern(Verb,Pat,PreMap,NumList),   
   map(PreMap,Map),
   computeID(NumList,Max,_,Num),
   format('~q. %%% ~p~n',[vnpattern(Verb,Num,Map),Pat]), 
   fail.

printpatterns(_).


/* ----------------------------------------------------------------------
   Format ID
---------------------------------------------------------------------- */ 

formatid(['.'|L1],N1,N,L2):- !,
   N2 is N1 - 1,
   formatid(L1,N2,N,L2).

formatid(['-'|L1],0,N1,L2):- !,
   N2 is N1 - 1,
   formatid(L1,0,N2,L2).

formatid(['-'|L1],N1,N,[0|L2]):- 
   N1 > 0, !,
   N2 is N1 - 1,
   formatid(['-'|L1],N2,N,L2).

formatid([X|L1],N1,N2,[X|L2]):- 
   formatid(L1,N1,N2,L2).

formatid([],N1,N,[0|L]):-
   N1 > 0, !,
   N2 is N1 - 1,
   formatid([],N2,N,L).

formatid([],N,N1,[0|L]):-
   N1 > 0, !,
   N2 is N1 - 1,
   formatid([],N,N2,L).

formatid([],_,_,[]).


/* ----------------------------------------------------------------------
   Mapping from Proto to VerbNet roles
---------------------------------------------------------------------- */ 

map(M1,[Prep:Role|M3]):- sublist([prep:Prep,np:Role],M1,M2), map(M2,M3), !.
map(M1,[Prep:Role|M3]):- sublist([prep:Prep,s:Role],M1,M2), map(M2,M3), !.
map(M1,[rel:Role|M3]):- select(pp:Role,M1,M2), map(M2,M3), !.

map([np:R1,v,np:R2,np:R3],[agent:R1,recipient:R2,theme:R3]):- !.
map([np:R1,v,np:R2,s:R3],[agent:R1,recipient:R2,theme:R3]):- !.
map([np:R1,v,np:R2],[agent:R1,patient:R2]):- !.
map([np:R1,v,s:R2],[agent:R1,theme:R2]):- !.
map([np:R1,v],[agent:R1]):- !.
map([v,np:R1],[agent:R1]):- !.

% map(X,_):- format('% ~p~n',[X]), fail.


/* ----------------------------------------------------------------------
   Probability of pattern, based on role frequency
---------------------------------------------------------------------- */ 

patprob([],0):- !.
patprob([_:R|L],Sum):- role(R,Tmp1), !, patprob(L,Tmp2), Sum is Tmp1 + Tmp2.
patprob([_|L],Sum):- patprob(L,Sum).


/* ----------------------------------------------------------------------
   Patterns
---------------------------------------------------------------------- */ 

patterns([],_,_,_):- !.

patterns([V|L],Len,Len1,Len2):-
   verbnet(V,XX,Pat,Map,Id),
   length(Pat,Len),
   patprob(Map,Score),
   \+ (verbnet(V,_,Pat1,Map1,_), length(Pat1,Len), patprob(Map1,Score1), Score1 < Score), !,
   formatid(Id,Len1,Len2,FId),
   addpat(V,Pat,Map,FId), 
   retract(verbnet(V,XX,Pat,Map,Id)),
   patterns([V|L],Len,Len1,Len2).

patterns([V|L],Len,Len1,Len2):-
   verbnet(V,_,Pat,_,_),
   length(Pat,Higher), Len < Higher,
   NewLen is Len + 1,
   patterns([V|L],NewLen,Len1,Len2).

patterns([_|L],_,Len1,Len2):- 
   patterns(L,1,Len1,Len2).


/* ----------------------------------------------------------------------
   Add Patterns to Database (longest one first)
---------------------------------------------------------------------- */ 

addpat(Verb,Pat,Map,Id):-
   member(prep:Prep,Map),
   atom_chars(Prep,Chars), 
   member(' ',Chars), !,
   split(Map,Map1,Map2),
   addpat(Verb,Pat,Map1,Id),
   addpat(Verb,Pat,Map2,Id).

addpat(Verb,Pat,Map,Id):-
   vnpattern(Verb,Pat,Map,Id), !.

addpat(Verb,Pat,Map,Id):-
   asserta(vnpattern(Verb,Pat,Map,Id)).


/* ----------------------------------------------------------------------
   Split Patterns
---------------------------------------------------------------------- */ 

split([],[],[]).

split([prep:X|L],[prep:X1|L],[prep:X2|L]):-
   atom_chars(X,Chars),
   append(First,[' '|Rest],Chars), !,
   atom_chars(X1,First),
   atom_chars(X2,Rest).

split([X|L],[X|L1],[X|L2]):-
   split(L,L1,L2).


/* ----------------------------------------------------------------------
   Sub List
---------------------------------------------------------------------- */ 

sublist(Sub,Old,New):- 
   append(Start,Rest,Old), 
   append(Sub,Temp,Rest), 
   append(Start,Temp,New).

/* ----------------------------------------------------------------------
   Self Starting
---------------------------------------------------------------------- */ 

:- patterns.

