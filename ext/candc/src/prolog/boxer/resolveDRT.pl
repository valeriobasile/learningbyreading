
:- module(resolveDRT,[resolveDRS/2,goldAntecedent/2]).

:- use_module(boxer(bindingViolation),[noBindingViolationDrs/1]).
:- use_module(boxer(freeVarCheck),[boundVarCheckContext/2,
                                   drsCondition/2]).
:- use_module(library(lists),[member/2,append/3,select/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2,gold/2]).
:- use_module(boxer(categories),[att/3]).
:- use_module(knowledge(antecedent),[extractFeaturesAna/2,
                                     extractFeaturesAnt/2,
                                     ana_ant_sort/3,
                                     ana_ant_symb/3,
                                     pos_ant/2,
                                     sentence_position_ant/2,
                                     same_sentence/4]).


/* ========================================================================
   Dynamic Predicate
======================================================================== */

:- dynamic antecedent/2.


/* ========================================================================
   Managing Gold Standard Antecedents
======================================================================== */

goldAntecedent(Indices,Att):-
   att(Att,antecedent,AntecedentIndex), 
   number(AntecedentIndex), !,
%  write(antecedent(Indices,AntecedentIndex)),nl,
   assert(antecedent(Indices,AntecedentIndex)).

goldAntecedent(_,_).


/* ========================================================================
   resolveDRS(+PDRS,     % Projective Discourse Representation Structure
              +T1-T2).   % Tags (token information)
======================================================================== */

resolveDRS(B,Tags):- 
   option('--resolve',true), !, 
   copy_term(Tags,L-[]), 
   setof(X:P,T^(member(X:T,L),member(pos:P,T)),IDs),
   resolvePDRS(B,[]-_,[]-_,IDs).

resolveDRS(_,_).


/* ========================================================================
   resolvePDRS(+PDRS,
               +C1-C2, % Context is a difference list of pointed DRSs
               +P1-P2, % Presuppositions
               +T1-T2) % Tags
======================================================================== */

resolvePDRS(sdrs([],_),C-C,P-P,_):- !.

resolvePDRS(sdrs([lab(_,B)|L],C),C1-C3,P1-P3,IDs):- !,
   resolvePDRS(B,C1-C2,P1-P2,IDs),
   resolvePDRS(sdrs(L,C),C2-C3,P2-P3,IDs).

resolvePDRS(sdrs([sub(B1,B2)|L],C),C1-C3,P1-P4,IDs):- !,
   resolvePDRS(B1,C1-C2,P1-P2,IDs),
   resolvePDRS(B2,C2-_,P2-P3,IDs),
   resolvePDRS(sdrs(L,C),C2-C3,P3-P4,IDs).

resolvePDRS(merge(B1,B2),C1-C3,P1-P3,IDs):- !,
   resolvePDRS(B1,C1-C2,P1-P2,IDs),
   resolvePDRS(B2,C2-C3,P2-P3,IDs).

resolvePDRS(lab(_,B),Context,P,IDs):- !,
   resolvePDRS(B,Context,P,IDs).

resolvePDRS(K:drs(D,C),C1-[K:drs(D,C)|C1],P1-P3,IDs):- !,
   getAnaphora(K:drs(D,C),C1,P1,[]-As),
   projectAnaphora(As,[K:drs(D,C)|C1],P1-P2,IDs),
   resolveConds(C,[K:drs(D,C)|C1],P2-P3,IDs).

resolvePDRS(U,C-C,P-P,_):- 
   warning('unknown DRS in resolvePDRS/4: ~p',[U]).


/* ========================================================================
   Project an ordered list of anaphoric DRSs
======================================================================== */

projectAnaphora([],_,P-P,_).

projectAnaphora([_I:K:B:Dep|As],C,P1-P3,IDs):-
    project(P1,C,K:B,P1-P2,C,Dep,[],IDs), !,
    projectAnaphora(As,C,P2-P3,IDs).

projectAnaphora([I2:K2:B2:Dep|As1],C,P1-P2,IDs):-
    select(I1:K1:B1:[],As1,As2), !,
    projectAnaphora([I1:K1:B1:[],I2:K2:B2:Dep|As2],C,P1-P2,IDs).

projectAnaphora([_:_:_:Dep|As],C,P1-P2,IDs):-
    warning('dependent variable in project/6 not found: ~p',[Dep]),
    projectAnaphora(As,C,P1-P2,IDs).


/* ========================================================================
   Resolve Conditions
======================================================================== */

resolveConds([],_,P-P,_):- !.

resolveConds([_:C|L],Context,P,IDs):- !, 
   resolveConds([C|L],Context,P,IDs).

resolveConds([not(B)|C],Context,P1-P3,IDs):- !,
   resolvePDRS(B,Context-_,P1-P2,IDs),
   resolveConds(C,Context,P2-P3,IDs).

resolveConds([nec(B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([pos(B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([prop(_,B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([imp(B1,B2)|C],C1,P1-P4,IDs):- !,
   resolvePDRS(B1,C1-C2,P1-P2,IDs),
   resolvePDRS(B2,C2-_,P2-P3,IDs),
   resolveConds(C,C1,P3-P4,IDs).

resolveConds([duplex(_,B1,_,B2)|C],Context,P,IDs):- !,
   resolveConds([imp(B1,B2)|C],Context,P,IDs).

resolveConds([or(B1,B2)|C],C1,P1-P4,IDs):- !,
   resolvePDRS(B1,C1-_,P1-P2,IDs),
   resolvePDRS(B2,C1-_,P2-P3,IDs),
   resolveConds(C,C1,P3-P4,IDs).

resolveConds([_|C],Context,P,IDs):- !,
   resolveConds(C,Context,P,IDs).


/* ========================================================================
   Identify Anaphoric Material (free pointers)
======================================================================== */

%getAnaphora(_:drs([],_),_,_,A1-A4):-
%   member(_:_:_:[_|_],A1),
%   member(_:_:_:[],A1), !,
%   setof(I:K:B:[],member(I:K:B:[],A1),A2),
%   setof(I:K:B:[D|L],member(I:K:B:[D|L],A1),A3),
%   append(A2,A3,A4).
getAnaphora(_:drs([],_),_,_,A1-A2):- sort(A1,A2).
getAnaphora(K:drs([F:_:_|Dom],Con),Context,Presups,As):- K==F, !, getAnaphora(K:drs(Dom,Con),Context,Presups,As).
getAnaphora(K:drs([F:_:_|Dom],Con),Context,Presups,As):- member(C:_,Context), C==F, !, getAnaphora(K:drs(Dom,Con),Context,Presups,As).
getAnaphora(K:drs([F:_:_|Dom],Con),Context,Presups,As):- member(P:_,Presups), P==F, !, getAnaphora(K:drs(Dom,Con),Context,Presups,As).
getAnaphora(K:drs([F:I:R|Dom],Con),Context,Presups,A1-A2):-
   anaphoricSet(Dom,F,FDom,I1),
   anaphoricSet(Con,F,FCon,I2),
   dependencies(FCon,[F:I:R|FDom],[]-Dependencies),
   append(I1,I2,Is),
   getAnaphora(K:drs(Dom,Con),Context,[F:_|Presups],[Is:F:drs([F:I:R|FDom],FCon):Dependencies|A1]-A2). 


/* ========================================================================
   Check dependencies
======================================================================== */

dependencies([],Dom,D1-D2):-
   checkDependencies(D1,Dom,D2).

dependencies([_:_:rel(X,Y,_,_)|L],Dom,D1-D2):- !,
   dependencies(L,Dom,[X,Y|D1]-D2).

dependencies([_:_:role(X,Y,_,_)|L],Dom,D1-D2):- !,
   dependencies(L,Dom,[X,Y|D1]-D2).

dependencies([_:_:eq(X,Y,_,_)|L],Dom,D1-D2):- !,
   dependencies(L,Dom,[X,Y|D1]-D2).

dependencies([_|L],Dom,D1-D2):-
   dependencies(L,Dom,D1-D2).


/* ========================================================================
   Find all dependencies (free variables bound outside the presupposition)
======================================================================== */

checkDependencies([],_,[]).

checkDependencies([X|L1],Dom,L2):-
   member(_:_:Y,Dom), X==Y, !,
   checkDependencies(L1,Dom,L2).

checkDependencies([X|L1],Dom,[X|L2]):-
   checkDependencies(L1,Dom,L2).


/* ========================================================================
   Check for bound variable
======================================================================== */

boundVar(X,Context):-
   member(P1:drs(Dom,_),Context),
   member(P2:_:Y,Dom),
   X==Y, P1==P2, !.


/* ========================================================================
   Compute Anaphoric Material
======================================================================== */

anaphoricSet([],_,[],[]).
anaphoricSet([P:[]:E|L1],F,[P:[]:E|L2],I):- P==F, !, anaphoricSet(L1,F,L2,I).
anaphoricSet([P:[I|L]:E|L1],F,[P:[I|L]:E|L2],[I]):- P==F, !, anaphoricSet(L1,F,L2,_).
anaphoricSet([_|L1],F,L2,I):- anaphoricSet(L1,F,L2,I).


/* ========================================================================
   Projection -- try to bind, else accommodate

   project(+List of presuppositions seen so far (could act as antecedents),
           +List of Context DRSs (Possible antecedents),
           +Anaphoric DRS,
           +Pair of Ingoing and Output List of Presuppositions
           +List of DRSs (local DRS + context DRS, to check for binding violations)
           +Dependencies (free variables in presupposition)
           -Accumulator of solution/4,
           -List of IDs to compute proximity)
======================================================================== */

% Try to match presupposed DRS as antecedent if there are no dependencies.
% 
project([K1:drs([K0:_:X|D],C)|P],Cs,K2:B2,P1-P2,Bs,[],Solutions,IDs):-
   K1 == K0,                                  % Antecedent DRS from presuppositions
   match(K0,C,X,B2,IDs,Bs,Y,Score,Ant), !,    % Match antecedent with anaphoric DRS
   project([K1:drs(D,C)|P],Cs,K2:B2,P1-P2,Bs,[],[solution(Score,K1:X,K2:Y,Ant)|Solutions],IDs).

% Found dependent variable in domain. Remove it from list.
%
project([K1:drs([_:_:X|D],C)|P],Cs,K2:B2,P1-P2,Bs,Deps1,Solutions,IDs):-
   select(Y,Deps1,Deps2), X == Y, !,
   project([K1:drs(D,C)|P],Cs,K2:B2,P1-P2,Bs,Deps2,Solutions,IDs).

% All other cases.
%
project([K1:drs([_|D],C)|P],Cs,K2:B2,P1-P2,Bs,Dep,Solutions,IDs):- !,
   project([K1:drs(D,C)|P],Cs,K2:B2,P1-P2,Bs,Dep,Solutions,IDs).

% Try next presupposed DRS
%
project([_|P],Cs,K,P1-P2,Bs,Dep,Solutions,IDs):- !,
   project(P,Cs,K,P1-P2,Bs,Dep,Solutions,IDs).

% No presupposed DRSs anymore. Add free accommodation to solutions.
%
project([],Cs,K,Ps,Bs,[],Solutions,IDs):- !,
   project(Cs,K,Ps,Bs,[],[solution(8,_:_,_:_,free)|Solutions],IDs).

% Continue with context-DRSs.
%
project([],Cs,K,Ps,Bs,Dep,Solutions,IDs):-
   project(Cs,K,Ps,Bs,Dep,Solutions,IDs).


% Match antecedent with anaphoric DRS (no dependent variables)
% 
project([K1:drs([K0:_:X|D],C)|Context],K2:B2,P1-P2,Bs,[],Solutions,IDs):-
   (K1==K0 ; member(K3:_,Context), K3==K0),
   match(K0,C,X,B2,IDs,Bs,Y,Score,Source), !,
   project([K1:drs(D,C)|Context],K2:B2,P1-P2,Bs,[],[solution(Score,K1:X,K2:Y,Source)|Solutions],IDs).

% Found dependent variable in domain. Remove it from list.
%
project([K1:drs([_:_:X|D],C)|Context],K2:B2,P1-P2,Bs,Deps1,Solutions,IDs):-      
   select(Y,Deps1,Deps2), X == Y, !,
   project([K1:drs(D,C)|Context],K2:B2,P1-P2,Bs,Deps2,Solutions,IDs).

% ALl other cases
%
project([K1:drs([_|D],C)|Context],A,P1-P2,Bs,Dep,Solutions,IDs):- !,
   project([K1:drs(D,C)|Context],A,P1-P2,Bs,Dep,Solutions,IDs).

% Tried all discourse referents. Add local accommodation to solutions.
%
project([K1:drs([],_)|Context],K2:B2,P1-P2,Bs,[],Solutions,IDs):- !,
%  length(Context,Levels), Prob is 0.01/(Levels + 1), Score is 1-Prob,
   length(Context,Levels), Score is 9+Levels,
   project(Context,K2:B2,P1-P2,Bs,[],[solution(Score,K1:_,K2:_,local)|Solutions],IDs).

% Try next context DRS (all other cases)
%
project([_|Context],A,P1-P2,Bs,Dep,Sol,IDs):- !,  % first argument can be an SDRS?
   project(Context,A,P1-P2,Bs,Dep,Sol,IDs).

% All context DRSs (and presupposed DRSs) have been considered.
% Pick most likely solution (the one with the best score)
%
project([],B,P1-P2,Bs,[],Solutions,_):- !,
   sort(Solutions,Sorted),  
%  write(solutions:Sorted),nl,
   best(Sorted,Bs,B,P1-P2), !.                                

%project([],B,Ps,Bs,[X|L],Solutions,IDs):-        
%   warning('dependent variable in project/6 not found: ~p',[X]), !,
%   project([],B,Ps,Bs,L,Solutions,IDs).


/* ========================================================================
   Best (sorted on score, the lower the better!)
======================================================================== */   

best([Solution|_],Bs,ADRS,P-[ADRS|P]):-         % DRS with free pointer
   Solution = solution(_Score,_,_,free),        % hence add to list of presuppositions
   append(Bs,[ADRS|P],Context),
   boundVarCheckContext(Context,ADRS), !.

best([Solution|_],Bs,ADRS,P-P):- 
   Solution = solution(_Score,X,Y,Reason),
   member(Reason,[local,global]),
   append(Bs,P,Context),
   \+ \+ (X=Y, boundVarCheckContext(Context,ADRS)), !, 
   X=Y.

best([Solution|_],Bs,ADRS,P1-P2):- 
   Solution = solution(_Score,X,Y,Reason),
   \+ member(Reason,[local,global,free]),
   append(Bs,P1,Context),
   \+ \+ (X=Y,                                  % if unifying X with Y does not
          boundVarCheckContext(Context,ADRS),   % yield any free variables
          noBindingViolationDrs(Bs)), !,        % or binding violations
   X=Y,                                         % then do so
   updatePresups(P1,ADRS,P2).

best([_|L],Bs,ADRS,P):- best(L,Bs,ADRS,P).


/* ========================================================================
   Update Presuppositions
======================================================================== */   

updatePresups([],_,[]).
updatePresups([K:drs(D1,C1)|L],P:drs(D2,C2),[K:drs(D4,C4)|L]):- P==K, !, append(D1,D2,D3), removeDuplicates(D3,D4), append(C1,C2,C3), removeDuplicates(C3,C4).
updatePresups([B|L1],P,[B|L2]):- updatePresups(L1,P,L2).

removeDuplicates([],[]).
removeDuplicates([X|L1],L2):- member(Y,L1), X==Y, !, removeDuplicates(L1,L2).
removeDuplicates([X|L1],[X|L2]):- removeDuplicates(L1,L2).


/* ========================================================================
   Check if there is gold standard data available
======================================================================== */   

goldAntecedentIndex(Conds,AnaInd,AntInd):- 
   antecedent(AnaInd,AntInd),               % there is a gold antecedent
   member( _:AnaInd:_,Conds), !.            % for the current anaphoric expression


/* ========================================================================
   Match antecedent with presupposition

   match(+Label of Antecedent DRS,
         +Conditions of Antecedent DRS,
         +Referent of Antecedent DRS,
         +Unlabeled Anaphoric DRS,
         +List of Token IDs,
         +List of Context DRSs
         -Referent of Anaphoric DRS,
         -Matching Score,
         -Matching Type)

======================================================================== */   

% There is a gold-standard antecedent available; take this as antecedent
%
match(K1,C1,X,drs([_:_:Y|_],C2),IDs,Bs,Y,0,bow):-
   goldAntecedentIndex(C2,I2,AntInd),           % check whether there is a gold label AntInd for one of the conditions of C1
   member(K2:I1:Ant,C1), K1==K2,                % get pointed condition with index I1 that belongs to antecedent DRS K1
   member(AntInd,I1),                           % this index I1 must contain the AntInd
   drsCondition(Z,Ant), Z==X, !,   
   refConditions(X,[K1:drs([],C1)|Bs],[]-XConds), 
   refConditions(Y,Bs,[]-YConds), 
   proximity(I1,I2,IDs,Prox,Pos),
   \+ \+ ( X=Y,numbervars(YConds,0,Co),
           numbervars(XConds,Co,_),
           gold('ana_ant(~q,~q,~p). % p(antecedent: ~p, anaphor: ~p, pos: ~q).',[YConds,XConds,Prox,I1,I2,Pos]) ).


% Old rule-based algorithm
%
% overall precision: 0.44 (2173/4848)
% overall recall: 0.44 (2173/4849)

match(K1,C1,X,drs(_,C2),_IDs,_Bs,Y,NewScore,P):-
   member( _:_:Ana,C2),
   member(K2:_:Ant,C1),          K1==K2, 
   matching(Y^Ana,Z^Ant,Score,P), Z==X,
   noConflicts(Y,C2,X,C1), !,
   NewScore is 1-Score.              % inverse score for sorting purposes


% Experimental version of pronoun resolution
%
newmatch(K1,C1,X1,drs([_:_:Y1|_],C2),IDs,Bs,Y1,Score,ana):-
%  option('--x',nottrue),                  % set to 'nottrue' to skip this work-in-progress clause
   member( _:I2:Ana,C2), \+ I2=[],         % get anaphor condition 
   drsCondition(Y2,Ana), Y1==Y2,           % and proper DRS condition
%   member(Pro,[male,female]), Ana=pred(_,Pro,_,_),
   member(K2:I1:Ant,C1), K1==K2, \+ I1=[], % get antecedent condition

%   sentence_position(I1,Sen1,_),
%   sentence_position(I2,Sen2,_),
%   SenDif is Sen2 - Sen1,  SenDif < 3,

   drsCondition(X2,Ant), X1==X2,           % make sure it really is an antecedent condition
   refConditions(X1,Bs,[]-Conds), 
   compute_score(I1:Conds,I2:[Ana],IDs,Score),
   noConflicts(Y1,C2,X1,C1), !.


/* ========================================================================
   Get Part-of-Speech given an index
======================================================================== */   

index2pos([I],L,P):- member(I:P,L), !.
index2pos([_,J|R],L,P):- index2pos([J|R],L,P).


/* ========================================================================
   Compute Score of Antecedent Candidate
======================================================================== */   

compute_score(I1:Ant,I2:Ana,IDs,Score):-
   index2pos(I1,IDs,POS),
   sentence_position(I1,Sen1,AntPos),
   sentence_position(I2,Sen2,AnaPos),
   ( Sen1=Sen2, AntPos < AnaPos; \+ Sen1=Sen2 ),
   extractFeaturesAna(Ana,[FAna|_]),
   extractFeaturesAnt(Ant,FAnt),
   %
   % Feature 1: Probability in sentence -N given token position pronoun
   %
   SenDif is Sen1-Sen2,
   same_sentence(FAna,AnaPos,SenDif,Prob1),
   %
   % Feature 2
   %
%   sentence_position_ant(AntPos,Prob2),
   %
   % Feature 3: Probability Part-of-Speech X of antecedent
   %
   pos_ant(POS,Prob3),
   %
   % Feature 4
   %
   material_in_common(Ant,Ana,Prob4),
   %
   % Feature 5
   %
   member(sort:Sort,FAnt), ana_ant_sort(FAna,Sort,Prob5),
   \+ (member(sort:Sort1,FAnt), ana_ant_sort(FAna,Sort1,Prob51), Prob51 < Prob5),
   %
   % Feature 6
   %
   member(symb:Symb,FAnt), 
   ana_ant_symb(FAna,Symb,Prob6),
   \+ (member(symb:Symb1,FAnt), ana_ant_symb(FAna,Symb1,Prob61), Prob61 < Prob6),
%  write(ana_ant_symb(FAna,Symb,Prob6)),nl,
   %
   % Combine Features
   %
%    Score is Prob1+Prob4. % 0.38 (1876/4826)
%    Score is Prob1+Prob3+Prob4. % 0.46 (2244/4826)
%    Score is Prob1+Prob4+Prob5. % 0.58 (2384/4827)
    Score is Prob1+Prob3+Prob4+Prob5. % 0.59 (2956/4946)
%    Score is Prob1+Prob3+Prob4. % 0.40 (2003/4957)
%    Score is Prob1+Prob3+Prob5+Prob6.  % 0.59




/* ========================================================================
   Check whether anaphor and antecedent have material in common
======================================================================== */   

material_in_common(_  ,Ana,0):- member(Pro,[female,male,thing]), member(pred(_,Pro,n,_),Ana), !.
%material_in_common(Ant,Ana,0):- \+ \+ (member(Same,Ant), member(Same,Ana)), !.
material_in_common(Ant,Ana,0):- member(pred(_,Symb,Sort,_),Ant), member(pred(_,Symb,Sort,_),Ana), !.
material_in_common(Ant,Ana,1):- member(pred(_,Symb,_,_),Ant), member(pred(_,Symb,_,_),Ana), !.
material_in_common(Ant,Ana,0):- member(named(_,Same,Sort,_),Ant), member(named(_,Same,Sort,_),Ana), !.
material_in_common(Ant,Ana,1):- member(named(_,Same,_,_),Ant), member(named(_,Same,_,_),Ana), !.
material_in_common(_  ,_  ,10).


/* ========================================================================
   Calculate Proximity
======================================================================== */   

proximity([_,Y|L1],L2,IDs,P,Pos):- !, proximity([Y|L1],L2,IDs,P,Pos).
proximity([X],[_,Z|L],IDs,P,Pos):- !, proximity([X],[Z|L],IDs,P,Pos).
proximity([X],[Y],IDs,P,Pos):- number(X), number(Y), X<Y, from(IDs,X,Y,P), member(X:Pos,IDs), !.
proximity(_  ,_  ,_  ,0,'UNK').

from([],_,_,0).
from([X:_|L],X,Y,D):- !, to(L,Y,0,D).
from([_|L],X,Y,D):- from(L,X,Y,D).

to([X:_|_],X,D1,D2):- !, D2 is D1 + 1.
to([_|L],X,D1,D2):- D is D1 + 1, to(L,X,D,D2).

sentence_position([X|_],Zin,Pos):-
   number(X),
   Pos is mod(X,1000),
   Zin is (X-Pos)/1000, !.


/* ========================================================================
   Get conditions for a specific discourse referent
======================================================================== */   

refConditions(X,[K:drs(D,C1)|L],L1-L2):-
   select(_:_:C,C1,C2), 
   member(C,[pred(Z,_,_,_),named(Z,_,_,_),role(_,Z,_,1),role(Z,_,_,-1)]), Z==X, !,
   refConditions(X,[K:drs(D,C2)|L],[C|L1]-L2).

refConditions(X,[K:drs(D,C1)|L],L1-L3):-
   select(_:_:eq(Z,Y),C1,C2), Z==X, !,
   refConditions(X,[K:drs(D,C2)|L],L1-L2),
   refConditions(Y,[K:drs(D,C2)|L],L2-L3).

refConditions(X,[_|L],L1-L2):- !, refConditions(X,L,L1-L2).
    
refConditions(_,[],L-L):- \+ L = [].


/* ========================================================================
   Check for Conflicts
======================================================================== */   

noConflicts(X,AnaConds,Y,AntConds):-                    
    \+ \+ ( X=Y,                                           % resolving must
            \+ ( member(_:_:not(_:drs(_,C0)),AntConds),    % not result in X=X
                 member(_:_:eq(A,B),C0),                   % in a negated DRS
                 A==X, B==X ),                             % and
            \+ ( member(_:_:pred(A,male,_,_),AnaConds),    % not result in
                 member(_:_:pred(B,female,_,_),AntConds),  % hermaphrodites
                 A==X, B==X ),                             
            \+ ( member(_:_:pred(A,female,_,_),AnaConds),
                 member(_:_:pred(B,male,_,_),AntConds),
                 A==X, B==X ) ).


/* ========================================================================
   Matching (anaphor, antecedent)
======================================================================== */   

% amr matching
matching(Y^pred(Y,i,n,1),  Z^pred(Z,i,n,1),  1.0,n:i  ):-    option('--semantics',amr).
matching(Y^pred(Y,you,n,1),Z^pred(Z,you,n,1),1.0,n:you):-    option('--semantics',amr).
matching(Y^pred(Y,he,n,1), Z^pred(Z,he,n,1), 1.0,n:he):-     option('--semantics',amr).
matching(Y^pred(Y,she,n,1),Z^pred(Z,she,n,1),1.0,n:she):-    option('--semantics',amr).
matching(Y^pred(Y,it,n,1),Z^pred(Z,it,n,1),1.0,n:she):-      option('--semantics',amr).
matching(Y^pred(Y,we,n,1),Z^pred(Z,we,n,1),1.0,n:she):-      option('--semantics',amr).
matching(Y^pred(Y,they,n,1),Z^pred(Z,they,n,1),1.0,n:she):-  option('--semantics',amr).

% time
matching(Y^pred(Y,now,a,1),Z^pred(Z,now,a,1),0.99,a:now).

% he
matching(Y^pred(Y,male,n,2),Z^named(Z,S,per,_),0.9,per:S).
matching(Y^pred(Y,male,n,2),Z^named(Z,S,_,_),0.1,per:S).
matching(Y^pred(Y,male,n,2),Z^pred(Z,male,n,2),0.99,n:male).
matching(Y^pred(Y,male,n,2),Z^pred(Z,S,n,_),0.5,n:S).
matching(Y^pred(Y,male,n,2),Z^card(Z,_,_),0.1,card).

% she
matching(Y^pred(Y,female,n,2),Z^named(Z,S,per,_),0.9,per:S).
matching(Y^pred(Y,female,n,2),Z^named(Z,S,_,_),0.1,per:S).
matching(Y^pred(Y,female,n,2),Z^pred(Z,female,n,2),0.99,n:female).
matching(Y^pred(Y,female,n,2),Z^pred(Z,S,n,_),0.5,n:S).
matching(Y^pred(Y,female,n,2),Z^card(Z,_,_),0.1,card).

% it
matching(Y^pred(Y,neuter,a,_),Z^named(Z,S,per,_),0.1,per:S).
matching(Y^pred(Y,neuter,a,_),Z^named(Z,S,_,_),0.8,per:S).
matching(Y^pred(Y,neuter,a,_),Z^pred(Z,neuter,a,_),0.99,a:neuter).
matching(Y^pred(Y,neuter,a,_),Z^pred(Z,S,n,_),0.5,n:S).

% they, them, theirs, this, that, those, these
matching(Y^pred(Y,thing,n,12),Z^pred(Z,S,n,_),0.5,n:S):-    \+ option('--semantics',amr).
matching(Y^pred(Y,thing,n,12),Z^named(Z,S,_,_),0.1,per:S):- \+ option('--semantics',amr).

% I, me, mine, you, yours, we, us, ours, myself, yourself, ourselves
matching(Y^pred(Y,person,n,1),Z^pred(Z,S,n,_),0.1,n:S):-      \+ option('--semantics',amr).
matching(Y^pred(Y,person,n,1),Z^named(Z,S,per,_),0.8,per:S):- \+ option('--semantics',amr).
matching(Y^pred(Y,person,n,1),Z^named(Z,S,_,_),0.5,per:S):-   \+ option('--semantics',amr).

% the
matching(Y^pred(Y,S,n,_),Z^pred(Z,S,n,_),0.9,n:S):- \+ option('--semantics',amr).

% names
matching(Y^named(Y,S,T,_),Z^named(Z,S,T,_),0.9,per:S):- \+ option('--semantics',amr).
matching(Y^named(Y,S,_,_),Z^named(Z,S,_,_),0.7,per:S):- \+ option('--semantics',amr).

% timex
matching(Y^timex(Y,date(_:D1,_:D2,_:D3,_:D4)),Z^timex(Z,date(_:D1,_:D2,_:D3,_:D4)),0.9,timex):- \+ option('--semantics',amr).
