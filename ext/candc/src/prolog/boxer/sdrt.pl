
:- module(sdrt,[mergeSDRS/2]).

:- use_module(library(lists),[member/2,append/3]).
 

/* =========================================================================
   Subordinate SDRT relations
========================================================================= */

subordinate(elaboration).
subordinate(instance).
subordinate(topic).
subordinate(explanation).
subordinate(precondition).
subordinate(commentary).
subordinate(correction).


/* =========================================================================
   Coordinate SDRT relations
========================================================================= */

coordinate(continuation).
coordinate(narration).
coordinate(background).
coordinate(result).


/* =========================================================================
   Insert DRS into SDRS
========================================================================= */

mergeSDRS(smerge(S1,S2,Rel,_Pops),S3):-
   subordinate(Rel), !,
   subDRS(S1,Rel,S2,S3).

mergeSDRS(smerge(S1,S2,Rel,_Pops),S3):-
   coordinate(Rel), !,
   cooDRS(S1,Rel,S2,S3).


/* =========================================================================
   Insert subordinate DRS into SDRS
========================================================================= */

% DRS + (S)DRS
%
subDRS(DRS,Rel,B2,SDRS):-
   member(DRS,[_:drs(_,_),alfa(_,_,_),merge(_,_)]), !,
   SDRS = sdrs([sub(lab(K1,DRS),lab(K2,B2))],[[]:rel(K1,K2,Rel)]).

% SUB + (S)DRS
%
subDRS(sdrs([sub(L1,lab(L2,S1))],R),Rel,B,sdrs([sub(L1,lab(L2,S2))],R)):- !,
   subDRS(S1,Rel,B,S2).

% Non-SUB + (S)DRS
%
subDRS(sdrs([lab(K1,B1)],R),Rel,B2,SDRS):- !,
   SDRS = sdrs([sub(lab(K1,B1),lab(K2,B2))],[[]:rel(K1,K2,Rel)|R]).

% Recurse down to last label of first argument  
%
subDRS(sdrs([X|L1],R1),Rel,New,sdrs([X|L2],R2)):-
   subDRS(sdrs(L1,R1),Rel,New,sdrs(L2,R2)).


/* =========================================================================
   Insert coordinate DRS into SDRS
========================================================================= */

% Make SDRSs of both DRSs (if needed)
%
cooDRS(B1,Rel,B2,S):- 
   member(B1,[_:_,merge(_,_),alfa(_,_,_)]), !, 
   cooDRS(sdrs([lab(_,B1)],[]),Rel,B2,S).

cooDRS(B1,Rel,B2,S):- 
   member(B2,[_:_,merge(_,_),alfa(_,_,_)]), !, 
   cooDRS(B1,Rel,sdrs([lab(_,B2)],[]),S).

% Non-SUB + Non-SUB
%
cooDRS(sdrs([lab(K1,B1)],R1),Rel,sdrs([lab(K2,B2)|L],R2),SDRS):- !,
   append(R1,[[]:rel(K1,K2,Rel)|R2],R3),
   SDRS = sdrs([lab(K1,B1),lab(K2,B2)|L],R3).

% SUB (no pop)
%
cooDRS(sdrs([sub(B1,lab(K3,B3))],R),Rel,B2,sdrs([sub(B1,lab(K3,SDRS))],R)):- !,
   cooDRS(B3,Rel,B2,SDRS).

% SUB (pop) + Non-SUB
%
cooDRS(sdrs([sub(lab(K1,B1),B3)],R1),Rel,sdrs([lab(K2,B2)|L],R2),SDRS):- !,
   append(R1,[[]:rel(K1,K2,Rel)|R2],R3),
   SDRS = sdrs([sub(lab(K1,B1),B3),lab(K2,B2)|L],R3).

% SUB (pop) + SUB
%
cooDRS(sdrs([sub(lab(K1,B1),B3)],R1),Rel,sdrs([sub(lab(K2,B2),B4)|L],R2),SDRS):- !,
   append(R1,[[]:rel(K1,K2,Rel)|R2],R3),
   SDRS = sdrs([sub(lab(K1,B1),B3),sub(lab(K2,B2),B4)|L],R3).

% Non-SUB + SUB
%
cooDRS(sdrs([lab(K1,B1)],R1),Rel,sdrs([sub(lab(K2,B2),B4)|L],R2),SDRS):- !,
   append(R1,[[]:rel(K1,K2,Rel)|R2],R3),
   SDRS = sdrs([lab(K1,B1),sub(lab(K2,B2),B4)|L],R3).

% Recurse down to last label of first argument  
%
cooDRS(sdrs([X|L1],R1),Rel,New,sdrs([X|L2],R2)):- !,
   cooDRS(sdrs(L1,R1),Rel,New,sdrs(L2,R2)).




