
:- module(pdrs2drs,[pdrs2drs/2]).

%:- use_module(semlib(errors),[warning/2]).
%:- use_module(semlib(options),[option/2]).
:- use_module(library(lists),[select/3,append/3]).


/* ========================================================================
    Main Predicate
======================================================================== */

pdrs2drs(PDRS,DRS):-
   pdrs2drs(PDRS,[dom(g):[]-D,con(g):[]-C],Projected,B),
   G = drs(D,C),
   accommodate(Projected),
   ( G = drs([],[]), !, DRS = B; DRS = merge(G,B) ).


/* ========================================================================
   Accommodation
======================================================================== */

accommodate([]):- !.
accommodate([dom(_):X-X|L]):- !, accommodate(L).
accommodate([con(_):X-X|L]):- !, accommodate(L).


/* ========================================================================
   Translate PDRSs into DRSs
======================================================================== */

pdrs2drs(lab(X,B1),G1,G2,lab(X,B2)):- !,
   pdrs2drs(B1,G1,G2,B2).

pdrs2drs(sub(B1,B2),G1,G3,sub(B3,B4)):- !,
   pdrs2drs(B1,G1,G2,B3),
   pdrs2drs(B2,G2,G3,B4).

pdrs2drs(merge(B1,B2),G1,G3,merge(B3,B4)):- !,
   pdrs2drs(B1,G1,G2,B3),
   pdrs2drs(B2,G2,G3,B4).

pdrs2drs(alfa(T,B1,B2),G1,G3,alfa(T,B3,B4)):- !,
   pdrs2drs(B1,G1,G2,B3),
   pdrs2drs(B2,G2,G3,B4).

pdrs2drs(B:drs(D1,C1),G1,G3,drs(D2,C2)):- !,
   dom2drs(D1,[dom(B):[]-D2|G1],G2),
   cons2drs(C1,[con(B):[]-C2|G2],G3).

pdrs2drs(sdrs(S1,C),G1,G2,sdrs(S2,C)):- !,
   pdrs2drs(S1,G1,G2,S2).

pdrs2drs([B1|L1],G1,G3,[B2|L2]):- !,
   pdrs2drs(B1,G1,G2,B2),
   pdrs2drs(L1,G2,G3,L2).

pdrs2drs([],G,G,[]):- !.


/* ========================================================================
   Project Domains
======================================================================== */

dom2drs([],G,G).

dom2drs([B1:Ind1:Ref1|L],G1,G3):-                 
   select(dom(B1),G1,G2,dom(B2):D1-D2),
   select(Ind2:Ref2,D1,D3), Ref1 == Ref2, !,
   append(Ind1,Ind2,Ind),
   dom2drs(L,[dom(B2):[Ind:Ref1|D3]-D2|G2],G3).  

dom2drs([B1:Ind:Ref|L],G1,G3):-                 
   select(dom(B1),G1,G2,dom(B2):D1-D2),
   dom2drs(L,[dom(B2):[Ind:Ref|D1]-D2|G2],G3).  


/* ========================================================================
   Project Conditions
======================================================================== */

cons2drs([],G,G).

cons2drs([B1:Ind1:Con1|L],G1,G4):-        
   con2drs(Con1,G1,G2,Con2),
   select(con(B1),G2,G3,con(B2):C1-C2), 
   select(Ind2:Con3,C1,C3), Con2 == Con3, !,
   append(Ind1,Ind2,Ind),
   cons2drs(L,[con(B2):[Ind:Con2|C3]-C2|G3],G4).  

cons2drs([B1:Ind:Con1|L],G1,G4):-        
   con2drs(Con1,G1,G2,Con2),
   select(con(B1),G2,G3,con(B2):C1-C2), 
   cons2drs(L,[con(B2):[Ind:Con2|C1]-C2|G3],G4).  


/* ========================================================================
   Project Condition
======================================================================== */ 

con2drs(not(B1),G1,G2,not(B2)):- !,
   pdrs2drs(B1,G1,G2,B2).

con2drs(pos(B1),G1,G2,pos(B2)):- !,
   pdrs2drs(B1,G1,G2,B2).

con2drs(nec(B1),G1,G2,nec(B2)):- !,
   pdrs2drs(B1,G1,G2,B2).
 
con2drs(prop(X,B1),G1,G2,prop(X,B2)):- !,
   pdrs2drs(B1,G1,G2,B2).

con2drs(or(B1,B2),G1,G3,or(B3,B4)):- !,
   pdrs2drs(B1,G1,G2,B3),
   pdrs2drs(B2,G2,G3,B4).

con2drs(imp(B1,B2),G1,G3,imp(B3,B4)):- !,
   pdrs2drs(B1,G1,G2,B3),
   pdrs2drs(B2,G2,G3,B4).

con2drs(whq(B1,B2),G1,G3,whq(B3,B4)):- !,
   pdrs2drs(B1,G1,G2,B3),
   pdrs2drs(B2,G2,G3,B4).

con2drs(duplex(X,B1,Y,B2),G1,G3,duplex(X,B3,Y,B4)):- !,
   pdrs2drs(B1,G1,G2,B3),
   pdrs2drs(B2,G2,G3,B4).

con2drs(role(X,Y,Rel,1),G,G,rel(X,Y,Rel,0)):- !.

con2drs(role(X,Y,Rel,-1),G,G,rel(Y,X,Rel,0)):- !.

con2drs(C,G,G,C).


/* ========================================================================
   Select Domain
======================================================================== */

select(dom(B1),G1,G2,dom(B1):C1-C2):- 
   select(dom(B2):C1-C2,G1,G2), B1==B2, !. 

select(dom(_),G1,G2,dom(g):C1-C2):- 
   select(dom(B):C1-C2,G1,G2), B==g, !.


/* ========================================================================
   Select Conditions
======================================================================== */

select(con(B1),G1,G2,con(B1):C1-C2):- 
   select(con(B2):C1-C2,G1,G2), B1==B2, !. 

select(con(_),G1,G2,con(g):C1-C2):- 
   select(con(B):C1-C2,G1,G2), B==g, !.
