/* -------------------------------------------------------------------------
   PP|NP 

   ... switch *to* bass ...
   ... walked away *from* the project ...

------------------------------------------------------------------------- */

semlex(pp/np,Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   option('--roles',verbnet),
   roles(Sym,pp/np,[Role],Att1-Att2), !,
   Sem = lam(Q,lam(X,app(Q,lam(Y,B:drs([],[B:Index:role(X,Y,Role,1)]))))).

semlex(pp/np,Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(Q,lam(X,app(Q,lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)]))))).


/* -------------------------------------------------------------------------
   PP|N
------------------------------------------------------------------------- */

semlex(pp/n,Sym,Index,Att-[sem:'UOM'|Att],Sem):-
   att(Att,pos,'$'), !,
   Sem = lam(N,lam(X,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,n,1),B:[]:rel(X,Y,rel,0)]),
                           app(N,Y)))).

semlex(pp/n,Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(N,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,Sym,0)]),app(N,Y)))).


/* -------------------------------------------------------------------------
   PP/VP[ng]

   ... succeeded *in* freeing ...
   ... suspected *of* using ...
   ... refrained *from* opening ...
   ... limited *to* providing ...

------------------------------------------------------------------------- */

semlex(pp/(s:ng\np),Sym,Index,Att1-[sem:'SUB'|Att2],Sem):-
   roles(Sym,pp/(s:ng\np),[Role],Att1-Att2), !,
   plosing(CC),
   NP = lam(P,merge(B2:drs([B2:[]:Y],[]),app(P,Y))),
   Sem = lam(VP,lam(E,B1:drs([B1:[]:P],[B1:Index:role(E,P,Role,1),
                                        B1:[]:prop(P,app(app(VP,NP),CC))]))).


/* -------------------------------------------------------------------------
   PP/VP[adj]

------------------------------------------------------------------------- */

semlex(pp/(s:adj\np),Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   roles(Sym,(s:adj\np),[Role],Att1-Att2), !,
   Sem = lam(VP,lam(X,app(app(VP,lam(P,merge(B:drs([B:[]:Y],[]),app(P,Y)))),
                          lam(E,C:drs([],[C:Index:role(X,E,Role,1)]))))).

semlex(pp/(s:b\np),Sym,Index,Att1-[sem:'AND'|Att2],Sem):-
   roles(Sym,pp/(s:b\np),[Role],Att1-Att2), !,
   Sem = lam(VP,lam(X,app(app(VP,lam(P,merge(B:drs([B:[]:Y],[]),app(P,Y)))),
                          lam(E,C:drs([],[C:Index:role(X,E,Role,1)]))))).

semlex(pp/(s:_\np),Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(VP,lam(X,app(app(VP,lam(P,merge(B:drs([B:[]:Y],[]),app(P,Y)))),
                          lam(E,C:drs([],[C:Index:rel(X,E,Sym,0)]))))).



%   Prep + NP + VP (... results in shareholders receiving ...)
%
semlex((pp/(s:_\np))/np,Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(NP,lam(VP,lam(E,app(app(VP,NP),lam(F,B:drs([],[B:Index:rel(E,F,Sym,0)])))))).

semlex(pp/s:_,Sym,Index,Att1-[sem:'SUB'|Att2],Sem):-
   roles(Sym,pp/s:_,[Role],Att1-Att2), !,
   Sem = lam(S,lam(E,app(S,lam(X,B:drs([],[B:Index:role(E,X,Role,1)]))))).

semlex(pp/s,Sym,Index,Att1-[sem:'SUB'|Att2],Sem):-
   roles(Sym,pp/s,[Role],Att1-Att2), !,
   Sem = lam(S,lam(E,app(S,lam(X,B:drs([],[B:Index:role(E,X,Role,1)]))))).

semlex(pp/s:_,Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(S,lam(E,app(S,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))))).

semlex(pp/s,Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(S,lam(E,app(S,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))))).


/* -------------------------------------------------------------------------
   (pp|np)|np
------------------------------------------------------------------------- */

semlex((pp\np)/np,Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   Sem = lam(Q1,lam(Q2,lam(X,merge(app(Q2,lam(Z,B1:drs([],[B1:[]:rel(X,Z,rel,0)]))),
                                   app(Q1,lam(Y,B2:drs([],[B2:Index:rel(X,Y,Sym,0)]))))))).



/* -------------------------------------------------------------------------
   PP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):- 
   option('--semantics',drg),   
   notSymbol(Sym),
   member(Cat,[pp/pp,pp\pp]), !, 
   Sem = lam(P,lam(E,B:drs([],[B:Index:pred(E,Sym,s,1),B:[]:not(app(P,E))]))).

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   notSymbol(Sym),
   member(Cat,[pp/pp,pp\pp]), !, 
   Sem = lam(P,lam(E,B:drs([],[B:Index:not(app(P,E))]))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[pp/pp,pp\pp]), !, 
   att(Att,sense,Sense),
   Sem = lam(P,lam(E,merge(app(P,E),
                           B:drs([],[B:Index:pred(E,Sym,r,Sense)])))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(pp\pp)\np,
               (pp\pp)/np,
               (pp/pp)/np]), !, 
   Sem = lam(NP,lam(P,lam(E,merge(app(P,E),
                                  app(NP,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(pp\pp)\s:dcl,
               (pp\pp)/s:dcl,
               (pp/pp)/s:dcl]), !, 
   Sem = lam(S,lam(P,lam(E,merge(app(P,E),
                                 app(S,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))))))).

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-  
   notSymbol(Sym),
   member(Cat,[(pp\pp)/(pp\pp),
               (pp/pp)/(pp/pp)]), !,
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,B:drs([],[B:Index:not(app(P,X))]))),Y)))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-  
   member(Cat,[(pp\pp)/(pp\pp),
               (pp/pp)/(pp/pp)]), !,
   att(Att,sense,Sense),
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,r,Sense)]),
                                                 app(P,X)))),Y)))).


/* -------------------------------------------------------------------------
   Preposition: the (as in "the week before")
------------------------------------------------------------------------- */

semlex(Cat,the,Index,Att1-[sem:'DEF'|Att2],Sem):-
   member(Cat,[(pp\pp)/n]), !, 
   role(['Time'],Att1-Att2,[Role]),
   Sem = lam(N,lam(P,lam(E,merge(app(P,E),
                                 alfa(def,merge(B1:drs([B1:[]:X],[]),
                                                app(N,X)),
                                          B2:drs([],[B2:Index:role(E,X,Role,1)])))))).

semlex(Cat,Sym,Index,Att-[sem:'DEF'|Att],Sem):-
   member(Cat,[(pp\pp)/n]), !, 
   Sem = lam(N,lam(P,lam(E,merge(app(P,E),
                                 alfa(def,merge(B1:drs([B1:[]:X],[]),
                                                app(N,X)),
                                          B2:drs([],[B2:Index:rel(E,X,Sym,0)])))))).


/* -------------------------------------------------------------------------
   Compound adjectives (actually, the hyphen in compound adjectives)
------------------------------------------------------------------------- */

semlex(pp\n,'-',Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(N,lam(X,alfa(def,merge(B1:drs([B1:[]:Y],[]),app(N,Y)),
                                    B2:drs([],[B2:Index:rel(X,Y,loc_rel,0)])))).

semlex(pp\n,'-',Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(N,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,rel,0)]),
                           app(N,Y)))).

semlex(pp\n,'-',Index,Att-[sem:'UNK'|Att],Sem):- !,
   Sem = lam(N,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,attribute,0)]),
                           app(N,Y)))).


