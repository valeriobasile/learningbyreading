
/* -------------------------------------------------------------------------
   Expletive 'there' and other "special" nouns
------------------------------------------------------------------------- */

semlex(n,many,Index,Att-[sem:'QUA'|Att],Sem):-
   att(Att,pos,'NN'), !,
   Sem = lam(X,B:drs([],[B:Index:pred(X,quantity,n,1)])).

semlex(n,much,Index,Att-[sem:'QUA'|Att],Sem):-
   att(Att,pos,'NN'), !,
   Sem = lam(X,B:drs([],[B:Index:pred(X,amount,n,3)])).

semlex(n,there,Index,Att-[sem:'UNK'|Att],Sem):-
   att(Att,pos,'EX'), !,
   Sem = lam(X,B:drs([B:[]:Y],[B:Index:pred(Y,location,n,1),
                               B:[]:rel(Y,X,rel,0)])).


/* -------------------------------------------------------------------------
   Nouns
------------------------------------------------------------------------- */

semlex(n,other,Index,Att-[sem:'ALT'|Att],Sem):-       % OTHERS
   \+ option('--semantics',drg), 
   att(Att,pos,'NNS'), !,
   Sem = lam(X,merge(B0:drs([],[B0:Index:pred(X,person,n,1)]),
                     alfa(def,
                          B1:drs([B1:[]:Y],[B1:[]:pred(Y,person,n,1)]),
                          B2:drs([],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))])))).

semlex(n,Sym,Index,Att-[sem:'DOM'|Att],Sem):-         % DAY OF MONTH
   att(Att,pos,Pos),
   member(Pos,['CD','NNP','NN']),
   att(Att,namex,NE), neClass(NE,tim),
   dofm(Sym,DID), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',[]:'XX',Index:DID))])).

semlex(n,Sym,Index,Att-[sem:'YOC'|Att],Sem):-         % YEAR
   att(Att,pos,'CD'),
   att(Att,namex,NE), neClass(NE,tim),
   year(Sym,Year), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',Index:Year,[]:'XX',[]:'XX'))])).

semlex(n,Sym,Index,Att-[sem:'MOY'|Att],Sem):-         % MONTH
   att(Att,pos,'CD'),
   att(Att,namex,NE), neClass(NE,tim),
   month(Sym,MID), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',Index:MID,[]:'XX'))])).

semlex(n,Sym,Index,Att-[sem:'MOY'|Att],Sem):-         % MONTH
   att(Att,pos,'NNP'),
   att(Att,namex,NE), neClass(NE,tim),
   month(Sym,MID), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',Index:MID,[]:'XX'))])).

semlex(n,Sym,Index,Att1-[sem:'GPE'|Att2],Sem):-       % GPE-NOUNS
   att(Att1,namex,'gpe-nam'), 
   ( att(Att1,pos,'NNS');  att(Att1,pos,'NN') ), !,
   rel(from,Att1-Att2,Relation),
   Sem = lam(X,B:drs([B:[]:Y],[B:Index:pred(X,person,n,1),
                               B:[]:named(Y,Sym,gpe,nam),
                               B:[]:rel(X,Y,Relation,1)])).

semlex(n,Sym,Index,Att-[sem:'DEC'|Att],Sem):-         % DECADE
   att(Att,pos,'NNS'),
   decade(Sym,DID), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',Index:DID,[]:'XX',[]:'XX'))])).

semlex(n,Sym,Index,Att-[sem:'SCO'|Att],Sem):-         % SCORE
   att(Att,pos,'CD'),
   string2score(Sym,Score), !,
   Sem = lam(X,B:drs([],[B:Index:named(X,Score,sco,num)])).  % preliminary representation

semlex(n,Sym,Index,Att-[sem:'QUA'|Att],Sem):-         % QUANTITY
   att(Att,pos,'CD'),
   string2digit(Sym,Digit), !,
   Sem = lam(X,B:drs([],[B:Index:card(X,Digit,eq),B:[]:pred(X,thing,n,12)])).

semlex(n,Sym,Index,Att-[sem:Tag|Att],Sem):-         % NAME (SG)
   att(Att,pos,'NNP'), !,
   att(Att,namex,NE), neClassType(NE,Class,Type,Tag),
   Sem = lam(X,B:drs([],[B:Index:named(X,Sym,Class,Type)])).

semlex(n,Sym,Index,Att-[sem:Tag|Att],Sem):-         % NAME (PL)
   att(Att,pos,'NNPS'), !,
   att(Att,namex,NE), neClassType(NE,Class,Type,Tag),
   Sem = lam(X,B:drs([],[B:Index:named(X,Sym,Class,Type)])).

semlex(n,most,Index,Att-[sem:'TOP'|Att],Sem):- 
   att(Att,pos,'JJS'), !,
   att(Att,sense,Sense),
   Sem = lam(X,B:drs([],[B:Index:pred(X,most,n,Sense)])).

semlex(n,Sym,Index,Att-[sem:'TOP'|Att],Sem):-
   att(Att,pos,'JJS'),
%   option('--semantics',amr),
   !,
   att(Att,sense,Sense),
   Sem = lam(X,B:drs([],[B:Index:pred(X,Sym,n,Sense),B:[]:pred(X,most,r,1)])).

semlex(n,Sym,Index,Att-[sem:'TOP'|Att],Sem):- 
   \+ option('--semantics',drg),
   \+ option('--semantics',amr),
   att(Att,pos,'JJS'), !,
   Sem = lam(X,B1:drs([],[B1:[]:imp(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(Y,X)]))]),B4:drs([],[B4:Index:rel(X,Y,Sym,0)]))])).

%semlex(n,Sym,_,Index,Att-[sem:'UNK'|Att],Sem):- 
%   option('--x',true),
%   negprefix(_, Sym, Prefix, Core), !,
%   Sem = lam(X,B1:drs([],[B1:Index:not(B2:drs([],[B2:Index:pred(X,Prefix,n,71),B2:Index:pred(X,Core,n,1)]))])).

%semlex(n,Sym,_,Index,Att-[sem:'UNK'|Att],Sem):- 
%   option('--x',true),
%   negsuffix(_, Sym, Suffix, Core), !,
%   Sem = lam(X,B1:drs([],[B1:Index:not(B2:drs([],[B2:Index:pred(X,Suffix,n,72),B2:Index:pred(X,Core,n,1)]))])).

semlex(n,Sym,Index,Att-[sem:'CON'|Att],Sem):-
   option('--plural',true),
   att(Att,pos,'NNS'), !,
   att(Att,sense,Sense),
   Sem = lam(X,B:drs([],[B:Index:pred(X,Sym,n,Sense),B:[]:card(X,2,ge)])).

semlex(n,Sym,Index,Att-[sem:'CON'|Att],Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(X,B:drs([],[B:Index:pred(X,Sym,n,Sense)])).


/* -------------------------------------------------------------------------
   Relational nouns
------------------------------------------------------------------------- */

semlex(n/pp,Sym,Index,Att-[sem:'ROL'|Att],Sem):-
   att(Att,pos,Pos), 
   member(Pos,['NNP','NNPS']), !,
   att(Att,namex,NE), neClassType(NE,Class,Type),
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                           app(P,X)))).


semlex(n/pp,Sym,Index,Att-[sem:'ROL'|Att],Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                           app(P,X)))).

semlex(n/pp,Sym,Index,Att1-[sem:'ROL'|Att2],Sem):- 
   att(Att1,sense,Sense),
   roles(play,(s:dcl\np)/np,[_,Role],Att1-Att2), !,
   Sem = lam(P,lam(X,merge(B:drs([[]:Y],
                               [B:Index:pred(Y,Sym,n,Sense),
                                B:[]:role(X,Y,Role,1),
                                B:[]:pred(X,person,n,1)]),
                           app(P,Y)))).

semlex((n/pp)/pp,Sym,Index,Att-[sem:'ROL'|Att],Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P1,lam(P2,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                   merge(app(P1,X),app(P2,X)))))).

semlex(((n/pp)/pp)/pp,Sym,Index,Att-[sem:'ROL'|Att],Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P1,lam(P2,lam(P3,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                          merge(app(P1,X),merge(app(P2,X),app(P3,X)))))))).

semlex(n/(s:_\np),Sym,Index,Att-[sem:'CON'|Att],Sem):- !,
   att(Att,sense,Sense),
   closing(CC),
   Sem = lam(VP,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                            app(app(VP,lam(P,app(P,X))),CC)))).

semlex((n/(s:_\np))/pp,Sym,Index,Att-[sem:'ROL'|Att],Sem):- !,
   att(Att,sense,Sense),
   closing(CC),
   Sem = lam(P,lam(VP,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                  merge(app(app(VP,lam(P,app(P,X))),CC),
                                        app(P,X)))))).

semlex((n/pp)/(s:_\np),Sym,Index,Att-[sem:'ROL'|Att],Sem):- !,
   att(Att,sense,Sense),
   closing(CC),
   Sem = lam(VP,lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                  merge(app(app(VP,lam(P,app(P,X))),CC),
                                        app(P,X)))))).


/* =========================================================================
   Adjectives
========================================================================= */

/* -------------------------------------------------------------------------
   Wrongly Classified Adjectives + "own"
------------------------------------------------------------------------- */

semlex(n/n,Sym,Index,Att-[sem:'NIL'|Att],Sem):-
   att(Att,pos,'DT'), !,
   warning('wrongly classified determiner ~p at ~p',[Sym,Index]),
   Sem = lam(P,lam(X,app(P,X))).

semlex(Cat,many,Index,Att-[sem:'QUA'|Att],Sem):-
   cat(Cat,n|n), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,quantity,n,1)]),app(P,X)))).

semlex(Cat,much,Index,Att-[sem:'QUA'|Att],Sem):-
   cat(Cat,n|n), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,amount,n,3)]),app(P,X)))).

semlex(Cat,only,Index,Att-[sem:'EXC'|Att],Sem):- 
   cat(Cat,n|n), !,
   \+ option('--semantics',amr), !,
   Sem = lam(P,lam(X,merge(app(P,X),B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:Y],[]),app(P,Y)),B3:drs([],[B3:[]:eq(X,Y)]))])))).


/* -------------------------------------------------------------------------
   Negation Adjectives

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   cat(Cat,n|n), 
   option('--semantics',amr),
   negprefix(_, Sym, _Prefix, Core), !,
   Sem = lam(P,lam(X,merge(B1:drs([],[B1:Index:not(B2:drs([B2:[]:E],[B2:[]:rel(E,X,invmod,1),
                                                                     B2:Index:pred(E,Core,a,1)]))]),app(P,X)))).

semlex(Cat,Sym,Index,Att-[sem:'NOT'|Att],Sem):-
   cat(Cat,n|n), 
   option('--semantics',amr),
   negsuffix(_, Sym, _Suffix, Core), !,
   Sem = lam(P,lam(X,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:E],[B2:[]:rel(X,E,mod,1),
                                                                     B2:Index:pred(E,Core,a,1)]),app(P,X)))]))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Presuppositional Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'ALT'|Att],Sem):-
   \+ option('--semantics',drg), 
   \+ option('--semantics',amr), 
   member(Sym,[other,previous,different]),
   cat(Cat,n|n), !,
   Sem = lam(P,lam(X,merge(app(P,X),
                           alfa(def,
                                merge(B1:drs([B1:[]:Y],[]),app(P,Y)),
                                      B2:drs([],[B2:[]:not(B3:drs([],[B3:Index:eq(X,Y)]))]))))).


/* -------------------------------------------------------------------------
   Present participles, Gerunds
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'PRE'|Att2],Sem):-
   cat(Cat,n|n), 
   att(Att1,pos,'VBG'), 
   roles(Sym,s:dcl\np,[Role],Att1-Att2), !,
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],
                                 [B:Index:pred(E,Sym,v,0),
                                  B:[]:role(X,E,Role,-1)]), 
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Past Participles
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'PAS'|Att2],Sem):-
   cat(Cat,n|n), 
   att(Att1,pos,'VBN'), 
   roles(Sym,s:pss\np,[Role],Att1-Att2), !,
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],
                                 [B:Index:pred(E,Sym,v,0),
                                  B:[]:role(X,E,Role,-1)]), 
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Noun Noun Compounds
------------------------------------------------------------------------- */

semlex(Cat,'%',Index,Att1-['UOM'|Att2],Sem):-
   att(Att1,pos,'NN'),
   cat(Cat,n|n), !,
   semlex(Cat,percent,Index,Att1-Att2,Sem).

semlex(Cat,Sym,Index,Att-[sem:'UOM'|Att],Sem):- 
   cat(Cat,n|n),
   att(Att,pos,'$'), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,1)]),app(P,X)))).

semlex(Cat,'&',Index,Att-[sem:'ORG'|Att],Sem):- 
   cat(Cat,n|n),
   att(Att,namex,Ne), neClassType(Ne,org,Type), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:named(X,'&',org,Type)]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att-[sem:'SCO'|Att],Sem):- 
   att(Att,pos,'CD'),
   cat(Cat,n|n),
   string2score(Sym,Score), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:named(X,Score,sco,num)]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att-[sem:'DOM'|Att],Sem):- 
   att(Att,pos,'CD'),
   att(Att,namex,NE), neClass(NE,tim),
   cat(Cat,n|n),
   dofm(Sym,DID), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',[]:'XX',Index:DID))]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att-[sem:'MOY'|Att],Sem):- 
   att(Att,pos,'NNP'),
   att(Att,namex,NE), neClass(NE,tim),
   cat(Cat,n|n),
   month(Sym,MID), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',Index:MID,[]:'XX'))]),
                           app(P,X)))).

semlex(Cat,YID,Index,Att-[sem:'YOC'|Att],Sem):- 
   att(Att,pos,'CD'),
   att(Att,namex,NE),
   neClass(NE,tim),
   cat(Cat,n|n),
   year(YID,Year), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:timex(X,date([]:'+',Index:Year,[]:'XX',[]:'XX'))]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att1-[sem:'REL'|Att2],Sem):-    %%% NOUN-{NAME|NOUN} COMPOUNDS
   att(Att1,pos,Pos),
   member(Pos,['NN','NNS']),
   cat(Cat,n|n), !,
   ( att(Att1,relation,Rel), \+ Rel=unknown, !, Att2=Att1; Rel = of, Att2=[relation:of|Att1] ),
   ( Rel = '=', !, RelCond = eq(X,Y); RelCond = rel(X,Y,Rel,0) ),
   att(Att1,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:Index:pred(Y,Sym,n,Sense),
                                  B:[]:RelCond]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att1-[sem:'GPE'|Att2],Sem):-   %%% GPE-ADJECTIVES (e.g. French, German, etc.)
   cat(Cat,n|n),
   att(Att1,pos,'JJ'),
   att(Att1,namex,'gpe-nam'), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:Index:named(Y,Sym,gpe,nam),
                                  B:[]:rel(X,Y,Relation,1)]),
                           app(P,X)))).

/* NEEDS TO BE CHECKED; THERE ARE MANY JJ,Location tagged words

semlex(n/n,Sym,Index,Att1-[sem:'UNK'|Att2],Sem):-   %%% LOC-ADJECTIVES (e.g. Antarctic, etc.)
   att(Att1,pos,'JJ'),
   att(Att1,namex,Ne), neClassType(Ne,loc,Type), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:Index:named(Y,Sym,loc,Type),
                                  B:[]:rel(X,Y,Relation,1)]),
                           app(P,X)))).
*/

semlex(Cat,Sym,Index,Att1-[sem:Tag|Att2],Sem):-    %%% NAME-{NOUN|NAME} COMPOUNDS
   cat(Cat,n|n),
   att(Att1,pos,Pos),
   member(Pos,['NNP','NNPS']), !,
   att(Att1,namex,Ner), neClassType(Ner,Class,Type,Tag), 
   ( att(Att1,relation,Rel), \+ Rel=unknown, !, Att2=Att1; Rel = '=', Att2=[relation:Rel|Att1] ),
   ( Rel = '=', !, RelCond = eq(X,Y); RelCond = rel(X,Y,Rel,0) ),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:Index:named(Y,Sym,Class,Type),
                                  B:[]:RelCond]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att-[sem:Tag|Att],Sem):-
   att(Att,pos,Pos),
   member(Pos,['NNP','NNPS']),
   cat(Cat,n|n), !,
   att(Att,namex,Ner), neClassType(Ner,Class,Type,Tag), 
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Cardinal Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'QUA'|Att],Sem):-
   cat(Cat,n|n),
   string2digit(Sym,Digit), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:card(X,Digit,eq)]),app(P,X)))).

semlex(Cat,Sym,Index,Att-[sem:'QUA'|Att],Sem):-
   cat(Cat,n|n),
   string2digit(Sym,Digit), !,
   Sem = lam(P,lam(X,merge(app(P,X),B:drs([],[B:Index:card(X,Digit,eq)])))).


/* -------------------------------------------------------------------------
   Composite Adjectives:  10-hour story

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   cat(Cat,n|n),
   atomic_list_concat([Prefix,Suffix],'-',Sym),
   member(Suffix,[acre,year,yard,foot,pound,day,minute,page,point,man,inch,
                  degree,week,member,mile,week,km,dollar,kilometer,
                  'square-foot',seat,meter,story,hour,time,ton,month]),
   string2digit(Prefix,Number), !, 
   att(Att1,sense,Sense),
   rel(Suffix,Att1-Att2,Relation),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:[]:card(Y,Number,eq),
                                  B:Index:pred(Y,Suffix,n,Sense),
                                  B:[]:rel(X,Y,Relation,0)]),app(P,X)))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Comparative and Superlative Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'MOR'|Att],Sem):-
   att(Att,pos,'JJR'), \+ Sym=more,
   option('--semantics',amr),
   cat(Cat,n|n), !,
   att(Att,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:rel(X,E,mod,1),B:Index:pred(E,Sym,a,Sense),B:[]:pred(E,more,r,1)]),app(P,X)))).

semlex(Cat,Sym,Index,Att1-[sem:'MOR'|Att2],Sem):-
   att(Att1,pos,'JJR'), \+ Sym=more,
   cat(Cat,n|n), !,
   att(Att1,sense,Sense),
   roles(Sym,s:adj\np,[Role],Att1-Att2), 
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:role(X,E,Role,-1),B:Index:pred(E,Sym,a,Sense),B:[]:pred(E,more,r,1)]),app(P,X)))).

semlex(Cat,Sym,Index,Att-[sem:'TOP'|Att],Sem):-
   att(Att,pos,'JJS'), \+ Sym=most,
   option('--semantics',amr),
   cat(Cat,n|n), !,
   att(Att,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:rel(X,E,mod,1),B:Index:pred(E,Sym,a,Sense),B:[]:pred(E,most,r,1)]),app(P,X)))).

semlex(Cat,Sym,Index,Att1-[sem:'TOP'|Att2],Sem):-
   att(Att1,pos,'JJS'), \+ Sym=most,
   cat(Cat,n|n), !,
   att(Att1,sense,Sense),
   roles(Sym,s:adj\np,[Role],Att1-Att2), 
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:role(X,E,Role,-1),B:Index:pred(E,Sym,a,Sense),B:[]:pred(E,most,r,1)]),app(P,X)))).

%semlex(Cat,Sym,Index,Att-[sem:'UNK'|Att],Sem):-
%   \+ option('--semantics',drg),
%   \+ option('--semantics',amr),
%   att(Att,pos,'JJS'),
%   cat(Cat,n|n), !,
%   Sem = lam(P,lam(X,merge(app(P,X),
%                           B1:drs([],[B1:[]:imp(merge(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),
%                                                      app(P,Y)),
%                                                B4:drs([],[B4:Index:rel(X,Y,Sym,0)]))])))).


/* -------------------------------------------------------------------------
   Singular Intersective Adjectives: AMR
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'IST'|Att],Sem):-
   cat(Cat,n|n), 
   option('--semantics',amr), !,
   att(Att,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:rel(X,E,mod,1),B:Index:pred(E,Sym,a,Sense)]),app(P,X)))).


/* -------------------------------------------------------------------------
   Singular Intersective Adjectives: Thematic Role Analysis
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'IST'|Att2],Sem):-
   cat(Cat,n|n), !,
   att(Att1,sense,Sense),
   roles(Sym,s:adj\np,[Role],Att1-Att2), !,
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:role(X,E,Role,-1),B:Index:pred(E,Sym,a,Sense)]),app(P,X)))).

semlex(Cat,Sym,Index,Att1-[sem:'IST'|Att2],Sem):-
   cat(Cat,(n|pp)|(n|pp)), !,
   att(Att1,sense,Sense),
   roles(Sym,s:adj\np,[Role],Att1-Att2), !,
   Sem = lam(PP,lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:role(X,E,Role,-1),B:Index:pred(E,Sym,a,Sense)]),app(app(PP,P),X))))).


/* -------------------------------------------------------------------------
   Singular Intersective Adjectives: Classic Analysis
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'IST'|Att],Sem):-
   cat(Cat,n|n), !,
   att(Att,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),app(P,X)))).

semlex(Cat,Sym,Index,Att-[sem:'IST'|Att],Sem):-
   cat(Cat,(n|pp)|(n|pp)), !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),app(app(PP,P),X))))).


/* -------------------------------------------------------------------------
   Comparatives
------------------------------------------------------------------------- */

% more than 10 dogs
%
semlex(Cat,Sym,Index,Att-[sem:'MOR'|Att],Sem):-  
   Cat = ((n/n)/(n/n))\(s:adj\np), !,
   att(Att,sense,Sense),
   Sem = lam(M,lam(C,lam(P,lam(X,merge(app(P,X),
             app(app(M,lam(Q,app(Q,X))),
                   lam(E,merge(B1:drs([B1:[]:Z],[]),
                         app(app(C,lam(Y,B2:drs([],[B2:Index:rel(E,Y,Sym,Sense)]))),Z))))))))).

% more than $ 600 million
%
semlex(Cat,Sym,Index,Att-[sem:'MOR'|Att],Sem):- 
   Cat = (n/n)\(s:adj\np), !,
   att(Att,sense,Sense),
   Sem = lam(M,lam(P,lam(X,merge(B1:drs([B1:[]:Y],[]),
                                 merge(app(P,Y),
                                       app(app(M,lam(Q,app(Q,X))),lam(E,B:drs([],[B:Index:rel(E,Y,Sym,Sense)])))))))).
%   closing(CC),
%   Sem = lam(VP,lam(N,lam(X,app(app(VP,lam(P,merge(app(P,X),app(N,X)))),CC)))).

% this category is sometimes assigned to opening brackets
%
semlex(Cat,_Sym,_Index,Att-[sem:'MOR'|Att],Sem):- 
   Cat = (n/n)/(s:adj\np), !,
   closing(CC),
   Sem = lam(VP,lam(N,lam(X,app(app(VP,lam(P,merge(app(P,X),app(N,X)))),CC)))).

/* -------------------------------------------------------------------------
   Wrongly classified determiners
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_Index,Att-[sem:'NIL'|Att],Sem):-  
   att(Att,pos,'DT'),
   member(Cat,[(n/n)/(n/n),(n\n)/(n\n),(n/n)\(n/n)]), !,
   Sem = lam(Z,lam(P,lam(X,app(app(Z,P),X)))).


/* -------------------------------------------------------------------------
   Compound numerals
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'QUA'|Att],Sem):-  
   member(Cat,[(n/n)/(n/n),
               (n\n)/(n\n),
               (n/n)\(n/n)]), 
   string2digit(Sym,Digit), !,
   Sem = lam(Z,lam(P,lam(X,merge(B:drs([],[B:Index:card(X,Digit,eq)]),
                                 app(app(Z,P),X))))).


/* -------------------------------------------------------------------------
   Compound superlative adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'TOP'|Att2],Sem):-        
   att(Att1,pos,'JJS'),
   member(Cat,[(n/n)/(n/n),        %%%% Example: ... fastest growing segment
               (n/n)\(n/n)]), !,   %%%% Example: ... third largest bank 
   att(Att1,sense,Sense),
   roles(Sym,s:adj\np,[Role],Att1-Att2), !,
   Sem = lam(Z,lam(P,lam(X,merge(app(app(Z,P),X),
                                 B:drs([B:[]:E],[B:[]:role(X,E,Role,-1),
                                                 B:Index:pred(E,Sym,a,Sense),
                                                 B:[]:pred(E,most,r,1)]))))).

semlex(Cat,Sym,Index,Att-[sem:'TOP'|Att],Sem):-        
   \+ option('--semantics',drg),
   att(Att,pos,'JJS'),
   member(Cat,[(n/n)/(n/n),        %%%% Example: ... fastest growing segment
               (n/n)\(n/n)]), !,   %%%% Example: ... third largest bank (incorrect semantics!)
   Sem = lam(Z,lam(P,lam(X,merge(app(app(Z,P),X),
                                 B1:drs([],[B1:[]:imp(merge(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),
                                                              app(app(Z,P),Y)),
                                                        B4:drs([],[B4:Index:rel(X,Y,Sym,0)]))]))))).

/* -------------------------------------------------------------------------
   Comparatives (e.g. "more important details")
------------------------------------------------------------------------- */

semlex(Cat,more,Index,Att-[sem:'MOR'|Att],Sem):-        
   member(Cat,[(n/n)/(n/n),(n/n)\(n/n)]), !, 
   Sem = lam(Z,lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,more,r,1)]),
                                 app(app(Z,P),X))))).


/* -------------------------------------------------------------------------
   Intensifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'INT'|Att],Sem):-  
   att(Att,pos,Pos),
   member(Pos,['NNP','NNPS']),
   member(Cat,[(n/n)/(n/n),
               (n\n)/(n\n),
               (n/n)\(n/n)]), !,
   att(Att,namex,Ne), neClassType(Ne,Class,Type),
   Sem = lam(Z,lam(P,lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                                 app(app(Z,P),X))))).

semlex(Cat,Sym,Index,Att-[sem:'INT'|Att],Sem):-  
   member(Cat,[(n/n)/(n/n),
               (n\n)/(n\n),
               (n/n)\(n/n)]), !,
   att(Att,sense,Sense),
   Sem = lam(Z,lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,r,Sense)]),
                                 app(app(Z,P),X))))).

semlex(Cat,Sym,Index,Att-[sem:'INT'|Att],Sem):-  
   Cat = ((n/n)/(n/n))/((n/n)/(n/n)), !,
   att(Att,sense,Sense),
   Sem = lam(M,lam(A,lam(P,lam(X,app(app(app(M,A),lam(Y,merge(B:drs([],[B:Index:pred(Y,Sym,r,Sense)]),app(P,Y)))),X))))).

/* -------------------------------------------------------------------------
   Definite Prepositions
------------------------------------------------------------------------- */

% except
%
semlex(Cat,except,Index,Att-[sem:'NOT'|Att],Sem):- 
   Cat = (n\n)/pp, !,
   Sem = lam(PP,lam(P,lam(X,merge(app(P,X),B:drs([],[B:Index:not(app(PP,X))]))))).

semlex(Cat,Sym,Index,Att-[sem:'IST'|Att],Sem):- 
   Cat = (n\n)/pp, !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(P,lam(X,merge(app(P,X),merge(B:drs([],[B:Index:pred(X,Sym,r,Sense)]),app(PP,X)))))).

% Range constructions (e.g., "10 to 20")
%
semlex(Cat,to,Index,Att-[sem:'DIS'|Att],Sem):- 
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,B:drs([],[B:Index:or(app(P,X),app(N,X))])))).

% seven cents a share
%
semlex(Cat,Sym,Index,Att1-[sem:'AND'|Att2],Sem):- 
   member(Sym,[a,an]),
   Cat = (n\n)/n, !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(N,lam(P,lam(X,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:[]:Y],[]),app(N,Y)),
                                                merge(B3:drs([],[B3:Index:rel(X,Y,Relation,0)]),app(P,X)))])))).

semlex(Cat,Sym,Index,Att-[sem:'PRX'|Att],Sem):- 
   option('--semantics',amr),
   member(Sym,[this,these]),
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,alfa(def,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,this,r,2)]),app(N,Y)),
                                    merge(B2:drs([],[B2:[]:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,Index,Att-[sem:'DST'|Att],Sem):- 
   option('--semantics',amr),
   member(Sym,[that,those]),
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,alfa(def,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,that,r,2)]),app(N,Y)),
                                    merge(B2:drs([],[B2:[]:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,Index,Att-[sem:'PRX'|Att],Sem):- 
   member(Sym,[the,this,these]),
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,alfa(def,merge(B1:drs([B1:[]:Y],[]),app(N,Y)),
                                    merge(B2:drs([],[B2:Index:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,Index,Att-[sem:'DST'|Att],Sem):- 
   member(Sym,[that,those]),
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,alfa(def,merge(B1:drs([B1:[]:Y],[]),app(N,Y)),
                                    merge(B2:drs([],[B2:Index:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,Index,Att-[sem:'INT'|Att],Sem):- 
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,merge(merge(B1:drs([B1:[]:Y],[]),app(N,Y)),
                                 merge(B2:drs([],[B2:Index:rel(X,Y,Sym,0)]),app(P,X)))))).

semlex((n/n)/n,Sym,Index,Att-[sem:'UOM'|Att],Sem):- 
   att(Att,pos,'$'), !,
   Sem = lam(N,lam(P,lam(X,merge(merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,Sym,n,1)]),app(N,Y)),
                                 merge(B2:drs([],[B2:[]:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,Index,Att-[sem:'INT'|Att],Sem):- 
   Cat = (n/n)/n, !,
   att(Att,sense,Sense),
   Sem = lam(N,lam(P,lam(X,merge(merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,Sym,n,Sense)]),app(N,Y)),
                                 merge(B2:drs([],[B2:[]:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(((n/n)\(n/n))/(n/n),_,Index,Att-[sem:'DIS'|Att],Sem):- !,
   Sem = lam(M2,lam(M1,lam(P,lam(X,B:drs([],[B:Index:or(app(app(M1,P),X),
                                                        app(app(M2,P),X))]))))).

semlex(((n/n)\(n/n))/n,_,Index,Att-[sem:'DIS'|Att],Sem):- !,
   Sem = lam(N,lam(M,lam(P,lam(X,B:drs([],[B:Index:or(app(app(M,P),X),
                                                      merge(app(N,X),app(P,X)))]))))).

semlex(((n/pp)\(n/pp))/n,_,Index,Att-[sem:'INT'|Att],Sem):- !,
   Sem = lam(N,lam(RN,lam(PP,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,rel,0)]),
                                         merge(app(N,Y),app(app(RN,PP),X))))))).



/* -------------------------------------------------------------------------
   Prepositions
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(n\n)/np,
               (n/n)/np,
               (n/n)\np,
               (n\n)\np]), !,
   Sem = lam(Q,lam(P,lam(X,merge(app(P,X),app(Q,lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)]))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   Cat = ((n/(s:to\np))\(n/(s:to\np)))/np, !,
   Sem = lam(Q,lam(N,lam(VP,lam(X,merge(app(app(N,VP),X),app(Q,lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)])))))))).

semlex(Cat,Sym,Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[((n/n)\(n/n))/np,
               ((n/pp)\(n/pp))/np]), !,
   Sem = lam(Q,lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(app(Q,lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)]))),
                                                       app(P,X)))),Y))))).  


/* -------------------------------------------------------------------------
   Noun subcategorising for sentence
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-[sem:'SUB'|Att2],Sem):-
   member(Cat,[n/s:em,n/s:qem,n/s:bem]), !,
   att(Att1,sense,Sense),
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2), 
   Sem = lam(S,lam(X,merge(B1:drs([],
                                  [B1:Index:pred(X,Sym,n,Sense)]),
                           app(S,lam(E,B2:drs([],[B2:[]:role(X,E,Role,1)])))))).

semlex(Cat,Sym,Index,Att1-[sem:'SUB'|Att2],Sem):-
   Cat = n/s:_, !,
   att(Att1,sense,Sense),
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2),
   plosing(CC),
   Sem = lam(S,lam(X,B:drs([B:[]:K],
                           [B:Index:pred(X,Sym,n,Sense),
                            B:[]:role(X,K,Role,1),
                            B:[]:prop(K,app(S,CC))]))).

semlex(Cat,Sym,Index,Att1-[sem:'SUB'|Att2],Sem):-
   Cat = (n/pp)/s:_, !, 
   att(Att1,sense,Sense),
   plosing(CC),
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2),
   Sem = lam(S,lam(P,lam(X,merge(B:drs([B:[]:K],
                                       [B:Index:pred(X,Sym,n,Sense),
                                        B:[]:rel(X,K,Role,0),
                                        B:[]:prop(K,app(S,CC))]),app(P,X))))).


/* -------------------------------------------------------------------------
   NP modifying noun
------------------------------------------------------------------------- */

semlex(n/np,Sym,Index,Att-[sem:'AND'|Att],Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(NP,lam(X,merge(B1:drs([],[B1:Index:pred(X,Sym,n,Sense)]),
                            app(NP,lam(Y,B2:drs([],[B2:[]:rel(X,Y,rel,0)])))))).


/* -------------------------------------------------------------------------
   Restrictive Relative Pronous
------------------------------------------------------------------------- */

semlex(Cat,Sym,_Index,Att1-[sem:'AND'|Att2],Sem):-
   member(Cat,[(n\n)/(s:to\np), (n\n)/(s:to/np)]), !,
   roles(Sym,(s:dcl\np)/s:_,[Role,_],Att1-Att2), 
   Sem = lam(VP,lam(N,lam(X,merge(app(N,X),
                                  app(app(VP,
                                          lam(P,app(P,X))),
                                      lam(Y,B:drs([],[B:[]:role(X,Y,Role,1)]))))))).

semlex(Cat,_Sym,_Index,Att-[sem:'AND'|Att],Sem):-
   member(Cat,[(n\n)/(s:_\np), (n\n)/(s:_/np)]), !,
   closing(CC),
   Sem = lam(VP,lam(N,lam(X,merge(app(N,X),
                                  app(app(VP,
                                          lam(P,app(P,X))),
                                      CC))))).

semlex(((n\n)/(s:dcl\np))/n,_,Index,Att1-[sem:'AND'|Att2],Sem):- !,
   closing(CC),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(VP,lam(P,lam(X,merge(app(P,X),app(app(VP,lam(P,merge(B:drs([B:[]:Y],[B:Index:rel(Y,X,Relation,1)]),
                                                                        merge(app(N,Y),app(P,Y))))),CC)))))).

semlex((n\n)/s:_, Sym,Index,Att-[sem:'SUB'|Att],Sem):- !,
   plosing(CC),
   Sem = lam(S,lam(P,lam(X,merge(B:drs([B:[]:Z],[B:Index:rel(X,Z,Sym,0),
                                               B:[]:prop(Z,app(S,CC))]),
                                 app(P,X))))).


