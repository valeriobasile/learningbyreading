
:- module(categories,
          [category/3,       %  +Type, +Cat, -Mood
           roles/4,
           sense/4,
           att/3,
           rel/3, 
           role/3
          ]).

:- use_module(boxer(slashes)).
:- use_module(semlib(options)).
:- use_module(semlib(errors),[warning/2]).
:- use_module(lex(verbnet),[verbnet/3]).


/* -------------------------------------------------------------------------
   Accessing Attributes
------------------------------------------------------------------------- */

att([],sense,0):- !.
att([],namex,'O'):- !.
att([],verbnet,[]):- !.
att([],_,unknown):- !.
att([F:V|_],F,V):- !.
att([_|L],F,V):- att(L,F,V).

/* -------------------------------------------------------------------------
   Word Senses
------------------------------------------------------------------------- */

sense(_Sym,_Cat,Sense,A-A):- att(A,sense,Sense), \+ Sense = 0, !.
sense(_Sym,_Cat,Sense,A-[sense:Sense|A]):- Sense = 1.


/* -------------------------------------------------------------------------
   Relations
------------------------------------------------------------------------- */

rel(_,Att-Att,Rel):- att(Att,relation,Rel), \+ Rel=unknown, !.
rel(Rel,Att-[relation:Rel|Att],Rel).

role(_,Att-Att,Roles):- att(Att,verbnet,Roles), \+ Roles=[], !.
role(Roles,Att-[verbnet:Roles|Att],Roles).


/* -------------------------------------------------------------------------
   Thematic Roles introduced by PPs
------------------------------------------------------------------------- */

roles(_,((s:X\np)\(s:X\np))/np,Roles,A-A):- option('--roles',verbnet), att(A,verbnet,Roles), \+ Roles=[], !.
roles(by,((s:X\np)\(s:X\np))/np,Roles,A-[verbnet:Roles|A]):- option('--roles',verbnet), !, Roles = ['Agent'].
roles(_,((s:X\np)\(s:X\np))/np,Roles,A-[verbnet:Roles|A]):- option('--roles',verbnet), !, Roles = ['Instrument'].
roles(_,((s:X\np)\(s:X\np))/np,Roles,A-A):- option('--roles',proto), !, Roles = ['Actor'].
roles(for,pp/(s:_\np),Roles,A-A):- option('--roles',verbnet), !, Roles = ['Goal'].
roles(_,pp/(s:_\np),Roles,A-A):- option('--roles',verbnet), !, Roles = ['Topic'].
roles(for,pp/s:_,Roles,A-A):- option('--roles',verbnet), !, Roles = ['Goal'].
roles(_,pp/s:_,Roles,A-A):- option('--roles',verbnet), !, Roles = ['Topic'].


/* -------------------------------------------------------------------------
   Thematic Roles: passive
------------------------------------------------------------------------- */

roles(Verb,(s:pss\np)/np,[Role1,Role2],A):- roles(Verb,((s:dcl\np)/np)/np,[Role1,Role2,_],A), !.
roles(Verb,(s:pss\np)/s:M,[Role1,Role2],A):- roles(Verb,((s:dcl\np)/s:M)/np,[Role1,Role2,_],A), !.
roles(Verb,(s:pss\np)/pp,[Role],A):- roles(Verb,((s:dcl\np)/pp)/np,[Role,_],A), !.
roles(Verb,((s:pss\np)/np)/pp,[Role1,Role2],A):- roles(Verb,(((s:dcl\np)/np)/pp)/np,[Role1,Role2,_],A), !.
roles(Verb,s:pss\np,[Role],A):- roles(Verb,(s:dcl\np)/np,[Role,_],A), !.
roles(Verb,((s:M\s:T)\np)/np,Roles,A):- roles(Verb,((s:M\np)/s:T)/np,Roles,A), !.

/* -------------------------------------------------------------------------
   Thematic Roles: standard case 
------------------------------------------------------------------------- */

roles(_,_,Roles,A-A):- option('--roles',verbnet), att(A,verbnet,Roles), \+ Roles=[], !.
roles(Verb,Cat,Roles,A-[verbnet:Roles|A]):- option('--roles',verbnet), verbnet(Verb,Cat,Roles), !.
roles(Verb,Cat,Roles,A-A):- option('--roles',proto), proto(Verb,Cat,Roles), !.


/* -------------------------------------------------------------------------
   Thematic Roles: fall-back rules
------------------------------------------------------------------------- */

roles(Verb,(s:M\np)\np,Roles,A):- !, roles(Verb,(s:M\np)/np,Roles,A).
roles(Verb,s:inv/np,Roles,A):- !, roles(Verb,s:dcl\np,Roles,A).
roles(Verb,(s:q/np)/np,Roles,A):- !, roles(Verb,(s:dcl\np)/np,Roles,A).
roles(Verb,(s:M\pp)/np,Roles,A):- !, roles(Verb,s:M\np,Roles,A).
roles(Verb,(s:M/np)/np,Roles,A):- !, roles(Verb,(s:M\np)/np,Roles,A).
roles(Verb,s:M/np,Roles,A):- !, roles(Verb,s:M\np,Roles,A).
roles(Verb,C/pp,Roles,A):- !, roles(Verb,C,Roles,A).
roles(Verb,(C/pp)/np,Roles,A):- !, roles(Verb,C/np,Roles,A).
roles(Verb,(C/pp)/s:X,Roles,A):- !, roles(Verb,C/s:X,Roles,A).
roles(Verb,(s:M\np)/(s:X\np),Roles,A):- !, roles(Verb,(s:M\np)/s:X,Roles,A).


/* -------------------------------------------------------------------------
   Thematic Roles: no roles could be assigned
------------------------------------------------------------------------- */

roles(Verb,Cat,Roles,A-A):- 
   warning('role assignment failure for ~p with category ~p',[Verb,Cat]),
   Roles = [].


/* -------------------------------------------------------------------------
   Proto (roles are listed in the order of arguments, not surface order!)
------------------------------------------------------------------------- */

proto(_, s:adj\np,           ['Holder']):- option('--semantics',amr), !.
proto(_, (s:adj\np)\np,      ['Theme','Holder']):- option('--semantics',amr), !.
proto(_, s:adj\np,           ['Theme']):- !.
proto(_, (s:adj\np)\np,      ['Theme','Pivot']):- !.
proto(_, s:_\np,             ['Actor']):- !.
proto(_, (s:_\np)/np,        ['Theme','Actor']):- !. 
proto(_, (s:_\np)/s:_,       ['Topic','Actor']):- !.
proto(_, ((s:_\np)/np)/np,   ['Recipient','Theme','Actor']):- !.
proto(_, ((s:_\np)/s:_)/np,  ['Recipient','Topic','Actor']):- !.


/* -------------------------------------------------------------------------
   Sentence
------------------------------------------------------------------------- */

category(s, s:X,    X).   


/* -------------------------------------------------------------------------
   VP adverbials
------------------------------------------------------------------------- */

category(vpadv, (s:X\np)\(s:X\np),     _).
category(vpadv, (s:X\np)/(s:X\np),     _).


/* -------------------------------------------------------------------------
   S modifiers
------------------------------------------------------------------------- */

category(smod, s:X/s:X,    _).
category(smod, s:X\s:X,    _).
category(smod, s:_/s:dcl,  _).
category(smod, s:dcl/s:inv, _).


/* -------------------------------------------------------------------------
   Complementisers
------------------------------------------------------------------------- */

category(comp, s:poss/s:dcl, _).
category(comp, s:qem/s:dcl, _).
category(comp, s:bem/s:b, _).
category(comp, s:em/s:dcl, _).
category(comp, s:em/s:b, _).
