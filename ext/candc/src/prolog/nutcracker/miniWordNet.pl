
:- module(miniWordNet,[compConcepts/2,
                       compISA/0,
                       graphMWN/2,
                       outputMWN/2,
                       axiomsWN/1,
                       cutDownMWN/0,
                       checkConsBK/0,
                       addTopMWN/0,
                       sizeMWN/1,
                       clearMWN/0]).


/* ========================================================================
    Load Modules
======================================================================== */

:- use_module(library(lists),[member/2,append/3,select/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[error/2,warning/2]).
:- use_module(semlib(drs2fol),[symbol/4,timex/2]).


% :- use_module(knowledge(nationality),[nationality/2]).


/* ========================================================================
   Load WordNet
======================================================================== */

:- [working/wordnet/wordnet].

%:- [working/wordnet/vanilla_train_s].
%:- [working/wordnet/vanilla_train_l].
%:- [working/wordnet/vanilla_train_xxxl].

%:- [working/wordnet/vanilla_test_xl].

%:- [working/wordnet/human_test].

%:- [working/wordnet/elliesimple].
%:- [working/wordnet/ellie_train].

%:- [working/wordnet/combined].


/* ========================================================================
    Declare Dynamic Predicates
======================================================================== */

:- dynamic concept/2, % concept(Concept, ConceptId)
           isa/2,     % isa(ConceptId1, ConceptId2)
           isnota/2,  % isnota(ConceptId1, ConceptId2)
           word/5.    % word(Word,Cat,Sense,Freq,ConceptId),


/* ========================================================================
   Minimum Frequence for words under consideration
======================================================================== */

minFreq(0).


/* ========================================================================
   Clear Background Knowledge
======================================================================== */

clearMWN:-  
   retractall(word(_,_,_,_,_)),
   retractall(isa(_,_)),
   retractall(isnota(_,_)),
   retractall(concept(_,_)).


/* ========================================================================
   Compute Concepts for one DRS
======================================================================== */

compConcepts(DRS,DRS):-
   findSymDrs(DRS,[]-_,[]-Concepts),
   selectConcepts(Concepts), !.

compConcepts(DRS,DRS).


/* ========================================================================
   Get all symbols from a DRS
======================================================================== */

findSymDrs(smerge(A,B),X1-X3,R1-R2):- !,
   findSymDrs(A,X1-X2,R1-R3),  
   findSymDrs(B,X2-X3,R3-R2).

findSymDrs(merge(A,B),X1-X3,R1-R2):- !,
   findSymDrs(A,X1-X2,R1-R3),  
   findSymDrs(B,X2-X3,R3-R2).

findSymDrs(alfa(_,A,B),X1-X3,R1-R2):- !,
   findSymDrs(A,X1-X2,R1-R3),  
   findSymDrs(B,X2-X3,R3-R2).

findSymDrs(drs([_:Y1|Dom],Conds),X1-X3,R1-R2):- 
   select(Y2,X1,X2), Y1==Y2, !,
   findSymDrs(drs(Dom,Conds),X2-X3,R1-R2).

findSymDrs(drs([_|Dom],Conds),X1-X2,R1-R2):- 
   findSymDrs(drs(Dom,Conds),X1-X2,R1-R2).

findSymDrs(drs([],Conds),X1-X2,R1-R2):- !,
   findSymConds(Conds,X1-X2,R1-R2).


/* ========================================================================
   Get all symbols from a DRS-Condition
======================================================================== */

findSymConds([],X-X,R-R).

findSymConds([_:imp(B1,B2)|L],X1-X3,R1-R4):- !,
   findSymDrs(B1,X1-X2,R1-R2), 
   findSymDrs(B2,X2-_,R2-R3),   
   findSymConds(L,X1-X3,R3-R4).

findSymConds([_:or(B1,B2)|L],X1-X2,R1-R4):- !,
   findSymDrs(B1,X1-_,R1-R2),
   findSymDrs(B2,X1-_,R2-R3),   
   findSymConds(L,X1-X2,R3-R4).

findSymConds([_:duplex(_,B1,_,B2)|L],X1-X3,R1-R4):- !,
   findSymDrs(B1,X1-X2,R1-R2),
   findSymDrs(B2,X2-_,R2-R3),   
   findSymConds(L,X1-X3,R3-R4).
   
findSymConds([_:not(B)|L],X1-X2,R1-R3):- !,
   findSymDrs(B,X1-_,R1-R2),
   findSymConds(L,X1-X2,R2-R3).

findSymConds([_:pos(B)|L],X1-X2,R1-R3):- !,
   findSymDrs(B,X1-_,R1-R2),
   findSymConds(L,X1-X2,R2-R3).

findSymConds([_:nec(B)|L],X1-X2,R1-R3):- !,
   findSymDrs(B,X1-_,R1-R2),
   findSymConds(L,X1-X2,R2-R3).

findSymConds([_:prop(_,B)|L],X1-X2,R1-R3):- !,
   findSymDrs(B,X1-_,R1-R2),
   findSymConds(L,X1-X2,R2-R3).

findSymConds(L1,X1-X2,R1-R2):-
   select(_:pred(Y1,Sym,n,Sense),L1,L2), !,
   ( stoplist(Sym,n), !, findSymConds(L2,X1-X2,R1-R2) 
   ; member(Y0,X1), Y1==Y0, !, warning('symbol ~p not selected for MWN',[Sym]), findSymConds(L2,X1-X2,R1-R2)
   ; findSymConds(L2,[Y1|X1]-X2,[s(Sym,n,Sense)|R1]-R2) ).

findSymConds(L1,X1-X2,R1-R2):-
   select(_:pred(Y,Sym,v,Sense),L1,L2), !,
   ( stoplist(Sym,v), !, findSymConds(L2,X1-X2,R1-R2) 
   ; findSymConds(L2,[Y|X1]-X2,[s(Sym,v,Sense),s(event,n,1)|R1]-R2) ).

findSymConds(L1,X1-X2,R1-R2):-
   select(_:pred(Y,Sym,a,Sense),L1,L2), !,
   ( stoplist(Sym,a), !, findSymConds(L2,X1-X2,R1-R2) 
   ; findSymConds(L2,[Y|X1]-X2,[s(Sym,a,Sense)|R1]-R2) ).

findSymConds(L1,X1-X2,R1-R2):-
   select(_:pred(Y,Sym,r,Sense),L1,L2), !,
   ( stoplist(Sym,r), !, findSymConds(L2,X1-X2,R1-R2) 
   ; findSymConds(L2,[Y|X1]-X2,[s(Sym,r,Sense)|R1]-R2) ).

findSymConds([_:card(_,N,_)|L],X1-X2,R1-[s(Sym,cardinal,1),s(numeral,n,1)|R2]):- 
   option('--plural',false),
   integer(N), N > 0, !,
   symbol(c,number,N,Sym),
   findSymConds(L,X1-X2,R1-R2).

findSymConds([_:timex(Y1,T)|L],X1-X2,R1-[s(Sym,timex,1),s(time,n,5)|R2]):- 
   \+ (member(Y0,X1), Y1==Y0), !,
   timex(T,Sym),
   findSymConds(L,[Y1|X1]-X2,R1-R2).

findSymConds([_:named(Y1,Sym,Cat,Sense)|L],X1-X2,R1-[s(Sym,Cat,Sense)|R2]):-
%  \+ (member(Y0,X1), Y0==Y1), 
   !,
   findSymConds(L,[Y1|X1]-X2,R1-R2).

findSymConds([_|L],X1-X2,R1-R2):-
   findSymConds(L,X1-X2,R1-R2).


/* ========================================================================
   Select relevant nouns and verbs as concepts
======================================================================== */

selectConcepts([]).

selectConcepts([s(_,ttl,_)|L]):- !,       % ignore titles
   selectConcepts(L).

selectConcepts([s(X,Cat,0)|L]):- !,       % WSD (most frequent sense)
   selectConcepts([s(X,Cat,1)|L]).

%selectConcepts([c(X,Syms,Cat,0)|L]):- !,  % WSD (most frequent sense)
%   selectConcepts([c(X,Syms,Cat,1)|L]).

selectConcepts([s(X,Cat,Sense)|L]):- 
   word(X,Cat,Sense,_,ID), !,             % if word (token) is already
   retract(word(X,Cat,Sense,N1,ID)),      % in the database
   N2 is N1 + 1,                          % then increase frequency
   assert(word(X,Cat,Sense,N2,ID)),
   selectConcepts(L).

selectConcepts([s(Sym1,Cat,Sense1)|L]):- 
   concept(Concept,ID),                   % if word is not in the database
   member(s(Sym2,Cat,Sense2),Concept),    % but a synonym is, then
   syn(Cat,Sym1,Sense1,Sym2,Sense2), !,   % extend list of synonyms
   assert(word(Sym1,Cat,Sense1,1,ID)),
   retract(concept(Concept,ID)),
   assert(concept([s(Sym1,Cat,Sense1)|Concept],ID)),
   selectConcepts(L).

selectConcepts([s(X,Cat,Sense)|L]):- !,
   getConceptId(ID),                      % else add word
   assert(word(X,Cat,Sense,1,ID)),        % and its concept to database
   assert(concept([s(X,Cat,Sense)],ID)),
   selectConcepts(L).

%selectConcepts([c(X,_,Cat,Sense)|L]):-
%   word(X,Cat,Sense,_,ID), !,             % if name is already
%   retract(word(X,Cat,Sense,N1,ID)),      % in the database
%   N2 is N1 + 1,                          % then increase frequency
%   assert(word(X,Cat,Sense,N2,ID)),
%   selectConcepts(L).

%selectConcepts([c(X,Symbols,Cat,Sense)|L]):- !,
%   getConceptId(ID),
%   assert(word(X,Cat,Sense,1,ID)),
%   findall(s(Sym,Cat,Sense),member(Sym,Symbols),Syn),
%   assert(concept([s(X,Cat,Sense)|Syn],ID)),
%   selectConcepts(L).

selectConcepts([_|L]):-
   selectConcepts(L).


/* ========================================================================
   Synonyms
======================================================================== */

syn(n,Sym1,Sense1,Sym2,Sense2):-
   synn(Sym1,Sense1,Sym2,Sense2), !.    %%% WordNet (nouns)

syn(n,Sym1,Sense1,Sym2,Sense2):-
   synn(Sym2,Sense2,Sym1,Sense1), !.    %%% WordNet (nouns)

syn(v,Sym1,Sense1,Sym2,Sense2):-
   synv(Sym1,Sense1,Sym2,Sense2), !.    %%% WordNet (verbs)

syn(v,Sym1,Sense1,Sym2,Sense2):-
   synv(Sym2,Sense2,Sym1,Sense1), !.    %%% WordNet (verbs)

syn(a,Sym1,Sense1,Sym2,Sense2):-
   syna(Sym1,Sense1,Sym2,Sense2), !.    %%% WordNet (adjectives)

syn(a,Sym1,Sense1,Sym2,Sense2):-
   syna(Sym2,Sense2,Sym1,Sense1), !.    %%% WordNet (adjectives)

syn(r,Sym1,Sense1,Sym2,Sense2):-
   synr(Sym1,Sense1,Sym2,Sense2), !.    %%% WordNet (adverbs)

syn(r,Sym1,Sense1,Sym2,Sense2):-
   synr(Sym2,Sense2,Sym1,Sense1), !.    %%% WordNet (adverbs)

syn(per,Sym1,Sense1,Sym2,Sense2):-
   synp(Sym1,Sense1,Sym2,Sense2), !.    %%% WordNet (names)

syn(loc,Sym1,Sense1,Sym2,Sense2):-
   synp(Sym1,Sense1,Sym2,Sense2), !.    %%% WordNet (names)

syn(org,Sym1,Sense1,Sym2,Sense2):-
   synp(Sym1,Sense1,Sym2,Sense2), !.    %%% WordNet (names)


/* ========================================================================
   Calculate WordNet ISA Relations (first select all relevant concepts)
======================================================================== */

compISA:- 
   minFreq(Min),
   setof(s(X,Cat,Sense,ID),N^(word(X,Cat,Sense,N,ID),N>Min),L), !,
   compISA(L),
   compISNOTA(L).

compISA.


/* ========================================================================
   Calculate WordNet ISA Relations
======================================================================== */

compISA([]).

compISA([s(_,_,_,ID)|L]):-
   isa(ID,_), !,                         % already calculated superconcept
   compISA(L).


/* ------------------------------------------------------------------------
   Time and Numeric Expressions
------------------------------------------------------------------------ */

compISA([s(_,timex,1,ID1)|L]):- !,       % timex
   addConcept(time,n,5,ID2),
   assert(isa(ID1,ID2)),
   compISA([s(time,n,5,ID2)|L]).

compISA([s(_,cardinal,1,ID1)|L]):- !,    % cardinal
   addConcept(numeral,n,1,ID2),
   assert(isa(ID1,ID2)),
   compISA([s(numeral,n,1,ID2)|L]).

/* ------------------------------------------------------------------------
   Locations
------------------------------------------------------------------------ */

%compISA([s(Sym1,loc,Sense1,ID1)|L]):-    % location in WordNet
%   isap(Sym1,Sense1,Sym2,Sense2), !,
%   addConcept(Sym2,n,Sense2,ID2),
%   assert(isa(ID1,ID2)),
%   compISA([s(Sym2,n,Sense2,ID2)|L]).

compISA([s(_,loc,_,ID1)|L]):-            % location not in WordNet
   addConcept(location,n,1,ID2), !,
   assert(isa(ID1,ID2)),
   compISA([s(location,n,1,ID2)|L]).

/* ------------------------------------------------------------------------
   Organisations
------------------------------------------------------------------------ */

%compISA([s(Sym1,org,Sense1,ID1)|L]):-    % organisation in WordNet
%   isap(Sym1,Sense1,Sym2,Sense2), !,
%   addConcept(Sym2,n,Sense2,ID2),
%   assert(isa(ID1,ID2)),
%   compISA([s(Sym2,n,Sense2,ID2)|L]).

compISA([s(_,org,_,ID1)|L]):-            % organisation not in WordNet
   addConcept(organisation,n,1,ID2), !,
   assert(isa(ID1,ID2)),
   compISA([s(organisation,n,1,ID2)|L]).

/* ------------------------------------------------------------------------
   Persons
------------------------------------------------------------------------ */

%compISA([s(Sym1,per,Sense1,ID1)|L]):-    % person in WordNet
%   isap(Sym1,Sense1,Sym2,Sense2), !,
%   addConcept(Sym2,n,Sense2,ID2),
%   assert(isa(ID1,ID2)),
%   compISA([s(Sym2,n,Sense2,ID2)|L]).

compISA([s(_,per,_,ID1)|L]):-            % person not in WordNet
   addConcept(somebody,n,1,ID2), !,
   assert(isa(ID1,ID2)),
   compISA([s(somebody,n,1,ID2)|L]).

/* ------------------------------------------------------------------------
   Nouns
------------------------------------------------------------------------ */

% take all isa relations
%
compISA([s(Sym1,n,Sense1,ID1)|L1]):-      % nouns in WordNet
%   option('--x',true),
   findall(s(Sym2,n,Sense2,ID2),( isan(Sym1,Sense1,Sym2,Sense2),
                                  addConcept(Sym2,n,Sense2,ID2),
                                  assert(isa(ID1,ID2)) ), New),
   New = [_|_], !,
   append(New,L1,L2),
   compISA(L2).

% take only first isa relation
%
%compISA([s(Sym1,n,Sense1,ID1)|L]):-      % nouns in WordNet
%   option('--x',false),
%   isan(Sym1,Sense1,Sym2,Sense2), !,
%   addConcept(Sym2,n,Sense2,ID2),
%   assert(isa(ID1,ID2)),
%   compISA([s(Sym2,n,Sense2,ID2)|L]).

/* ------------------------------------------------------------------------
   Verbs
------------------------------------------------------------------------ */

compISA([s(Sym1,v,Sense1,ID1)|L]):-      % verbs in WordNet (+ ISA)
   isav(Sym1,Sense1,Sym2,Sense2), !,
   addConcept(Sym2,v,Sense2,ID2),
   assert(isa(ID1,ID2)),
   compISA([s(Sym2,v,Sense2,ID2)|L]).

compISA([s(Sym,v,Sense,_)|L]):-          % verbs in WordNet (- ISA)
   synv(Sym,Sense,_,_), !,
   compISA(L).

compISA([s(Sym,v,Sense,_)|L]):-          % verbs in WordNet (- ISA)
   isav(_,_,Sym,Sense), !,
   compISA(L).

/* ------------------------------------------------------------------------
   All Other Cases
------------------------------------------------------------------------ */

compISA([s(Sym1,nam,Sense1,ID1)|L]):-    % named entity in WordNet
   isap(Sym1,Sense1,Sym2,Sense2), !,
   addConcept(Sym2,n,Sense2,ID2),
   assert(isa(ID1,ID2)),
   compISA([s(Sym2,n,Sense2,ID2)|L]).

compISA([s(Sym,Cat,Sense,ID)|L]):-       % outside WordNet
   word(Sym,Cat,Sense,F,ID), 
   minFreq(Min), F > Min, !,
   warning('unclassified token: ~p~p~p',[Cat,Sense,Sym]),
   compISA(L).

compISA([_|L]):-
   compISA(L).



/*========================================================================
   compISNOTA
========================================================================*/

compISNOTA([]).

compISNOTA([s(Sym1,a,Sense1,ID1)|L]):-    
   isnotaa(Sym1,Sense1,Sym2,Sense2), 
   member(s(Sym2,a,Sense2,ID2),L), !,
   assert(isnota(ID1,ID2)),
   compISNOTA(L).

compISNOTA([_|L]):-    
   compISNOTA(L).


/*========================================================================
    Get a new concept Id
========================================================================*/

getConceptId(I3):-
   concept(_,I1),
   \+ ( concept(_,I2), I2 > I1 ), !,
   I3 is I1 + 1.

getConceptId(1).


/*========================================================================
   Calculate First-Order Axioms 
========================================================================*/

axiomsWN(Axioms):-
   option('--modal',false), !,
   findall(isa(A,B),(isa(A,B),\+A=B,\+B=0),ISA),

   findall(isnota(A,B),isnota(A,B),ISNOTA),
%  co-hyponyms
%   findall(isnota(A,B),(isa(A,C), concept([s(_,T,_)|_],A),
%                        isa(B,C), concept([s(_,T,_)|_],B),
%                        A<B       ),ISNOTA),

   append(ISA,ISNOTA,Ax1),
   findall(iseq(A,B),(concept([A|L],Id1),
                      member(B,L),
                      \+ (concept(Other,Id2), \+ Id1=Id2, member(B,Other))),ISEQ),
   append(ISEQ,Ax1,Ax2),
   axiomsWN(Ax2,Axioms).

axiomsWN(Axioms):-
   option('--modal',true), !,
   findall(isa(A,B),(isa(A,B),\+A=B,\+B=0),Ax1),
   findall(iseq(A,B),(concept([A|L],Id1),
                      member(B,L),
                      \+ (concept(Other,Id2), \+ Id1=Id2, member(B,Other))),ISEQ),
   append(ISEQ,Ax1,Ax2),
   axiomsWN(Ax2,Axioms).


/*========================================================================
   Calculate First-Order Axioms given isa/2, isnota/2, iseq/2
========================================================================*/

axiomsWN([],[]):- !.

axiomsWN([isa(I1,I2)|L1],[Axiom|L2]):- !,
   isa2fol(I1,I2,Axiom), 
   axiomsWN(L1,L2).

axiomsWN([isnota(I1,I2)|L1],[Axiom|L2]):-
   option('--contradiction',true), !,
   isnota2fol(I1,I2,Axiom), 
   axiomsWN(L1,L2).

axiomsWN([iseq(S1,S2)|L1],[Axiom|L2]):- !,
   iseq2fol(S1,S2,Axiom), 
   axiomsWN(L1,L2).

axiomsWN([_|L1],L2):- !,
   axiomsWN(L1,L2).


/*========================================================================
   Translate IS-A to FOL 
========================================================================*/

isa2fol(I1,I2,Axiom):-
   option('--modal',false), !,
   concept2sym(I1,B1),
   concept2sym(I2,B2),
   Axiom = all(X,imp(F1,F2)),
   F1 =.. [B1,X],
   F2 =.. [B2,X].

isa2fol(I1,I2,Axiom):-
   option('--modal',true), !,
   concept2sym(I1,B1),
   concept2sym(I2,B2),
   Axiom = all(W,imp(possible_world(W),all(X,imp(F1,F2)))),
   F1 =.. [B1,W,X],
   F2 =.. [B2,W,X].


/*========================================================================
   Translate IS-NOT-A to FOL 
========================================================================*/

isnota2fol(I1,I2,Axiom):-
   option('--modal',false), !,
   concept2sym(I1,B1),
   concept2sym(I2,B2),
   Axiom = all(X,imp(F1,not(F2))),
   F1 =.. [B1,X],
   F2 =.. [B2,X].

isnota2fol(I1,I2,Axiom):-
   option('--modal',true), !,
   concept2sym(I1,B1),
   concept2sym(I2,B2),
   Axiom = all(W,imp(possible_world(W),all(X,imp(F1,not(F2))))),
   F1 =.. [B1,W,X],
   F2 =.. [B2,W,X].


/*========================================================================
   Translate IS-EQUAL to FOL 
========================================================================*/

iseq2fol(s(A1,T1,S1),s(A2,T2,S2),Axiom):-
   option('--modal',false), !,
   symbol(T1,A1,S1,B1),
   symbol(T2,A2,S2,B2),
   Axiom = all(X,bimp(F1,F2)),
   F1 =.. [B1,X],
   F2 =.. [B2,X].

iseq2fol(s(A1,T1,S1),s(A2,T2,S2),Axiom):-
   option('--modal',true), !,
   symbol(T1,A1,S1,B1),
   symbol(T2,A2,S2,B2),
   Axiom = all(W,imp(possible_world(W),all(X,bimp(F1,F2)))),
   F1 =.. [B1,W,X],
   F2 =.. [B2,W,X].


/*========================================================================
   Translate concept to symbol
========================================================================*/

concept2sym(I,Sym):-
   concept([s(Sym,timex,1)|_],I), !.

concept2sym(I,Sym):-
   concept([s(Sym,cardinal,1)|_],I), !.

concept2sym(I,B):-
   concept([s(Sym,Type,Sense)|_],I), !,
   symbol(Type,Sym,Sense,B).


/*========================================================================
   Print MiniWordNet 
========================================================================*/

graphMWN(Dir,File):-
   option('--graph',true),
   atomic_list_concat([Dir,'/',File,'.mwn','.dot'],DOT),
   atomic_list_concat([Dir,'/',File,'.mwn','.pdf'],PDF),
   open(DOT,write,Stream),
   format(Stream,'digraph mwn {~n~n',[]),
   setof(I,X^concept(X,I),Nodes),
   printNodes(Nodes,Stream),
   printIsa(Stream),
   close(Stream),
   createGraph(DOT,PDF), !.

graphMWN(_,_).


/*========================================================================
   Create Graph using the dot program
========================================================================*/

createGraph(File1,File2):-
   shell('which dot',0), 
   atomic_list_concat([dot,File1,'-Tpdf >',File2],' ',Command),
   shell(Command,0), !.

createGraph(_,_):-
   error('failed to execute the dot program',[]).


/*------------------------------------------------------------------------
   Print Nodes
------------------------------------------------------------------------*/

printNodes([],Stream):- !, nl(Stream).

printNodes([I|L],Stream):- 
   concept(X,I), !,
   format(Stream,'   ~p [label="',[I]),
   printNode(X,Stream),
   printNodes(L,Stream).

printNodes([_|L],Stream):- printNodes(L,Stream).


/*------------------------------------------------------------------------
   Print Single Node
------------------------------------------------------------------------*/

printNode([s(Sym,Cat,Sense)],Stream):-
   frequency(Sym,Cat,Sense,F), !,
   format(Stream,'~p-~p-~p (F=~p)"];~n',[Sym,Cat,Sense,F]).

printNode([s(Sym,Cat,Sense)|L],Stream):-
   frequency(Sym,Cat,Sense,F), !,
   format(Stream,'~p-~p-~p (F=~p)\\n',[Sym,Cat,Sense,F]),
   printNode(L,Stream).


/*------------------------------------------------------------------------
   Word Frequency
------------------------------------------------------------------------*/

frequency(X,C,S,F):-
   word(X,C,S,F,_), !.

frequency(_,_,_,0).


/*------------------------------------------------------------------------
   Print Relations
------------------------------------------------------------------------*/

printIsa(Stream):-
   isa(I,J), 
   format(Stream,'   ~p -> ~p;~n',[J,I]),
   fail.

printIsa(Stream):- 
   format(Stream,'~n}~n',[]).


/*========================================================================
   Add Concept 
========================================================================*/

addConcept(Sym,Cat,Sense,ID):-
   concept(Concept,ID),
   member(s(Sym,Cat,Sense),Concept), !.

addConcept(Sym1,Cat,Sense1,ID):-
   concept(Concept,ID),
   member(s(Sym2,Cat,Sense2),Concept), 
   syn(Cat,Sym1,Sense1,Sym2,Sense2),
   retract(concept(Concepts,ID)),
   assert(concept([s(Sym1,Cat,Sense1)|Concepts],ID)), !.  

addConcept(Sym,Cat,Sense,ID):-
   getConceptId(ID),
   assert(concept([s(Sym,Cat,Sense)],ID)).


/*========================================================================
   Check for inconsistencies
========================================================================*/

checkConsBK:-
   isa(A,B),
   isa(A,C), \+ C=B, !,
   format('inconsistent node ~p has two parents ~p and ~p~n',[A,B,C]).

checkConsBK.


/*========================================================================
   Cut down redundant nodes
========================================================================*/

cutDownMWN:-!,
   findall(I,concept(_,I),C), 
   cutDown(C).

cutDown([]).

cutDown([A|L]):-
   isa(A,B),                       %%% remove "hanging" nodes that are not
   \+ isa(_,A),                    %%% terminal symbols
   \+ word(_,_,_,_,A), !,
   retract(isa(A,B)),
   retract(concept(_,A)),
   cutDown(L).

cutDown([B|L]):-
   isa(A,B), isa(B,C),              %%% two other nodes, but not for nodes that have an instance
   \+ word(_,_,_,_,B),
   \+ (isa(D,B), \+ D=A), !,
   retract(isa(A,B)),
   retract(isa(B,C)),
   retract(concept(_,B)),
   assert(isa(A,C)),
   cutDown(L).

cutDown([_|L]):-
   cutDown(L).


/*========================================================================
   Add Top Concept (if there is not one yet)
========================================================================*/

addTopMWN:-
   concept(E,B), member(s(event,n,1),E),     %%% place all verbs under
   concept(V,A), member(s(_,v,_),V),         %%% the event-1 concept
   \+ isa(A,_), !,
   assert(isa(A,B)), 
   addTopMWN.

addTopMWN:-   
   concept(_,B), \+ isa(B,_),                %%% if there are at least two
   concept(_,C), \+ B=C, \+ isa(C,_), !,     %%% different nodes B and C without parent
   assert(concept([s('_top',n,0)],0)),       %%% then introduce a common top node
   assert(isa(B,0)),
   assert(isa(C,0)),
   addTopMWN1.

addTopMWN.

addTopMWN1:-                                 %%% second part of introducing top node
   concept([s('_top',n,0)],I),               %%% introduce top for all other nodes
   concept(_,B), \+ B=I,                     %%% without parents
   \+ isa(B,_), !,
   assert(isa(B,I)),
   addTopMWN1.

addTopMWN1.



/* ========================================================================
   Size WordNet (number of disjoint concepts)
======================================================================== */

sizeMWN(Size):-
   findall(X,isa(X,_),L),
   length(L,Size).


/* ========================================================================
   Symbol Stop List
======================================================================== */

stoplist(')',_).
stoplist('(',_).
stoplist('"',_).
stoplist('%',_).
stoplist('$',_).
stoplist('=',_).
stoplist('*',_).
stoplist('/',_).
stoplist('#',_).
stoplist('+',_).
stoplist('\\',_).
stoplist('>',_).
stoplist('<',_).
stoplist('\240',_).

stoplist(quantity,n).
stoplist(more,n).


/*========================================================================
   Output MWN (prolog)
========================================================================*/

outputMWN(Dir,File):-
   atomic_list_concat([Dir,'/',File,'.mwn'],Name),
   open(Name,write,Stream),
   printTerminals(Stream),
   close(Stream).


/*========================================================================
   Output terminal symbols
========================================================================*/

printTerminals(Stream):-
   format(Stream,'%~n% word(+Word, +Cat, +Sense, +Frequency, +ConceptID).~n%~n',[]),
   word(X,C,S,N,I), \+ I = 0,
   format(Stream,'~q.~n',[word(X,C,S,N,I)]),
   fail.

printTerminals(Stream):-
   format(Stream,'%~n% concept(+SynSet, +ConceptID).~n%~n',[]), 
   concept(I,J),
   format(Stream,'~q.~n',[concept(I,J)]),
   fail.

printTerminals(Stream):-
   format(Stream,'%~n% isa(+SubConceptID, +SuperConceptID).~n%~n',[]),
   isa(I,J),
   format(Stream,'~p.~n',[isa(I,J)]),
   fail.

printTerminals(Stream):-
   format(Stream,'%~n% isnota(+ConceptID, +ConceptID).~n%~n',[]),
   isnota(I,J),
   format(Stream,'~p.~n',[isnota(I,J)]),
   fail.

printTerminals(_).


/*========================================================================
   Adjacent Indexes
========================================================================*/

adjacent([I],[J]):- !, J is I + 1.
adjacent(Is,Js):- member(I,Is), member(J,Js), J is I + 1, !.



/*------------------------------------------------------------------------

   ALL BELOW IS NOT USED CURRENTLY --- SUBJECT TO REVISION!

   Unknown word is a nationality

unknownWord(Sym,_,Freq1):-
   nationality(Sym,_),
   minFreq(Min),
   word(inhabitant,n,Freq2,Id), 
   Freq2 > Min,
   getConceptId(NewId),
   assert(word(Sym,p,Freq1,NewId)),
   assert(concept([Sym],NewId)),
   assert(isa(NewId,Id)), !.
------------------------------------------------------------------------*/


/*------------------------------------------------------------------------
   Unknown word is known as a compound    
   (e.g. website -> web site)

unknownWord(Sym1,n,Freq1):-
   minFreq(Min),
   name(Sym1,Codes1),
   append(Prefix,Postfix,Codes1),
   append(Prefix,[95|Postfix],Codes2),
   name(Sym2,Codes2),
   word(Sym2,Cat,Freq2,Id), 
   Freq2 > Min,
   \+ Id=0,
   assert(word(Sym1,Cat,Freq1,Id)),
   retract(concept(Concepts,Id)),
   assert(concept([Sym1|Concepts],Id)), !,  
   true.
%   format('Added word ~p similar to ~p~n',[Sym1,Sym2]).
------------------------------------------------------------------------*/
   

/*------------------------------------------------------------------------
   Unknown compound word is known as a non compound 
   (e.g. web site -> website)

unknownWord(Sym1,n,Freq1):-
   minFreq(Min),
   name(Sym1,Codes1),
   append(Prefix,[95|Postfix],Codes1),
   append(Prefix,Postfix,Codes2),
   name(Sym2,Codes2),
   word(Sym2,Cat,Freq2,Id), 
   Freq2 > Min,
   assert(word(Sym1,Cat,Freq1,Id)),
   retract(concept(Concepts,Id)),
   assert(concept([Sym1|Concepts],Id)), !,
   true.
%   format('Added word ~p similar to ~p~n',[Sym1,Sym2]).
------------------------------------------------------------------------*/

   
/*------------------------------------------------------------------------
   Unknown compound word is hyponym of existing word
   E.g. winter_sport ISA sport

unknownWord(Sym1,n,Freq1):-
   minFreq(Min),
   name(Sym1,Codes1),
   append(_,[95|Codes2],Codes1),
   name(Sym2,Codes2),
   word(Sym2,Cat,Freq2,Id), 
   Freq2 > Min,
   getConceptId(NewId),
   assert(word(Sym1,Cat,Freq1,NewId)),
   assert(concept([Sym1],NewId)),
   assert(isa(NewId,Id)), !,
   true. 
%   format('Hyponym word ~p of ~p~n',[Sym1,Sym2]).
------------------------------------------------------------------------*/


/*------------------------------------------------------------------------
   Unknown hyphenated word is hyponym of existing word
   E.g. Asian-American ISA American

unknownWord(Sym1,Cat,Freq1):-
   member(Cat,[n,p]),
   minFreq(Min),
   name(Sym1,Codes1),
   append(_,[45|Codes2],Codes1),
   name(Sym2,Codes2),
   word(Sym2,Cat,Freq2,Id), 
   Freq2 > Min,
   getConceptId(NewId),
   assert(word(Sym1,Cat,Freq1,NewId)),
   assert(concept([Sym1],NewId)),
   assert(isa(NewId,Id)), !,
   true. 
%   format('Hyponym word ~p of ~p~n',[Sym1,Sym2]).
------------------------------------------------------------------------*/

   




