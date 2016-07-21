
:- module(idioms,[idiom/2]).

:- use_module(slashes).

/* ----------------------------------------------------------
   Idiom: Guardia di Finanza
---------------------------------------------------------- */

idiom(t(n,POS,I,'GUARDIA_DI_FINANZA'),
      fa(n,t(n/pp,POS,I,'Guardia'),
           fa(pp,t(pp/n,'PREP',I,'di'),
                 t(n,'NOU~CS',I,'Finanza')))).

/* ----------------------------------------------------------
   Idiom: punto di vista
---------------------------------------------------------- */

idiom(t(n,POS,I,'PUNTO_DI_VISTA'),
      fa(n,t(n/pp,POS,I,'punto'),
           fa(pp,t(pp/n,'PREP',I,'di'),
                 t(n,'NOU~CS',I,'vista')))).

/* ----------------------------------------------------------
   Idiom: di piu`
---------------------------------------------------------- */

idiom(t(pp,'ADVB',I,'DI_PIÙ'),
      fa(pp,t(pp/np,'PREP',I,di),
            t(np,'ADVB',I,'più'))).

idiom(t(s:X\s:X,'ADVB',I,'DI_PIÙ'),
      fa(s:X\s:X,t((s:X\s:X)/np,'PREP',I,di),
            t(np,'ADVB',I,'più'))).

%idiom(t((X\X)/np,'PREP',I,'IN_VISTA_DI'),
%      fa(pp,t(X\X/np,'PREP',I,di),
%            t(np,'ADVB',I,'più'))).

/* ----------------------------------------------------------
   Idiom: di rigore
---------------------------------------------------------- */

idiom(t(n\n,'ADJ~QU',I,'DI_RIGORE'),
      fa(n\n,t((n\n)/np,'PREP',I,di),
             tc(np,n,t(n,'NOU~CP',I,'rigore')))).

/* ----------------------------------------------------------
   Compound Locations/Organisations
---------------------------------------------------------- */

idiom(t(n,'NOU~PR',I,'SAN_MARINO'),
      fa(n,t(n/n,'NOU~PR',I,'San'),
           t(n,'NOU~PR',I,'Marino'))).

idiom(t(n,'NOU~PR',I,'UNIONE_EUROPEA'),
      ba(n,t(n,'NOU~PR',I,'Unione'),
           t(n\n,'NOU~PR',I,'Europea'))).

idiom(t(n,'NOU~PR',I,'UNIONE_MUSICALE'),
      ba(n,t(n,'NOU~PR',I,'Unione'),
           t(n\n,'NOU~PR',I,'Musicale'))).

idiom(t(n,'NOU~PR',I,'STATI_UNITI'),
      ba(n,t(n,'NOU~PR',I,'Stati'),
           t(n\n,'NOU~PR',I,'Uniti'))).


/* ----------------------------------------------------------
   Awards
---------------------------------------------------------- */

idiom(t(n,'NOU~PR',I,'|PALLONE_D\'_ORO|'),
      ba(n,t(n,'NOU~PR',I,'Pallone'),
           fa(n\n,t((n\n)/n,'PREP',I,'d\''),
                  t(n,'NOU~PR',I,'Oro')))).
