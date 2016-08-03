
:- module(abbreviations,[iAbb/2,tAbb/2]).

/* ----------------------------------------------------------------------------------
   Transitive abbreviations (usually not at the end of a sentence)
---------------------------------------------------------------------------------- */

tAbb(L,A):- title(L,A).
tAbb(L,A):- coord(L,A).


/* ----------------------------------------------------------------------------------
   Intransitive abbreviations
---------------------------------------------------------------------------------- */

iAbb(L,A):- abb(L,A).


/* ----------------------------------------------------------------------------------
   Titles
---------------------------------------------------------------------------------- */

title(en, "Mr").           % sg
title(en, "Messrs").       % pl
title(en, "Mrs").          % sg
title(en, "Mmes").         % pl
title(en, "Ms").           % 
title(en, "Dr").           % sg
title(en, "Drs").          % pl
title(en, "Prof").         % 
title(en, "Sen").          % 
title(en, "Gov").          % 
title(en, "St").           % Saint
title(en, "Rep").          % 
title(en, "Gen").          % 
title(en, "Lt").           % Lieutenant
title(en, "Lieut").        % Lieutenant
title(en, "Col").          % Colonel
title(en, "Adm").          % Admiral
title(en, "Cpt").          % Captain
title(en, "Rev").          % Reverend
title(en, "Hon").          % Honoroble
title(en, "Capt").         % 
title(en, "Cmdr").         % 
title(en, "Chapln").       % 
title(en, "Mt").           % Mount


/* ----------------------------------------------------------------------------------
   Coordinators
---------------------------------------------------------------------------------- */

coord(en, "v").            % versus
coord(en, "vs").           % versus
coord(en, "no").           % number
coord(en, "No").           % number
coord(en, "Cie").          % ??
coord(en, "a.k.a").        % also known as
coord(en, "e.g").          % exempli gratia (for example)


/* ----------------------------------------------------------------------------------
   Abbreviations
---------------------------------------------------------------------------------- */

abb(Codes):- member(46,Codes), member(X,Codes), alpha(X), !.
abb(Abb):- option('--language',Language), abb(Language,Abb), !.

abb(en, "proC"). % Corp
abb(en, "cnI").  % Inc
abb(en, "oC").   % Co
abb(en, "dtL").  % Ltd
abb(en, "rJ").   % Jr
abb(en, "rS").   % Sr
abb(en, "soC").  % Cos
abb(en, "sorB"). % Bros
abb(en, "cte").  % etc

