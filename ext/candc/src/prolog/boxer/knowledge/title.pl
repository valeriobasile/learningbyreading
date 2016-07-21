
:- module(title,[title/1,title/2,title/3]).

:- use_module(library(lists),[member/2]).

title(X):- title(X,_,_).
title(X,Y):- title(X,Y,_).

% Titles starting with A 
title(abbot, abbot,       male).
title(T,     admiral,     person):- member(T,[adm,'adm.',admiral]).
title(T,     ambassador,  person):- member(T,[amb,'amb.',ambassador]).

% Titles starting with B 
title(baron,   baron,     male).
title(T,       baroness,  female):- member(T,[baroness,brnss,'brnss.']).
title(bishop,  bishop,    male).
title(T,       brigadire, person):- member(T,[brig,'brig.',brigadire]).
title(T,       brother,   male  ):- member(T,[br,'br.',brother]).

% Titles starting with C
title(T,             commodore,           person):- member(T,[cdre,cmdre,commodore]).
title(cpo,           chief_petty_officer, person).
title(T,             captain,             person):- member(T,[capt,cpt,'capt.','cpt.',captain]).
title(cardinal,      cardinal,            person).
title(T,             chansellor,          person):- member(T, [chan,chansellor]).
title(T,             chaplain,            person):- member(T, [chapln,'chapln.',chaplain]).
title(T,             colonel,             person):- member(T, [col,'col.',colonel]).
title(commandant,    commandant,          person).
title(T,             commander,           person):- member(T,[cmdr,'cmdr.',commander]).
title(commissioner,  commissioner,        person).
title(T,             corporal,            person):- member(T,[cpl,'cpl.',corporal]).
title(councillor,    councillor,          person).
title(count,         count,               male).
title(T,             countess,            female):- member(T,[countess,cntss,'cntss.']).

% Titles starting with D
title(dame,      dame,      female).
title(deacon,    deacon,    male).
title(deaconess, deaconess, female).
title(dean,      dean,      person).
title(T,         doctor,    person):- member(T,[dr,'dr.',drs,'drs.',doctor]).
title(duke,      duke,      person).

% Titles starting with E
title(T, ensign,    person):- member(T,[ensign,ens,'ens.']).

% Titles starting with F
title(T,    father, male):- member(T,[father,fr,'fr.']).
title(T,    friar,  male):- member(T,[friar,fr,'fr.']).
title(frau, frau,   female).

% Titles starting with G
title(T, general,  person):- member(T,[gen,'gen.',general]).
title(T, governor, person):- member(T,[gov,'gov.',governor]).

% Titles starting with H
title(T, honorary, person):- member(T,[hon,'hon.',honorary]).

% Titles starting with J
title(judge,   judge, person).
title(justice, justice, person).

% Titles starting with L
title(lady, lady, female).
title(T,    lieutenant, person):- member(T,[lieut,'lieut.',lt,'lt.']).
title(lord, lord, person).

% Titles starting with M
title(marshal, marshal,      person).
title(master,  master,       person).
title(T,       madame,       female):- member(T,[mme,madame,'mme.',mmes,'mmes.']).
title(T,       major,        person):- member(T,[maj,'maj.',major]).
title(T,       miss,         female):- member(T,[miss,ms,'ms.']).
title(T,       mister,       male)  :- member(T,[mr,'mr.',mister,monsier,messrs,'messrs.']).
title(T,       missis,       female):- member(T,[mrs,'mrs.',missis,missus]).
title(T,       mademoiselle, female):- member(T,['mademoiselle',mlle,'mlle.']).
title(T,       monsignor,    male)  :- member(T,[monsignor,'mgr.',mgr,msgr,'msgr.']).

% Titles starting with P
title(T,        president, person):- member(T,[pres,'pres.',president]).
title(prince,   prince,    male).
title(princess, princess,  female).
title(T,        professor, person):- member(T,[prof,'prof.',professor]).

% Titles starting with R
title(rabbi, rabbi, person).
title(T, representative, person):- member(T,[rep,'rep.',representative]).
title(T, reverend,       person):- member(T,[rev,'rev.',reverend,revs,'revs.']).

% Titles starting with S
title(T,      senator,  person):- member(T,[sen,'sen.',senator]).
title(T,      senor,    male):-   member(T,[sr,'sr.',senor]).
title(T,      senora,   female):- member(T,[sra,'sra.',senora]).
title(T,      senorita, female):- member(T,[srta,'srta.',senorita]).
title(T,      sergeant, female):- member(T,[sgt,'sgt.',sergeant]).
title(sheikh, sheikh,   person).
title(sir,    sir,      person).
title(T,      sister,   female):- member(T,[sr,'sr.',sister]).
title(T,      saint,    person):- member(T,[st,'st.',saint]).
