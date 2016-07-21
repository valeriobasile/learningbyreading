:- module(ne,[neClass/2,neClassType/3,neClassType/4]).

neClass(N,C):- neClassType(N,C,_,_).
neClassType(N,C,T):- neClassType(N,C,T,_).

neClassType('I-LOC',geo,nam,'LOC'):- !.
neClassType('B-LOC',geo,nam,'LOC'):- !.
neClassType('E-LOC',geo,nam,'LOC'):- !.

neClassType('I-ORG',org,nam,'ORG'):- !.
neClassType('B-ORG',org,nam,'ORG'):- !.
neClassType('E-ORG',org,nam,'ORG'):- !.

neClassType('I-PER',per,nam,'PER'):- !.
neClassType('B-PER',per,nam,'PER'):- !.
neClassType('E-PER',per,nam,'PER'):- !.

neClassType('I-DAT',tim,nam,'TIM'):- !.
neClassType('B-DAT',tim,nam,'TIM'):- !.
neClassType('E-DAT',tim,nam,'TIM'):- !.

neClassType('I-TIM',tim,nam,'TIM'):- !.
neClassType('B-TIM',tim,nam,'TIM'):- !.
neClassType('E-TIM',tim,nam,'TIM'):- !.

neClassType('I-MON',geo,nam,'UOM'):- !.
neClassType('B-MON',geo,nam,'UOM'):- !.
neClassType('E-MON',geo,nam,'UOM'):- !.

neClassType('Person',per,nam,'PER'):- !.
neClassType('Organization',org,nam,'ORG'):- !.
neClassType('Location',geo,nam,'LOC'):- !.
neClassType('Artifact',art,nam,'ART'):- !.
neClassType('Event',eve,nam,'EVE'):- !.
neClassType('Natural_Object',nat,nam,'NAT'):- !.
neClassType('Time',tim,nam,'TIM'):- !.
neClassType('GPE',gpe,nam,'GPE'):- !.

neClassType(N,Class,Type,Tag):- atom(N), atomic_list_concat([Class,Type],'-',N), neClassType(_,Class,_,Tag), !.
neClassType(_,nam,nam,'UNK'):- !.
neClassType(_,_,_,'UNK').

