
:- use_module(library(lists),[max_list/2]).
:- ['working/rte.mod'].

model(T,dabs,I,Gold,V):- mod(_,T,I,Gold,V,_,_,_).
model(T,drel,I,Gold,V):- mod(_,T,I,Gold,_,V,_,_).
model(T,fabs,I,Gold,V):- mod(_,T,I,Gold,_,_,V,_).
model(T,frel,I,Gold,V):- mod(_,T,I,Gold,_,_,_,V).

model(T,aabs,I,Gold,V):- alt(_,T,I,Gold,V,_).
model(T,arel,I,Gold,V):- alt(_,T,I,Gold,_,V).

model(T,wabs,I,Gold,V):- mwn(_,T,I,Gold,V,_).
model(T,wrel,I,Gold,V):- mwn(_,T,I,Gold,_,V).

model(T,dabs,fabs,I,Gold,V1,V2):- mod(_,T,I,Gold,V1,_,V2,_).
model(T,dabs,frel,I,Gold,V1,V2):- mod(_,T,I,Gold,V1,_,_,V2).
model(T,dabs,drel,I,Gold,V1,V2):- mod(_,T,I,Gold,V1,V2,_,_).
model(T,frel,dabs,I,Gold,V1,V2):- mod(_,T,I,Gold,V2,_,_,V1).
model(T,drel,fabs,I,Gold,V1,V2):- mod(_,T,I,Gold,_,V1,V2,_).
model(T,fabs,drel,I,Gold,V1,V2):- mod(_,T,I,Gold,_,V2,V1,_).
model(T,drel,frel,I,Gold,V1,V2):- mod(_,T,I,Gold,_,V1,_,V2).
model(T,frel,drel,I,Gold,V1,V2):- mod(_,T,I,Gold,_,V2,_,V1).

estimate(Task,Fea,Acc):-
   findall(gold(V,Gold,I),model(Task,Fea,I,Gold,V),L),
   est1(L,Fea,0,best(0,0,[]),Acc).

est1(_,F,T1,Best,Acc):- 
   \+ ( model(_,F,_,_,T2) ,T2 > T1), !, 
   Best = best(Tau,Acc,Evidence),
   length(Evidence,Len),
   format('informative if ~p diff >= ~p (accuracy: ~p, n=~p)~n',[F,Tau,Acc,Len]).
%   write(Evidence), nl.

est1(L,F,T1,best(T,Best,IDs),Max):-
   calc1(L,informative,T1,0,0,Acc,NewIDs),
   ( Acc > Best, !, NewBest = best(T1,Acc,NewIDs); NewBest = best(T,Best,IDs) ),
   step(F,T1,T2),
   est1(L,F,T2,NewBest,Max).


estimate(Task,F1,F2,Acc):-
   findall(gold(V1,V2,Gold,I),model(Task,F1,F2,I,Gold,V1,V2),L),
   est2(L,F1,F2,0,0,best(0,0,0,[]),Acc).

est2(_,FD,FF,TauD,TauF,Best,Acc):- 
   \+ (model(_,FD,FF,_,_,DTau,_), DTau > TauD), 
   \+ (model(_,FD,FF,_,_,_,FTau), FTau > TauF), !,
   Best = best(Tau1,Tau2,Acc,_Evidence),
   format('informative if ~p diff >= ~p OR ~p diff >= ~p (accuracy ~p)~n',[FD,Tau1,FF,Tau2,Acc]).
%   write(Evidence), nl.

est2(L,FD,FF,TauD1,TauF1,best(TauD,TauF,Best,IDs),Max):-
   calc2(L,informative,TauD1,TauF1,0,0,Acc,NewIDs),
   ( Acc > Best, !, NewBest = best(TauD1,TauF1,Acc,NewIDs); NewBest = best(TauD,TauF,Best,IDs) ),
   ( 
     model(_,FD,FF,_,_,_,FTau), FTau > TauF1, !,
     step(FF,TauF1,TauF2),
     TauD2 = TauD1
   ;
     step(FD,TauD1,TauD2),
     TauF2 = 0
   ),
   est2(L,FD,FF,TauD2,TauF2,NewBest,Max).


step(dabs,T1,T2):- !, T2 is T1 + 1.
step(fabs,T1,T2):- !, T2 is T1 + 1.
step(wabs,T1,T2):- !, T2 is T1 + 1.
step(aabs,T1,T2):- !, T2 is T1 + 1.

step(drel,T1,T2):- !, T2 is T1 + 0.001.
step(frel,T1,T2):- !, T2 is T1 + 0.001.
step(wrel,T1,T2):- !, T2 is T1 + 0.001.
step(arel,T1,T2):- !, T2 is T1 + 0.001.


calc1([],_,_,C,N,Acc,[]):-
   Acc is C/N.

calc1([gold(Delta,Gold,Id)|L],F,Tau,C,M,Acc,[i:Id|IDs]):- 
   Gold = F, Delta >= Tau, !,
   N is M + 1,
   D is C + 1,
   calc1(L,F,Tau,D,N,Acc,IDs).

calc1([gold(Delta,Gold,Id)|L],F,Tau,C,M,Acc,[e:Id|IDs]):- 
   \+ Gold = F, Delta < Tau, !,
   N is M + 1,
   D is C + 1,
   calc1(L,F,Tau,D,N,Acc,IDs).

calc1([_|L],F,Tau,C,M,Acc,IDs):- 
   N is M + 1,
   calc1(L,F,Tau,C,N,Acc,IDs).


calc2([],_,_,_,C,N,Acc,[]):-
   Acc is C/N.

calc2([gold(DeltaD,_DeltaF,Gold,Id)|L],F,TauD,TauF,C,M,Acc,[i:Id|IDs]):- 
   Gold = F, DeltaD >= TauD, !,
   N is M + 1,
   D is C + 1,
   calc2(L,F,TauD,TauF,D,N,Acc,IDs).

calc2([gold(DeltaD,DeltaF,Gold,Id)|L],F,TauD,TauF,C,M,Acc,[i:Id|IDs]):- 
   Gold = F, DeltaD < TauD, DeltaF >= TauF, !,
   N is M + 1,
   D is C + 1,
   calc2(L,F,TauD,TauF,D,N,Acc,IDs).

calc2([gold(DeltaD,DeltaF,Gold,Id)|L],F,TauD,TauF,C,M,Acc,[e:Id|IDs]):- 
   \+ Gold = F, DeltaD < TauD, DeltaF < TauF, !,
   N is M + 1,
   D is C + 1,
   calc2(L,F,TauD,TauF,D,N,Acc,IDs).

calc2([_|L],F,TauD,TauF,C,M,Acc,IDs):- 
   N is M + 1,
   calc2(L,F,TauD,TauF,C,N,Acc,IDs).
   
   
estimate(T,Max):-
   nl,write('---TASK---':T), nl,
   estimate(T,dabs,Accdabs),
   estimate(T,aabs,Accaabs),
   estimate(T,dabs,fabs,Accdabsfabs),
   estimate(T,dabs,drel,Accdabsdrel),
   estimate(T,dabs,frel,Accdabsfrel), nl,
   estimate(T,drel,Accdrel),
   estimate(T,arel,Accarel),
   estimate(T,drel,fabs,Accdrelfabs),
   estimate(T,drel,frel,Accdrelfrel), nl,
   estimate(T,fabs,Accfabs),
   estimate(T,frel,Accfrel), nl,
   estimate(T,wabs,Accwabs),
   estimate(T,wrel,Accwrel),
   max_list([Accdabs,Accaabs,Accdabsfabs,Accdabsfrel,Accdabsdrel,
             Accdrel,Accarel,Accdrelfabs,Accdrelfrel,
             Accfabs,Accfrel,Accwabs,Accwrel],Max).

:- estimate('IE',Max1),
   write(best:Max1),nl,
   estimate('IR',Max2), 
   write(best:Max2),nl,
   estimate('QA',Max3), 
   write(best:Max3),nl,
   estimate('SUM',Max4), 
   write(best:Max4),nl,
   estimate(_,Max), 
   write(best:Max),nl,
   Task is ((Max1+Max2+Max3+Max4)/4),
   write(task:Task),nl.
   
:- halt.


