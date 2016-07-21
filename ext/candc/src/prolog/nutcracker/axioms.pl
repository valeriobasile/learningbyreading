imp(drs([E,X],[pred(E,say,v,0),rel(E,X,theme,0)]),drs([],[eq(X)])).
imp(drs([E,X],[pred(E,tell,v,0),rel(E,X,theme,0)]),drs([],[eq(X)])).
imp(drs([E,X],[pred(E,reveal,v,0),rel(E,X,theme,0)]),drs([],[eq(X)])).
imp(drs([E,X],[pred(E,add,v,0),rel(E,X,theme,0)]),drs([],[eq(X)])).
imp(drs([E,X],[pred(E,report,v,0),rel(E,X,theme,0)]),drs([],[eq(X)])).
imp(drs([E,X],[pred(E,discover,v,0),rel(E,X,theme,0)]),drs([],[eq(X)])).

imp(drs([E,X],[rel(E,X,when,0)]),drs([],[eq(X)])).
imp(drs([E,X],[rel(E,X,because,0)]),drs([],[eq(X)])).
imp(drs([E,X],[rel(E,X,although,0)]),drs([],[eq(X)])).

imp(drs([X,Y],[rel(X,Y,of,0),pred(X,wife,n,0)]),drs([E],[pred(E,marry,v,0),rel(E,Y,patient,0),rel(E,X,to,0)])).
imp(drs([X,Y],[rel(X,Y,of,0),pred(X,wife,n,0)]),drs([E],[pred(E,marry,v,0),rel(E,X,patient,0),rel(E,Y,to,0)])).
imp(drs([X,Y],[rel(X,Y,of,0),pred(X,husband,n,0)]),drs([E],[pred(E,marry,v,0),rel(E,Y,patient,0),rel(E,X,to,0)])).
imp(drs([X,Y],[rel(X,Y,of,0),pred(X,husband,n,0)]),drs([E],[pred(E,marry,v,0),rel(E,X,patient,0),rel(E,Y,to,0)])).
imp(drs([X],[pred(X,widow,n,0)]),drs([],[pred(X,wife,n,0)])).

%imp(drs([X,Y],[eq(X,Y)]),drs([E],[pred(E,be,v,0),rel(E,X,agent,0),rel(E,Y,patient,0)])).

imp(drs([X,Y],[pred(X,somebody,n,1),pred(Y,location,n,1),rel(X,Y,of,0)]),drs([E],[pred(E,live,v,1),rel(E,X,agent,1),rel(E,Y,in,1)])).

imp(drs([X,Y],[rel(X,Y,rel,2)]),drs([],[eq(X,Y)])).  % appositive



