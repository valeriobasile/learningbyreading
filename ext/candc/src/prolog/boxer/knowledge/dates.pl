
:- module(dates,[year/2,   % +Number, -YearID
                 month/2,  % +Month,  -MonthID
                 dofm/2,   % +Day,    -DayID
                 decade/2, % +Decade, -DecadeID
                 yearBC/1,
                 yearAD/1]).


/*========================================================================
   Months
========================================================================*/

month('january','01').
month('jan','01').
month('jan.','01').
month('february','02').
month('february','02').
month('feb','02').
month('feb.','02').
month('march','03').
month('mar','03').
month('mar.','03').
month('april','04').
month('apr','04').
month('apr','04').
month('apr.','04').
month('may','05').
month('june','06').
month('july','07').
month('august','08').
month('aug','08').
month('aug.','08').
month('september','09').
month('sept','09').
month('sep','09').
month('sep.','09').
month('sept.','09').
month('october','10').
month('oct','10').
month('oct.','10').
month('november','11').
month('nov','11').
month('nov.','11').
month('december','12').
month('dec','12').
month('dec.','12').


/*========================================================================
   Format Years BD and AD
========================================================================*/

yearBC(bc).
yearBC(bce).
yearBC('b.c.').
yearBC('b.c.e.').

yearAD(ad).
yearAD(ce).
yearAD('a.d.').
yearAD('c.e.').


/*========================================================================
   Format Years
========================================================================*/

year(Atom,Year):-
   atom_codes(Atom,[C1,C2,C3,C4]),
   C1 > 48, C1 < 51,
   C2 > 47, C2 < 58,
   C3 > 47, C3 < 58,
   C4 > 47, C4 < 58, !,
   atom_codes(Year,[C1,C2,C3,C4]).

year(Atom,Year):-
   atom_codes(Atom,[39,C3,C4]),
   C3 > 47, C3 < 58,
   C4 > 47, C4 < 58, !,
   atom_codes(Year,[49,57,C3,C4]).


/*========================================================================
   Last year
========================================================================*/

last_year(Year,Last):- 
   atom_codes(Year,[Y1,Y2,Y3,Y4]),
   Y1 > 48, Y1 < 51,
   Y2 > 47, Y2 < 58,
   Y3 > 47, Y3 < 58,
   Y4 > 47, Y4 < 58, !,
   number_codes(NumberYear,[Y1,Y2,Y3,Y4]),
   NumberLast is NumberYear - 1,
   number_codes(NumberLast,Codes),
   atom_codes(Last,Codes).

last_year(X,X).


/*========================================================================
   Centuries
========================================================================*/

century('1st',     '00XX').
century('first',   '00XX').
century('2nd',     '01XX').
century('second',  '01XX').
century('3rd',     '02XX').
century('third',   '02XX').
century('4th','     03XX').
century('fourth',  '03XX').
century('5th',     '04XX').
century('fifth',   '04XX').
century('6th',     '05XX').
century('sixth',   '05XX').
century('7th',     '06XX').
century('seventh', '06XX').
century('8th',     '07XX').
century('eigth',   '07XX').
century('9th',     '08XX').
century('ninth',   '08XX').
century('10th',    '09XX').
century('tenth',   '09XX').
century('11th',    '10XX').
century('12th',    '11XX').
century('13th',    '12XX').
century('14th',    '13XX').
century('15th',    '14XX').
century('16th',    '15XX').
century('17th',    '16XX').
century('18th',    '17XX').
century('19th',    '18XX').
century('20th',    '19XX').
century('21th',    '20XX').


/*========================================================================
   Decades
========================================================================*/

decade(Date,Decade):-
   atom(Date), 
   atom_codes(Date,[39,C,48,115]), !,         %%%  '90s -> 199X
   atom_codes(Decade,[49,57,C,88]).

decade(Date,Decade):-
   atom(Date), 
   atom_codes(Date,[A,B,C,48,115]), !,        %%%  1990s -> 199X
   atom_codes(Decade,[A,B,C,88]).

decade(Date,Decade):-
   atom(Date), 
   atom_codes(Date,[A,B,C,48]), !,            %%%  1990 -> 199X
   atom_codes(Decade,[A,B,C,88]).


/*========================================================================
   Days
========================================================================*/

dofm(Day,DID):- number(Day), !, day(Day,DID).
dofm(Day,DID):- atom(Day), day(_,Day), !, DID = Day.
dofm(Day,DID):- atom(Day), day(Day,DID), !.

day(1,'01').  day('1st','01').   day('1','01').
day(2,'02').  day('2nd','02').   day('2','02').
day(3,'03').  day('3rd','03').   day('3','03').
day(4,'04').  day('4th','04').   day('4','04').
day(5,'05').  day('5th','05').   day('5','05').
day(6,'06').  day('6th','06').   day('6','06').
day(7,'07').  day('7th','07').   day('7','07').
day(8,'08').  day('8th','08').   day('8','08').
day(9,'09').  day('9th','09').   day('9','09').
day(10,'10'). day('10th','10').
day(11,'11'). day('11th','11').
day(12,'12'). day('12th','12').
day(13,'13'). day('13th','13').
day(14,'14'). day('14th','14').
day(15,'15'). day('15th','15').
day(16,'16'). day('16th','16').
day(17,'17'). day('17th','17').
day(18,'18'). day('18th','18').
day(19,'19'). day('19th','19').
day(20,'20'). day('20th','20').
day(21,'21'). day('21st','21').
day(22,'22'). day('22nd','22').
day(23,'23'). day('23rd','23').
day(24,'24'). day('24th','24').
day(25,'25'). day('25th','25').
day(26,'26'). day('26th','26').
day(27,'27'). day('27th','27').
day(28,'28'). day('28th','28').
day(29,'29'). day('29th','29').
day(30,'30'). day('30th','30').
day(31,'31'). day('31st','31').

