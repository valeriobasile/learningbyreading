
:- module(noncomp,[noncomp/3]).

:- use_module(library(lists),[member/2,append/3,select/3]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).

/* ========================================================================
   Non-compositional reduction rules
======================================================================== */

noncomp(B1:I1:named(X1,Sym1,Sort,Type),B2:I2:named(X2,Sym2,Sort,Type),K):- 
   option('--mwe',yes),
   X1 == X2, B1 == B2, !,
   atomic_list_concat([Sym1,Sym2],'~',Sym3), 
   append(I1,I2,I3), sort(I3,I),
   K = B1:I:named(X1,Sym3,Sort,Type).

noncomp(B1:I1:named(X1,Sym1,Sort,Type),B2:I2:named(X2,Sym2,_,_),K):- 
   option('--mwe',all),
   X1 == X2, B1 == B2, !,
   atomic_list_concat([Sym1,Sym2],'~',Sym3), 
   append(I1,I2,I3), sort(I3,I),
   K = B1:I:named(X1,Sym3,Sort,Type).

noncomp(B1:I1:timex(X1,Date1),B2:I2:timex(X2,Date2),K):- 
   X1 == X2, B1 == B2, !,
   concat_dates(Date1,Date2,Date), 
   append(I1,I2,I3), sort(I3,I),
   K = B1:I:timex(X1,Date).

noncomp(B1:I1:card(X1,Num1,Type),B2:I2:card(X2,Num2,Type),K):- 
   X1 == X2, B1 == B2, number(Num1), number(Num2), Num2 < 10, !,
   Num is Num1 + Num2,
   append(I1,I2,I3), sort(I3,I),
   K = B1:I:card(X1,Num,Type).

noncomp(B1:I1:card(X1,Num1,Type),B2:I2:card(X2,Num2,Type),K):- 
   X1 == X2, B1 == B2, number(Num1), number(Num2), !,
   Num is round(Num1 * Num2),
   append(I1,I2,I3), sort(I3,I),
   K = B1:I:card(X1,Num,Type).


/*========================================================================
   Concatenate Dates
========================================================================*/

concat_dates(date([]:'+', []:'XXXX', Month,  Day),
             date([]:'+',  Year,   []:'XX', Day),
             date([]:'+',  Year,    Month,  Day)).

concat_dates(date([]:'+',  Year,   []:'XX', Day),
             date([]:'+', []:'XXXX', Month,  Day), 
             date([]:'+',  Year,    Month,  Day)).

concat_dates(date([]:'+', Year, []:'XX', Day),
             date([]:'+', Year,  Month, []:'XX'), 
             date([]:'+', Year,  Month,  Day)).

concat_dates(date([]:'+', Year,  Month, []:'XX'), 
             date([]:'+', Year, []:'XX', Day),
             date([]:'+', Year,  Month,  Day)).

concat_dates(date([]:'+', []:'XXXX', []:'XX', Day),
             date([]:'+', Year,  Month, []:'XX'), 
             date([]:'+', Year,  Month,  Day)).

concat_dates(date([]:'+', Year, []:'XX', []:'XX'), 
             date([]:'+', []:'XXXX', Month, Day),
             date([]:'+', Year, Month, Day) ).

concat_dates(date([]:'+', []:'XXXX', Month, Day),
             date([]:'+', Year, []:'XX', []:'XX'),
             date([]:'+', Year, Month, Day) ).

concat_dates(date([]:'+', []:'XXXX', Month, []:'XX'),
             date([]:'+', Year, []:'XX', Day),
             date([]:'+', Year, Month, Day) ).


