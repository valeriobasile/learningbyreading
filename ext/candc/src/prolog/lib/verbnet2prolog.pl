
:- use_module(library(sgml)).
:- use_module(library(lists),[member/2,append/3,reverse/2]).

test(F):- 
   load_xml_file(F,T), 
%  pretty_print(T,0),
   elements(T,['VNCLASS'],f(X,C)),   
   value(X,'ID',ID),
   check(ID,C).

verbnet2prolog(F):- 
   load_xml_file(F,T), 
%  pretty_print(T,0),
   elements(T,['VNCLASS'],f(X,C)),   
   value(X,'ID',ID),
   members(C,F,[],ID).


/* ----------------------------------------------------------------------
   Pretty Printing XML
---------------------------------------------------------------------- */ 

pretty_print([],_).

pretty_print([element(A,B,C)|L],Tab):- !,
   tab(Tab), write(A), write(' '), write(B), nl,
   NewTab is Tab+3, 
   pretty_print(C,NewTab),
   pretty_print(L,Tab).

pretty_print([E|L],Tab):-  
   tab(Tab), write(unknown:E),nl,
   pretty_print(L,Tab).


/* ----------------------------------------------------------------------
   Checking Syntax
---------------------------------------------------------------------- */ 

check(ID,X):-
   elements(X,['FRAMES','FRAME'],f(_,Frame)),   
   elements(Frame,['DESCRIPTION'],f(De,_)),   
   value(De,primary,Primary),
   write(ID), write(': '), write(Primary),nl,
   fail.

check(_,X):-
   elements(X,['SUBCLASSES','VNSUBCLASS'],f(Y,Sub)), 
   value(Y,'ID',ID),
   check(ID,Sub),
   fail.

check(_,_).

         
/* ----------------------------------------------------------------------
   Processing all members of a VerbNet class
---------------------------------------------------------------------- */ 

members(X,File,SuperFrames,XID):- 
   findall(Name,(elements(X,['MEMBERS','MEMBER'],f(Member,_)),value(Member,name,Name)),Names),
   findall(Frame,(elements(X,['FRAMES','FRAME'],f(_,Frame))),SubFrames),
   append(SuperFrames,SubFrames,Frames),
   frameMember(Frames,Names,XID,File),
   findall(Sub,(elements(X,['SUBCLASSES','VNSUBCLASS'],f(Y,Sub)),
                value(Y,'ID',YID),
                members(Sub,File,Frames,YID)),_).
                 

/* ----------------------------------------------------------------------
   Process a member/frame pair
---------------------------------------------------------------------- */ 

frameMember([],_,_,_):- !.

frameMember([F|L],Names,ID,File):-
   pairMemberFrame(Names,F,ID,File),
   frameMember(L,Names,ID,File).

pairMemberFrame([],_,_,_).

pairMemberFrame([Name|L],Frame,ID,File):-
   elements(Frame,['DESCRIPTION'],f(De,_)),   
   value(De,primary,Pr),
   elements(Frame,['SYNTAX'],f(_,Syntax)),  
   format('verbnet(~q, ~q, ',[Name,Pr]),
   subcatpat(Syntax,[],SubCatPat), 
   format('~q, ',[SubCatPat]), 
   subcat(Syntax,[],SubCat),
   atom_chars(ID,IDChars),
   formatID(IDChars,[_,_|FID]),
   format('~q,~q). %%% ~p (~p)~n',[SubCat,FID,ID,File]), !,
   pairMemberFrame(L,Frame,ID,File).


/* ----------------------------------------------------------------------
   Format VerbNet ID
---------------------------------------------------------------------- */ 

formatID(Chars,[Pre,Sep1|L]):-
   Seps = ['-','.'], member(Sep1,Seps),
   append(PreChars,[Sep1|RestChars],Chars), 
   \+ ( member(Sep2,Seps), member(Sep2,PreChars) ), !,
   formatNumber(PreChars,Pre),
   formatID(RestChars,L).

formatID(Chars,[ID]):-
   formatNumber(Chars,ID).

formatNumber(Chars,Num):-
   Chars = [First|_], 
   member(First,['0','1','2','3','4','5','6','7','8','9']), !, 
   number_chars(Num,Chars).

formatNumber(Chars,Atom):-
   atom_chars(Atom,Chars).

/* ----------------------------------------------------------------------
   Printing the subcat frame
---------------------------------------------------------------------- */ 

subcat([],Acc1,Acc2):- postproc(Acc1,[],Acc2).
subcat([E|L],Acc1,Acc3):- cat(E,Acc1,Acc2), subcat(L,Acc2,Acc3).

subcatpat([],Acc1,Acc2):- postproc(Acc1,[],Acc2).
subcatpat([E|L],Acc1,Acc3):- catpat(E,Acc1,Acc2), subcatpat(L,Acc2,Acc3).


/* ----------------------------------------------------------------------
   Post Processing (reverse + rewriting)
---------------------------------------------------------------------- */ 

postproc([],L,L).
postproc([np,pp|L1],Acc,L2):- !, postproc(L1,[pp|Acc],L2).
postproc([np:V,pp|L1],Acc,L2):- !, postproc(L1,[pp:V|Acc],L2).
postproc([s,pp|L1],Acc,L2):- !, postproc(L1,[s|Acc],L2).
postproc([s:V,pp|L1],Acc,L2):- !, postproc(L1,[s:V|Acc],L2).
postproc([X|L1],Acc,L2):- postproc(L1,[X|Acc],L2).

/* ----------------------------------------------------------------------
   Syntactic Restrictions
---------------------------------------------------------------------- */ 

restr(Restr,Type):- 
  Restr = [element('SYNRESTRS',[],L)],
  member(element('SYNRESTR',['Value'='+',type=Type],[]),L), !.

s_restr(that_comp).
s_restr(for_comp).
s_restr(wh_comp).

% s_restr(poss_ing). % not sentence!
s_restr(acc_ing).
s_restr(oc_ing).
s_restr(ac_ing).
s_restr(be_sc_ing).
s_restr(np_omit_ing).  % ???
s_restr(np_ppart).     % ??? 
s_restr(np_p_ing).     % ???
s_restr(np_ing).       % ???

s_restr(how_extract).
s_restr(what_extract).

s_restr(wh_inf).
s_restr(what_inf).
s_restr(wheth_inf).
s_restr(oc_bare_inf).
s_restr(oc_to_inf).
s_restr(ac_to_inf).
s_restr(sc_to_inf).
s_restr(np_to_inf).
s_restr(vc_to_inf).
s_restr(rs_to_inf). % very rare -- bug?
s_restr(to_inf_rs). % very rare -- bug?


/* ----------------------------------------------------------------------
   Printing a category
---------------------------------------------------------------------- */ 

cat(element('NP', [value=Value], R),A,[s:Value|A]):- s_restr(S), restr(R,S), !.
cat(element('NP', [value=Value], _),A,[np:Value|A]):- !.
cat(element('PREP', [], _),A,[pp|A]):- !.
cat(element('PREP', [value=Value], _),A,[prep:Value|A]):- !.
cat(element('LEX', [value='[+be]'], _),A,[lex:be|A]):- !. 
cat(element('LEX', [value='it[+be]'], _),A,[lex:be,lex:it|A]):- !.
cat(element('LEX', [value=at], _),A,[prep:at|A]):- !.
cat(element('LEX', [value=of], _),A,[prep:of|A]):- !.
cat(element('LEX', [value=Value], _),A,[lex:Value|A]):- !.
cat(element('VERB',[],[]),A,[v|A]):- !.
cat(element('ADJ',[],[]),A,[adj|A]):- !.
cat(element('ADV',[],[]),A,[adv|A]):- !.
cat(U,A,[unk:U|A]):- !.

catpat(element('NP',_,R),A,[s|A]):- s_restr(S), restr(R,S), !.
catpat(element('NP',_,_),A,[np|A]):- !.
catpat(element('PREP', [], _),A,[pp|A]):- !.
catpat(element('PREP', [value=_], _),A,[prep|A]):- !.
catpat(element('LEX',[value=at],_),A,[prep|A]):- !.
catpat(element('LEX',[value=of],_),A,[prep|A]):- !.
catpat(element('LEX',_,_),A,[lex|A]):- !.
catpat(element('VERB',_,_),A,[v|A]):- !.
catpat(element('ADJ',_,_),A,[adj|A]):- !.
catpat(element('ADV',_,_),A,[adv|A]):- !.
catpat(_,A,[unk|A]):- !.


/* ----------------------------------------------------------------------
   Processing elements of the XML tree
---------------------------------------------------------------------- */ 

elements([element(X,F,L)|_],[X],f(F,L)).
elements([element(X,_,L)|_],[X|R],A):- elements(L,R,A).
elements([_|L],X,A):- elements(L,X,A).


/* ----------------------------------------------------------------------
   Accessing a value
---------------------------------------------------------------------- */ 

value([Name=Value|_],Name,Value):- !.
value([_|L],Name,Value):- value(L,Name,Value).


/* ----------------------------------------------------------------------
   VerbNet Directory
---------------------------------------------------------------------- */ 

verbnet_dir('ext/VerbNet/').


/* ----------------------------------------------------------------------
   Processing all XML files
---------------------------------------------------------------------- */ 

process([]).
process([File|L]):-
%   test(File),
   verbnet2prolog(File),
   process(L).


/* ----------------------------------------------------------------------
   Start Predicate
---------------------------------------------------------------------- */ 

run:- 
   verbnet_dir(Dir), 
   exists_directory(Dir),
   WildCard = '*.xml',
%  WildCard = 'put-9.1.xml',
   atom_concat(Dir,WildCard,Expand),
   expand_file_name(Expand,Files),
   write(':- dynamic verbnet/5.'), nl,
   process(Files), 
   halt.

:- run.
