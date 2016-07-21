
:- module(ontology,[isa/2,isnota/2]).

/* -------------------------------------------------------------------------
   IS-A links
------------------------------------------------------------------------- */

isa(event,thing).
isa(event,neuter).

isa(time,thing).
isa(time,neuter).

isa(entity,thing).

isa(abstraction,thing).
isa(abstraction,neuter).

isa(organisation,abstraction).

isa(object,entity).
isa(object,neuter).

isa(organism,entity).

isa(animal,organism).
isa(person,organism).

isa(man,person).
isa(woman,person).

isa(woman,female).
isa(man,male).

/* -------------------------------------------------------------------------
   IS-NOT-A links
------------------------------------------------------------------------- */

isnota(male,female).
isnota(neuter,female).
isnota(neuter,male).

isnota(abstraction,time).
isnota(abstraction,event).
isnota(abstraction,entity).
isnota(entity,event).
isnota(time,event).
isnota(entity,time).

isnota(object,organism).

isnota(animal,person).

isnota(single,plural).
isnota(single,event).
isnota(plural,event).


/* -------------------------------------------------------------------------

   Simple sketch of the ontology:


                              /     /         \       \
                         entity   event(n)  time(n)   abstraction(n)
                         /    \                     /        \
                   object(n)  organism       organisation     
                              /     \
                         animal    person
                                   /    \
                              man(m)    woman(f)

--------------------------------------------------------------------------- */


