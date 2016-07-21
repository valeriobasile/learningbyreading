:- module(roles,[role/3,old2new/2]).

/* -----------------------------------------------------------
   role(?Role,?IsaRole,?Comment)

   Based on VerbNet and LIRICS.
   See: Bonial et al. 2011, Sowa 2000. 
   Purpose, Reason
----------------------------------------------------------- */


role('Actor',       'Participant', 'participant that is an instigator').
role('Undergoer',   'Participant', 'participant that is not an instigator').
role('Time',        'Participant', 'participant that indicates an instance or interval of time').
role('Place',       'Participant', 'participant that represents the place in which an entity exists'). % state->place
role('Abstract',    'Participant', 'participant that represents a goal, property or manner').          % added

role('Agent',       'Actor',       'intentional actor').
role('Cause',       'Actor',       'unintentional actor (animate or inanimate)').

role('Patient',     'Undergoer',   'undergoer that experiences a change of state, location or condition; exists independently of the event').
role('Instrument',  'Undergoer',   'undergoer that is manipulated by an agent; exists independently of the event').
role('Beneficiary', 'Undergoer',   'undergoer that is potentially advantaged or disadvantaged by the event').
role('Theme',       'Undergoer',   'undergoer that is central to an event; not structurally changed by the event').

role('Topic',       'Theme',       'theme with propositional information content'). % definition changed from LIRICS
role('Pivot',       'Theme',       'theme that is more central than another theme in an event').

role('Start',       'Time', 'time that indicates when an events begins or a state becomes true').
role('Finish',      'Time', 'time that indicates when an events ends or a state becomes false').
role('Duration',    'Time', 'length or extend of time').
role('Frequency',   'Time', 'number of occurences of an event within a given time span').

role('Location',    'Place', 'concrete place').
role('Source',      'Place', 'concrete or abstract place that is starting point of action').
role('Destination', 'Place', 'place that is an end point of an action').
role('Path',        'Place', '').
role('Value',       'Place', 'place along a formal scale').

role('Goal',        'Abstract',  'purpose of an (intentional) action').
role('Manner',      'Abstract',  'the particular way an event unfolds').
role('Attribute',   'Abstract',  'property of an entity').

role('Extent',      'Value', 'value indicating the amount of measurable change to a participant').
role('Asset',       'Value', 'value that is a concrete object').

role('Recipient',   'Destination', 'animate destination').
role('Experiencer', 'Patient',     'patient that is aware in perception events').
role('Result',      'Goal',        'goal that comes into existence through the event').
role('Stimulus',    'Cause',       'cause in perception event that elicits emotional or psychological response').


/* -----------------------------------------------------------
   Conversion from old (VerbNet) to new names
----------------------------------------------------------- */

old2new('Co-Agent','Agent'):- !.
old2new('Co-Theme','Theme'):- !.
old2new('Co-Patient','Patient'):- !.
old2new('Initial_Time','Start'):- !.
old2new('Final_Time','Finish'):- !.
old2new('Initial_Location','Source'):- !.
old2new('Final_Location','Destination'):- !.
old2new('Result','Goal'):- !.
old2new('Proposition','Topic'):- !.
old2new('Trajectory','Path'):- !.
old2new('Product','Result'):- !.
old2new('Material','Source'):- !.
old2new('Predicate','Attribute'):- !.
old2new('Reflexive','Theme'):- !.
old2new(R,R).
