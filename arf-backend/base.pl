/*
  This module defines the Base Ontology database - i.e. the
  fundamental ontological entities used by the rest of the ARF
  database system.
*/
:- module(base, []).
:- use_module(aux).
:- use_module(library(uuid)).
:- use_module(library(persistency)).

/*
  Note: see the SWI Prolog documentation for the must_be predicate
  to learn more about the type signatures used below.
*/
:- persistent
     /*
       Accepts three arguments: id, a unique ID; version, a number
       that indicates the version that this entry (previous
       versions are retained) is used under; and timestamp, a
       float that represents the time at which the entry was entered.
     */
     entry(id:atom, version:nonneg, timestamp:float),

     /*
       Accepts two arguments: id, a unique ID generated by uuid;
       and name; and asserts that there exists an entity named name.
     */
     entity(entry:atom, name:string),

     /*
       Accepts two arguments: id, a unique ID generated by uuid;
       and timestamp, a timestamp generated using get_time, and
       asserts that an event occured at timestamp.
     */
     event(entry:atom, timestamp:float),

     /*
       Accepts three arguments: id, a unique ID generated by uuid;
       of, an entry ID; and value, another entry ID; and asserts
       that the entry referenced by of has value as an attribute.
     */
     attribute(entry:atom, subject:atom),

     /*
       Accepts two arguments: event, an event ID; and by, an entity
       ID; and asserts that the entity referenced by by performed
       an action described by the event referenced by event.
     */
     action(event:atom, actor:atom),

     /*
       Accepts one argument: actionID, an actionID; and asserts
       that the referenced action represents an activity (an action
       spanning some duration).
     */
     activity(actionID:atom),

     /*
       Accepts five arguments: action, an action ID; of, an entity
       ID; unit, the measurement units; value, the measurement
       value; and precision, the precision of the measurement.
     */
     measurement(action:atom, of:atom, unit:atom, value:any, precision:any),

     /*
       Accepts one argument: attributeID, an attribute ID; and
       asserts that the referenced attribute represents a duration.
     */
     duration(attributeID:atom),

     /*
       Accepts one argument: attributeID, an attribute ID; and
       asserts that the referenced attribute is a weight.
     */
     weight(attributeID:atom),

     /*
       Accepts one argument: attributeID, an attribute ID; and
       asserts that the referenced attribute is a circumference.
     */
     circumference(attributeID:atom).

/*
  Accepts one argument: FileName, a string that represents a file
  name; and opens the referenced file as a Base Ontology database.
*/
attachBaseDB(FileName) :- db_attach(FileName, []).

/*
  Creates an entry and returns the entry's ID as -ID.
*/
entryCreate(ID) :-
  uuid(ID),
  get_time(Timestamp),
  assert_entry(ID, 0, Timestamp).

/*
  Accepts one argument: +Name, a string; creates an entity that
  has the given name; and returns -ID, the ID of the created entity.
*/
entityCreate(Name, ID) :-
  entryCreate(ID),
  assert_entity(ID, Name).

/*
  Creates an event and sets its timestamp to the moment when this
  function executed; and returns -ID, the event's ID.
*/
eventCreate(ID) :-
  entryCreate(ID),
  get_time(Timestamp),
  assert_event(ID, Timestamp).

/*
  Accepts one argument: +Timestamp, a timestamp; creates an event
  with the given timestamp; and returns -ID, the ID of the created
  event.
*/
eventCreate(Timestamp, ID) :-
  entryCreate(ID),
  assert_event(ID, Timestamp).

/*
  Accepts one argument: +Event, an event; and returns the date at
  which the event occured as a -DateTime.
*/
eventDateTime(EventID, DateTime) :-
  event(EventID, Timestamp),
  stamp_date_time(Timestamp, DateTime, 0).

/*
  A predicate that accepts four arguments: +EventID, an entity ID; and
  ?Year, ?Month, and ?Day, integers; and succeeds iff the event
  has occured on the given date.
*/
eventDate(EventID, Year, Month, Day) :-
  event(EventID, Timestamp),
  stamp_date_time(Timestamp, DateTime, 0),
  date_time_value(year, DateTime, Year),
  date_time_value(month, DateTime, Month),
  date_time_value(day, DateTime, Day).

eventCompareTimestamp(Result, EventID0, EventID1) :-
  event(EventID0, Timestamp0),
  event(EventID1, Timestamp1),
  compare(Result, Timestamp0, Timestamp1).

/*
  Returns a list of event ids sorted by event timestamp.
*/
eventsSort(SortedEventIDs) :-
  findall(EventID, event(EventID, _), EventIDs),
  predsort(eventCompareTimestamp, EventIDs, SortedEventIDs).

/*
  Accepts two arguments: +SubjectID and +ID, entity IDs; and creates
  an attribute with ID for the entity referenced by SubjectID.
*/
attributeCreate(SubjectID, ID) :-
  entryCreate(ID),
  assert_attribute(ID, SubjectID).

/*
  Accepts two arguments: +ActorID and +EventID, and creates an
  event with the given ID and attributes the action to the actor.
*/
actionCreate(ActorID, EventID) :-
  eventCreate(EventID),
  assert_action(EventID, ActorID).

/*
  Accepts one argument: +Action, an action; and returns -EventID,
  the event ID of the given action.
*/
actionEventID(Action, EventID) :- Action = action(EventID, _).

/*
  Accepts one argument: +Action, an action; and returns -ActorID,
  the actor ID of the given action.
*/
actionActorID(Action, ActorID) :- Action = action(_, ActorID).

/*
  Accepts five arguments: +SourceID and +ofID, entity IDs; +Unit, an
  atom such as bpm, min, etc; and +Value; and creates a measurement
  with these properties and returns the ID of the created measurement
  named -ID.
*/
measurementCreate(SourceID, OfID, Unit, Value, Precision, ID) :-
  actionCreate(SourceID, ID),
  assert_measurement(ID, OfID, Unit, Value, Precision).

durationCreate(SourceID, EventID, Unit, Value, ID) :-
  attributeCreate(EventID, ID),
  assert_duration(ID),
  measurementCreate(SourceID, ID, Unit, Value, _).

activityCreate(ActorID, Unit, Duration, ID) :-
  base:entity(ActorID, "me"), !,
  actionCreate(ActorID, ID),
  assert_activity(ID),
  durationCreate(ActorID, ID, Unit, Duration, _).

/*
  Accepts one argument; action, an action term; and returns the
  time at which the action occured/began.
*/
actionTimestamp(action(EventID, _), Timestamp) :- event(EventID, Timestamp).
