/* This module defines the Study database. */
:- module(study, []).
:- use_module(library(uuid)).
:- use_module(library(persistency)).

:- persistent
     /*
       Accepts one argument: entityID, and asserts that the
       referenced entity is a study subject.
     */
     subject(entityID:atom),

     /*
       Accepts two arguments: parentID, a subject ID; and subjectID;
       and asserts that the referenced subject is a sub topic of the
       subject referenced by parentID.
     */
     subtopic(parentID:atom, subjectID:atom),

     /*
       Accepts two arguments: activityID; and subjectID; and asserts
       that the referenced activity represents a study session.
     */
     session(activityID:atom, subjectID:atom).

attachDB(FileName) :- db_attach(FileName, []).

subjectCreate(Name, ID) :-
  base:entityCreate(Name, ID),
  assert_subject(ID).

/*
  Accepts five arguments:

    * +SubjectID, an entry ID that references the subject studied
    * +Timestamp, a timestamp recording when the study session occured
    * +Unit, the unit of time used to measure the duration
    * +Duration, the duration of the study session
    * and +DurationPrec, the precision of the duration measurement

  creates an entry recording an study session; and returns the entry's ID.
*/
sessionCreate(SubjectID, Timestamp, Unit, Duration, DurationPrec, ID) :-
  base:entity(ActorID, "me"), !,
  base:activityCreate(Timestamp, ActorID, Unit, Duration, DurationPrec, ID),
  assert_session(ID, SubjectID).

sessionCreate(SubjectID, Unit, Duration, DurationPrec, ID) :-
  base:entity(ActorID, "me"), !,
  base:activityCreate(ActorID, Unit, Duration, DurationPrec, ID),
  assert_session(ID, SubjectID).
