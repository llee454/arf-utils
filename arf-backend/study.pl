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
     studySession(activityID:atom, subjectID:atom).

attachDB(FileName) :- db_attach(FileName, []).

subjectCreate(Name, ID) :-
  base:entityCreate(Name, ID),
  assert_subject(ID).

studySession(SubjectID, Unit, Duration, ID) :-
  base:entity(ActorID, "me"), !,
  base:activityCreate(ActorID, Unit, Duration, ID),
  assert_studySession(ID, SubjectID).
