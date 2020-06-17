/*
  This module defines the music practice database.
*/
:- module(practice, []).

:- use_module(base).
:- use_module(library(persistency)).
:- use_module(library(apply)).
:- use_module(library(yall)).

:- persistent
     /*
       Accepts one argument: entity, an entity ID; and asserts that
       the referenced entity represents an exercise.
     */
     exercise(entity:atom),

     /*
       Accepts two arguments: entity, an entity ID; and exercises,
       a list of exercise IDs; and asserts that the referenced
       entity is a routine comprising the referenced exercises.
     */
     routine(entity:atom, exercises:list),

     /*
       Accepts four arguments: event, an event ID; routine, a
       routine ID; duration, a number representing the duration
       of the practice session in minutes; and exercises, a list
       of exercise IDs representing the set of exercises completed
       during the practice session.
     */
     session(event:atom, routine:atom, duration:number, exercises:list).

attachDB(FileName) :-
  db_attach(FileName, []).

exerciseCreate(Name, ID) :-
  base:entityCreate(Name, ID),
  assert_exercise(ID).

routineCreate(Name, Exercises, ID) :-
  base:entityCreate(Name, ID),
  assert_routine(ID, Exercises).

sessionCreate(Routine, Duration, Exercises, ID) :-
  base:eventCreate(ID),
  assert_session(ID, Routine, Duration, Exercises).

exerciseName(Name, ID) :-
  base:entity(ID, Name), exercise(ID).

routineName(Name, ID) :-
  base:entity(ID, Name), routine(ID, _).

exerciseNames(Names, IDs) :-
  maplist([Name, ID]>>(base:entity(ID, Name), practice:exercise(ID)), Names, IDs).
