/* This module defines the Health database. */
:- module(health, []).
:- use_module(library(uuid)).
:- use_module(library(persistency)).

:- persistent
     waistCircumference(attributeID:atom),
     bicepCircumference(attributeID:atom),
     bloodPressureSystolic(attributeID:atom),
     bloodPressureDiastolic(attributeID:atom),
     pulse(attributeID:atom).

attachDB(FileName) :- db_attach(FileName, []).

weightCreate() :-
  base:entity(SubjectID, "me"), 
  base:attributeCreate(SubjectID, AttributeID),
  base:assert_weight(AttributeID).

waistCircumferenceCreate() :-
  base:entity(SubjectID, "me"),
  base:attributeCreate(SubjectID, AttributeID),
  assert_waistCircumference(AttributeID).

bicepCircumferenceCreate() :-
  base:entity(SubjectID, "me"),
  base:attributeCreate(SubjectID, AttributeID),
  assert_bicepCircumference(AttributeID).

bloodPressureSystolicCreate() :-
  base:entity(SubjectID, "me"),
  base:attributeCreate(SubjectID, AttributeID),
  assert_bloodPressureSystolic(AttributeID).

bloodPressureDiastolicCreate() :-
  base:entity(SubjectID, "me"),
  base:attributeCreate(SubjectID, AttributeID),
  assert_bloodPressureDiastolic(AttributeID).

pulseCreate() :-
  base:entity(SubjectID, "me"),
  base:attributeCreate(SubjectID, AttributeID),
  assert_pulse(AttributeID).

weightMeasurementCreate(Value, Precision, ID) :-
  base:weight(AttributeID),
  base:attribute(AttributeID, SubjectID),
  base:entity(SubjectID, "me"), !,
  base:measurementCreate(SubjectID, AttributeID, lbs, Value, Precision, ID).

waistCircumferenceMeasurementCreate(Value, Precision, ID) :-
  base:entity(SubjectID, "me"), !,
  waistCircumference(AttributeID), !,
  base:measurementCreate(SubjectID, AttributeID, in, Value, Precision, ID). 

bicepCircumferenceMeasurementCreate(Value, Precision, ID) :-
  base:entity(SubjectID, "me"), !,
  bicepCircumference(AttributeID), !,
  base:measurementCreate(SubjectID, AttributeID, in, Value, Precision, ID). 

bloodPressureSystolicMeasurementCreate(Value, ID) :-
  base:entity(SubjectID, "me"), !,
  bloodPressureSystolic(AttributeID), !,
  base:measurementCreate(SubjectID, AttributeID, mmHg, Value, ID). 

bloodPressureDiastolicMeasurementCreate(Value, ID) :-
  base:entity(SubjectID, "me"), !,
  bloodPressureDiastolic(AttributeID), !,
  base:measurementCreate(SubjectID, AttributeID, mmHg, Value, ID). 

pulseMeasurementCreate(Value, ID) :-
  base:entity(SubjectID, "me"), !,
  pulse(AttributeID), !,
  base:measurementCreate(SubjectID, AttributeID, bpm, Value, ID). 
