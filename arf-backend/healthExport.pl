/*
  This module defines functions for exporting health information as CSV files.
*/
:- module(healthExport, []).

:- use_module(base).
:- use_module(aux).
:- use_module(health).

/*
 + ID, - weight_record
*/
weightRecord(ID, weight_record(ID, Weight, Units, Precision, Time, Year, DOY)) :-
  base:measurement(ID, AttributeID, Units, Weight, Precision), !,
  base:attribute(AttributeID, SubjectID),
  base:weight(AttributeID),
  base:entity(SubjectID, "me"),
  base:event(ID, Timestamp),
  stamp_date_time(Timestamp, Date, 'UTC'),
  format_time(atom(Time), '%F %T', Date, posix),
  format_time(atom(Year), '%Y', Date, posix),
  format_time(atom(DOY), '%j', Date, posix).

/*
  - WeightRecords

  Returns a list of all of my personal weight measurements.
*/
weightRecords(WeightRecords) :-
  findall(ID, base:measurement(ID, _, _, _, _), IDs),
  convlist(weightRecord, IDs, WeightRecords).

/*
  + Filename

  writes my personal weight measurements to the given filename as a CSV. 
*/
writeWeightRecords(Filename) :-
  weightRecords(WeightRecords),
  csv_write_file(Filename, [weight_record('Entry ID', 'Weight', 'Units', 'Precision', 'Time', 'Year', 'DOY')|WeightRecords]).
