/*
  This module defines functions for exporting information about past
  meals.
*/
:- module(nutritionExport, []).

:- use_module(base).
:- use_module(aux).
:- use_module(nutrition).

/*
  +ID, -MealRecord
*/
mealRecord(ID, meal_record(ID, Cals, Time, Year, DOY, Hour)) :-
  base:event(ID, Timestamp),
  stamp_date_time(Timestamp, Date, 'UTC'),
  format_time(atom(Time), '%F %T', Date, posix),
  format_time(atom(Year), '%Y', Date, posix),
  format_time(atom(DOY), '%j', Date, posix),
  format_time(atom(Hour), '%H', Date),
  base:attribute(AttribID, ID),
  nutrition:calories(AttribID, Cals).

/*
  - MealRecords
*/
mealRecords(MealRecords) :-
  nutrition:meals(AllIDs),
  convlist(mealRecord, AllIDs, MealRecords).

dailyCaloriesRecord([Timestamp, Calories], record(Year, DOY, Cals)) :-
  stamp_date_time(Timestamp, Date, 'UTC'),
  format_time(atom(Year), '%Y', Date, posix),
  format_time(atom(DOY), '%j', Date, posix),
  Cals = Calories.

dailyCaloriesRecords(Records) :-
  nutrition:caloriesByDate(Result),
  convlist(dailyCaloriesRecord, Result, Records).

/*
  + Filename

  writes the meals records as a CSV file to the given filename.
*/
writeMealRecords(Filename) :-
  mealRecords(MealRecords),
  csv_write_file(Filename, [meal_record('ID', 'Cals', 'Time', 'Year', 'DOY', 'Hour')|MealRecords]).

writeDailyCaloriesRecords(Filename) :-
  dailyCaloriesRecords(Records),
  csv_write_file(Filename, [record('Year', 'DOY', 'Cals')|Records]).
