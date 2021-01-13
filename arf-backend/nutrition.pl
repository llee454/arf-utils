/*
 This module defines the nutrition database.

  Note: it is an SWI-Prolog module.
  See: https://www.swi-prolog.org/pldoc/man?section=defmodule.

  Note: this module uses the Persistency library to create and
  manage a database file storing nutrition predicates. See:
  https://www.swi-prolog.org/pldoc/man?section=persistency
*/
:- module(nutrition, []).

:- use_module(base).
:- use_module(aux).
:- use_module(library(persistency)).
:- use_module(library(apply)).
:- use_module(library(yall)).

/*
  Declare the atoms that we will store in the Nutrition database.

  Note: the Persistency library will define four predicates
  for each entry type: name(Arg, ...), assert_name(Arg, ...),
  retract_name(Arg, ...), and retractall_name(Arg, ...)
*/
:- persistent
     meal(event:atom),
     calories(attribute:atom, value:number),
     servings(attribute:atom, type:atom, value:number).

/*
  Accepts one argument: FileName, a string that represents a file
  name; and opens the referenced file as a Nutrition database.
*/
attachNutritionDB(FileName) :-
  db_attach(FileName, []).

servingsCreate(MealID, serving(Type, Amount)) :-
  base:attributeCreate(MealID, AttributeID),
  assert_servings(AttributeID, Type, Amount).

/*
  Accepts one argument: +ID, a servings ID; and deletes the referenced
  serving.
*/
servingsDelete(ID) :-
  base:attributeDelete(ID),
  retract_servings(ID, _, _).

/*
  Accepts three arguments: +Timestamp, an unix timestamp; +Calories, an
  integer; and +Servings, a list of serving records; creates a meal
  record, adds the record to the database, and returns the record ID,
  -ID.
*/
mealCreate(Timestamp, Calories, Servings, ID) :-
  base:eventCreate(Timestamp, ID),
  assert_meal(ID),
  base:attributeCreate(ID, AttributeID),
  assert_calories(AttributeID, Calories),
  maplist({ID}/[Serving]>>servingsCreate(ID, Serving), Servings).

/*
  Accepts two arguments: +Calories, an integer; and +Servings, a list
  of serving records; create a meal record timestamp with the current
  time, adds the record to the database, and returns the record ID,
  -ID.
*/
mealCreate(Calories, Servings, ID) :-
  base:eventCreate(ID),
  assert_meal(ID),
  base:attributeCreate(ID, AttributeID),
  assert_calories(AttributeID, Calories),
  maplist({ID}/[Serving]>>servingsCreate(ID, Serving), Servings).

/*
  Accepts one argument: +ID, an attributeID; and returns true iff there
  exists a calorie attribute that has the given ID.
*/
isCalorieAttribute(ID) :-
  calories(ID, _).

/*
  Accepts one argument: +ID, an attribute ID; and returns true iff there
  exists a servings attribute that has the given ID.
*/
isServingsAttribute(ID) :-
  servings(ID, _, _).

/*
  Accepts one argument: +ID, an attribute ID; and deletes the
  referenced calorie attribute.
*/
deleteCalorieAttribute(ID) :-
  retract_calories(ID, _),
  base:attributeDelete(ID).

/*
  Accepts one argument: +ID, an attribute ID; and deletes the
  referenced servings attribute.
*/
deleteServingsAttribute(ID) :- true.
  retract_servings(ID, _, _),
  base:attributeDelete(ID).

/*
  WARNING: DOES NOT WORK!
  Accepts one argument: +ID, a meal ID; and deletes the referenced meal.
*/
mealDelete(ID) :-
  base:eventDelete(ID),
  retract_meal(ID),
  findall(AttributeID, base:attribute(AttributeID, ID), MealAttributeIDs),
  include(isCalorieAttribute, MealAttributeIDs, CalAttributeIDs),
  maplist(deleteCalorieAttribute, CalAttributeIDs),
  include(isServingsAttribute, MealAttributeIDs, ServingsAttributeIDs),
  maplist(deleteServingsAttribute, ServingsAttributeIDs).

mealToday(MealID) :-
  base:event(MealID, Timestamp),
  aux:timestampToday(Timestamp).

/*
  +ID, -Calories
*/
mealCalories(ID, Calories) :-
  base:attribute(AttributeID, ID),
  calories(AttributeID, Calories).

/* -IDs */
meals(IDs) :-
  findall(ID, meal(ID), IDs).

/*
  Returns -IDs, a list of meal IDs that represent the meals eaten today.
*/
mealsToday(IDs) :-
  meals(AllIDs),
  include(mealToday, AllIDs, IDs).

/*
  Returns -Calories, the number of calories consumed today.
*/
caloriesToday(Calories) :-
  mealsToday(IDs),
  foldl({}/[ID, Acc, Res]>>(mealCalories(ID, MealCalories), Res is Acc + MealCalories), IDs, 0, Calories).

/*
  The recommended daily calorie limit when trying to lose weight.
*/
dailyCalorieLimit(1900).

/*
  Returns the number of calories that I should aim to consume each hour
  when trying to lose weight.

  Note: this function assumes that I eat all of my meals between
  6:00 AM and 9:00 PM (local time).
*/
calorieConsumptionRate(Rate) :-
  dailyCalorieLimit(CalorieLimit),
  Rate is CalorieLimit / (12 + 9 - 6).

/*
  Returns the number of calories that I should aim to consume by the
  current time, +Limit.
*/
calorieLimit(Limit) :-
  get_time(CurrTime),
  aux:convertESTToUTC('6:00', StartTime),
  Hours is (CurrTime - StartTime)/3600,
  calorieConsumptionRate(Rate),
  Limit is Rate * Hours.

/*
  Returns the number of calories remaining to be consumed, +Remaining.

  Note: this function is used to help pace my calorie consumption so
  that throughout a day I can limit my consumption to my target daily
  intake.
*/
remainingCalories(Remaining) :-
  caloriesToday(Consumed),
  calorieLimit(Limit),
  Remaining is Limit - Consumed.

/*
  Returns my recommended meal size given three meals a day and 2 snacks
  of 200 calories, +Cals.
*/
mealSize(Cals) :-
  dailyCalorieLimit(Limit),
  Cals is (Limit - 400)/3.

/*
  Returns the number of hours until I can have my next meal if calorie
  pacing.
*/
numHoursTillNextMeal(Hours) :-
  mealSize(MealCals),
  remainingCalories(RemCals),
  calorieConsumptionRate(Rate),
  Hours is (MealCals - RemCals)/Rate.
