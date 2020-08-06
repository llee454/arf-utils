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

mealCreate(Calories, Servings, ID) :-
  base:eventCreate(ID),
  assert_meal(ID),
  base:attributeCreate(ID, AttributeID),
  assert_calories(AttributeID, Calories),
  maplist({ID}/[Serving]>>servingsCreate(ID, Serving), Servings).

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
