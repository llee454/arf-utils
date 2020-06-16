/*
 This module defines the nutrition database.

  Note: it is an SWI-Prolog module.
  See: https://www.swi-prolog.org/pldoc/man?section=defmodule.

  Note: this module uses the Persistency library to create and
  manage a database file storing nutrition predicates. See:
  https://www.swi-prolog.org/pldoc/man?section=persistency
*/
:- module(nutrition, []).

:- use_module(library(persistency)).

/*
  Declare the atoms that we will store in the Nutrition database.

  Note: the Persistency library will define four predicates
  for each entry type: name(Arg, ...), assert_name(Arg, ...),
  retract_name(Arg, ...), and retractall_name(Arg, ...)
*/
:- persistent
     meal(id:nonneg, date:float, calories:float).

/*
  Accepts one argument: FileName, a string that represents a file
  name; and opens the referenced file as a Nutrition database.
*/
attachNutritionDB(FileName) :-
  db_attach(FileName, []).

/*
  Adds a meal entry to the Nutrition database.
*/
recordMeal(Id, Date, Calories) :-
  with_mutex(nutrition, assert_meal(Id, Date, Calories)).

