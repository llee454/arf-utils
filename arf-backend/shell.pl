/*
 This module initizlies the SWI Prolog REPL so that it can be
 used as a command line utility for interacting with the Prolog
 ARF databases.
*/

% load database modules.
:- use_module(base).
:- base:attachBaseDB("databases/base.pl").

:- use_module(nutrition).
:- nutrition:attachNutritionDB("databases/nutrition.pl").

:- use_module(practice).
:- practice:attachDB("databases/practice.pl").

:- use_module(health).
:- health:attachDB("databases/health.pl").
