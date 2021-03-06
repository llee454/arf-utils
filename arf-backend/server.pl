/*
  This program defines a server application that accepts prolog
  facts from a Semantic MediaWiki instance, stores them in a prolog
  database file, and answers queries against the database.

  to start this application run:

  $ swipl server.pl --port=5000 --https=true \
      --certfile="/etc/letsencrypt/live/arf.larrylee.tech/fullchain.pem" \
      --keyfile="/etc/letsencrypt/live/arf.larrylee.tech/privkey.pem"

  Use `netstat -tulpn` to verify that the server started and
  connected to port 5000. Then connect to port 5000 using your
  browser.

  Examples:
  http://localhost:5000/status?
  http://localhost:5000/run?command=author(%22larry+le%22),write(%22yes%22)%3bwrite(%22no%22).
  http://localhost:5000/save?fact=author(Author):-book%3F(book(_,Author,_)).
  http://localhost:5000/run?command=findall(X,programmer?(X),Xs),write(Xs).
  http://localhost:5000/run?command=findall(Calories,nutrition:meal(Id,Date,Calories),Xs),write(Xs)
*/

% load the library dependencies to define a server.
:- use_module(library(http/http_unix_daemon)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).

:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).

% load database modules.
:- use_module(base).
:- base:attachBaseDB("databases/base.pl").

:- use_module(nutrition).
:- nutrition:attachNutritionDB("databases/nutrition.pl").

:- use_module(practice).
:- practice:attachDB("databases/practice.pl").

:- use_module(health).
:- health:attachDB("databases/health.pl").

:- use_module(study).
:- study:attachDB("databases/study.pl").

% load export modules.
:- use_module(nutritionExport).
:- use_module(healthExport).

% define the routing table.
:- http_handler(root(status), statusHandler, []).
:- http_handler(root(run), runHandler, []).
:- http_handler(root(meals), mealsHandler, []).
:- http_handler(root(weight), weightHandler, []).
:- http_handler(root(meals_rate), mealsRateHandler, []).
:- http_handler(root(remaining_cals), remainingCalsHandler, []).
:- http_handler(root(meal_size), mealSizeHandler, []).
:- http_handler(root(hours_till_next_meal), nextMealHandler, []).
:- http_handler(root(daily_cals), dailyCalsHandler, []).

% process status requests.
statusHandler(_Request) :-
  format('Access-Control-Allow-Origin: *~n'),
  format('Content-type: text/plain~n~n'),
  format('Status: Running~n').

% process query requests.
runHandler(Request) :-
  http_parameters(Request, [command(CommandString, [])]),
  format('Access-Control-Allow-Origin: *~n'),
  format('Content-type: text/plain~n~n'),
  term_string(Command, CommandString),
  call(Command).

% process export requests.
mealsHandler(_) :-
  format('Access-Control-Allow-Origin: *~n'),
  nutritionExport:writeMealRecords('exports/meals.csv'),
  http_reply_file('exports/meals.csv', [cache(false)], []).

dailyCalsHandler(_) :-
  format('Access-Control-Allow-Origin: *~n'),
  nutritionExport:writeDailyCaloriesRecords('exports/daily_cals.csv'),
  http_reply_file('exports/daily_cals.csv', [cache(false)], []).

weightHandler(_) :-
  format('Access-Control-Allow-Origin: *~n'),
  healthExport:writeWeightRecords('exports/weight.csv'),
  http_reply_file('exports/weight.csv', [cache(false)], []).

mealsRateHandler(_) :-
  format('Access-Control-Allow-Origin: *~n'),
  format('Content-type: text/plain~n~n'),
  nutrition:calorieConsumptionRate(Rate),
  write(Rate).

remainingCalsHandler(_) :-
  format('Access-Control-Allow-Origin: *~n'),
  format('Content-type: text/plain~n~n'),
  nutrition:remainingCalories(Rem),
  write(Rem).

mealSizeHandler(_) :-
  format('Access-Control-Allow-Origin: *~n'),
  format('Content-type: text/plain~n~n'),
  nutrition:mealSize(Cals),
  write(Cals).

nextMealHandler(_) :-
  format('Access-Control-Allow-Origin: *~n'),
  format('Content-type: text/plain~n~n'),
  nutrition:numHoursTillNextMeal(Hours),
  write(Hours).

% start the server.
% specify the initialization code (http_daemon) and call
% http_server with the router (http_dispatch)
:- initialization(http_daemon, main).

main :-
  http_server(http_dispatch).
