/*
  This program defines a server application that
  accepts prolog facts from a Semantic MediaWiki
  instance, stores them in a prolog database
  file, and answers queries against the database.

  to start this application run:

  $ swipl kono.pl
  $ > server(5000).

  Then connect to port 5000.

  Examples:
  http://localhost:5000/status?
  http://localhost:5000/run?command=author(%22larry+le%22),write(%22yes%22)%3bwrite(%22no%22).
  http://localhost:5000/save?fact=author(Author):-book%3F(book(_,Author,_)).
  http://localhost:5000/run?command=findall(X,programmer?(X),Xs),write(Xs).
  http://localhost:5000/run?command=findall(Calories,nutrition:meal(Id,Date,Calories),Xs),write(Xs)
*/

% load the library dependencies to define a server.
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
:- use_module(nutrition).

:- nutrition:attachNutritionDB("NutritionDB.pl").

% define the routing table.
:- http_handler(root(status), statusHandler, []).
:- http_handler(root(run), runHandler, []).
:- http_handler(root(meal), mealHandler, []).

% define the start command.
server(Port) :- http_server(http_dispatch, [port(Port)]).

% process status requests.
statusHandler(_Request)
  :- format('Access-Control-Allow-Origin: *~n'),
     format('Content-type: text/plain~n~n'),
     format('Status: Running~n').

% process query requests.
runHandler(Request) :-
  http_parameters(Request, [command(CommandString, [])]),
  format('Access-Control-Allow-Origin: *~n'),
  format('Content-type: text/plain~n~n'),
  term_string(Command, CommandString),
  call(Command).

mealHandler(Request) :-
  http_parameters(Request, [calories(CaloriesAtom, [])]),
  format('Access-Control-Allow-Origin: *~n'),
  format('Content-type: text/plain~n~n'),
  atom_number(CaloriesAtom, Calories),
  get_time(Date),
  nutrition:recordMeal(0, Date, Calories),
  format('Meal Recorded').
