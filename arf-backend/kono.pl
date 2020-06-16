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

% define the routing table.
:- http_handler(root(kono), konoHandler, []).

:- http_handler('/kono', konoHandler, []).
:- http_handler('/kono/read', http_reply_file('database.pl', []), []).
:- http_handler(root(status), statusHandler, []).
:- http_handler(root(save), saveHandler, []).
:- http_handler(root(run), runHandler, []).
:- http_handler(root(.), upload_form, []).
:- http_handler(root(upload), upload, []).
:- http_handler(root(meal), mealHandler, []).

% define the start command.
server(Port) :- http_server(http_dispatch, [port(Port)]).

konoHandler(Request)
  :- http_parameters(Request, [save(Fact, [default(false)]), resolve(TermString, [default(false)])]),
     format('Content-type: text/plain~n~n'),
     (saveH(Fact); resolveH(TermString)).

saveH(Fact)
  :- Fact \= false, !,
     open('database.pl', append, Handle),
     write(Handle, Fact), nl(Handle),
     close(Handle),
     format('Success'),
     write("save"), nl.

resolveH(TermString)
  :- TermString \= false, !,
     write("resolve"), nl,
     consult('database.pl'),
     term_string(Term, TermString),
     call(Term).


% process status requests.
statusHandler(_Request)
  :- format('Content-type: text/plain~n~n'),
     format('Status: Running~n').

% process save requests.
saveHandler(Request)
  :- http_parameters(Request, [fact(Fact, [])]),
     open('database.pl', append, Handle),
     write(Handle, Fact), nl(Handle),
     close(Handle),
     format('Content-type: text/plain~n~n'),
     format('Success').

% process query requests.
runHandler(Request)
  :- http_parameters(Request, [command(CommandString, [])]),
     format('Content-type: text/plain~n~n'),
     consult('database.pl'),
     term_string(Command, CommandString),
     call(Command).

mealHandler(Request) :-
  http_parameters(Request, [calories(CaloriesAtom, [])]),
  atom_number(CaloriesAtom, Calories),
  get_time(Date),
  nutrition:attachNutritionDB("NutritionDB.pl"),
  nutrition:recordMeal(0, Date, Calories),
  format('Content-type: text/plain~n~n'),
  format('Meal Recorded').

% process file upload requests.
upload_form(_Request) :-
  reply_html_page(
      title('Upload a file'),
      [ h1('Upload a file'),
        form([ method('POST'),
         action(location_by_id(upload)),
         enctype('multipart/form-data')
       ],
       table([],
       [ tr([td(input([type(file), name(file)]))]),
         tr([td(align(right),
          input([type(submit), value('Upload!')]))])
       ]))
      ]).

upload(Request) :-
  multipart_post_request(Request), !,
  http_read_data(Request, Parts,
           [ on_filename(save_file)
           ]),
  memberchk(file=file(FileName, Saved), Parts),
  format('Content-type: text/plain~n~n'),
  format('Saved your file "~w" into "~w"~n', [FileName, Saved]).
upload(_Request) :-
  throw(http_reply(bad_request(bad_file_upload))).

multipart_post_request(Request) :-
  memberchk(method(post), Request),
  memberchk(content_type(ContentType), Request),
  http_parse_header_value(
      content_type, ContentType,
      media(multipart/'form-data', _)).

:- public save_file/3.

save_file(In, file(FileName, File), Options) :-
	option(filename(FileName), Options),
        open('database.pl', write, Handle),
	copy_stream_data(In, Handle),
        close(Handle).

:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
	[ 'A file upload must be submitted as multipart/form-data using', nl,
	  'name=file and providing a file-name'
	].
