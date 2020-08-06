ARF Backend Server Readme
=========================

To start the server run:

> swipl server.pl
$ :- server(5000).

This will start the ARF backend server on port 5000. The ARF frontend connects to port 5000 to send prolog queries to this server.


Conventions
-----------

Whenever we need to represent an entity in our database and that entity has a set of attributes, we create a single data structure that stores all of these attributes.

Whenever, we need to represent a subtype of a given entity, we use an ID reference to link the attributes specific to the subtype to the attributes provided by the base type.
