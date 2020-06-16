/*
  The FAQ module allows developers to define answer databases for common
  questions and embed search blocks that users can use to find answers to
  common questions.
*/

var faq_DATABASE_URL = 'modules/faq/database.xml';

/*
  Load the FAQ database file, parse the answers, and register a search source
  that provides the answers for inclusion in search results.
*/
MODULE_LOAD_HANDLERS.add (
  function (done) {
    // I. load and parse the FAQ database file.
    faq_loadDatabase (faq_DATABASE_URL,
      function (error, database) {
        if (error) { return done (error); }

        // II. register a search source that returns the FAQ answers.
        search_registerSource ('faq', function (setId, done) {
          done (null, database);
        });

        done (null);
    });
});

/*
  Accepts two arguments:

  * databaseURL, a URL string
  * and done, a function that accepts two arguments: an Error object; and
  a faq_Database

  loads the database document at databaseURL, parses the document, and passes
  the resulting database object to done.

  If an error occurs, this function passes the error to done instead.
*/
function faq_loadDatabase (databaseURL, done) {
  $.get (databaseURL,
    function (databaseElement) {
      done (null, faq_parseDatabase (databaseElement));
    },
    'xml'
  )
  .fail (function () {
    var error = new Error ('[faq][faq_loadDatabase] Error: an error occured while trying to load "' + databaseURL + '".');
    strictError (error);
    done (error);
  });
}

/*
  Accepts one argument: databaseElement, a JQuery XML Document that represents
  a FAQ Database Document; and returns the database represented by the
  document as a faq_Database object.

  Note: a faq_Database is list of faq_Entry objects.
*/
function faq_parseDatabase (databaseElement) {
  var database = [];
  $('database > entry', databaseElement).each (
    function (i, entryElement) {
      database.push (faq_parseEntry (entryElement));
  });
  return database;
}

/*
  Accepts one argument: entryElement, a JQuery XML Element that represents
  a FAQ answer; and returns the faq_Entry represented by entryElement.
*/
function faq_parseEntry (entryElement) {
  return new faq_Entry (
    $('> id', entryElement).text (),
    $('> title', entryElement).text (),
    $('> keywords', entryElement).text (),
    $('> url', entryElement).text (),
    $('> body', entryElement).text ());
}

/*
  Accepts five arguments:

  * id, a FAQ ID string
  * title, the title that will be displayed in the search result
  * keywords, a space delimited list of keywords that this answer will be returned for
  * url, an optional string linking to some resource
  * and body, an HTML string that represents the answer text
*/
function faq_Entry (id, title, keywords, url, body) {
  search_Entry.call (this, 'faq/' + id);
  this.title    = title;
  this.keywords = keywords;
  this.url      = url;
  this.body     = body;
}

faq_Entry.prototype = Object.create (faq_Entry.prototype);

/*
  Accepts two arguments:

  * query, the query string
  * and done, a function that accepts two arguments: an error object; and
    a JQuery HTML Element.

  creates an HTML element that represents this search entry and passes the
  element to done.
*/
faq_Entry.prototype.getResultElement = function (query, done) {
  var resultElement = this.url == '' ?
    $('<li></li>')
      .addClass ('search_result')
        .addClass ('search_result_link')
        .append ($('<h3></h3>').text (this.title))
        .append (this.body) :
    $('<li></li>')
      .addClass ('search_result')
      .append ($('<a></a>')
        .addClass ('search_result_link')
        .attr ('href', this.url)
        .on ('click', function () {
           ga ('send', 'event', 'faq search result', this.id);
         })
        .append ($('<h3></h3>').text (this.title))
        .append (this.body));


  done (null, resultElement);
}
