/*
  The citation module defines data structures
  and inference rules for representing textual
  references.

  The purpose of this module is to allow people
  to record facts that they have read, to link
  those facts to their original sources, to
  organize them, and to record the reliability
  of those facts.
*/

% I. Authors

/*
  author(ID,Name)

  Represents authors. ID, is a unique integer
  that uniquely identifies each author; Name, is
  a string that represents the authors full name.
*/

/*
  authorID(+Author, ?AuthorID)

  Accepts two arguments: Author, an author term;
  and AuthorID, an unbound variable; and returns
  the authors ID through AuthorID.

  authorID(+Author, +AuthorID)

  Returns true iff the given author has the
  given author ID.

  authorID(+Author, -AuthorID)

  Returns the ID of the given author.
*/
authorID(Author, AuthorID) :- arg(1, Author, AuthorID).

/*
*/
authorName(Author, AuthorName) :- arg(2, Author, AuthorName).

/*
  authorsNames(+Authors, -AuthorNames)

  Accepts a list of author terms, Authors,
  and returns the names of the given authors
  in AuthorNames.
*/
authorsNames(Authors, AuthorNames) :- maplist(authorName, Authors, AuthorNames).

/*
  isAuthor(+Author)

  Accepts one argument: Author, an author term;
  and asserts that Author is an author.
*/

/*
  authorWithID(+AuthorID, -Author)

  Accepts an author ID and returns the author
  that has the given ID.
*/
authorWithID(AuthorID, Author) :- isAuthor(Author), authorID(Author, AuthorID).

/*
  authorIDsAuthor(+AuthorIDs, -Authors)

  Accepts a list of integers that represent
  author IDs and returns a list of author terms
  that represent the authors that have the given
  IDs in Authors.
*/
authorIDsAuthors(AuthorIDs, Authors) :- maplist(authorWithID, AuthorIDs, Authors).

% II. Sources

/*
  source(+ID, +Title, +AuthorIDs)

  Represents textual sources. 

  * ID, is an integer that uniquely identifies
    each source
  * Title, is a string that represents the
    source's title
  * AuthorIDs, is a list of integers that
    identifies that authors responsible for
    the work
*/

/*
  isSource(?Source)

  Accepts one argument: Source, a source term;
  and asserts that Source is a source.
*/

/*
  sourceID(+Source, ?SourceID)
*/
sourceID(Source, SourceID) :- arg(1, Source, SourceID).

/*
  sourceTitle(+Source, +Title)
*/
sourceTitle(Source, Title) :- arg(2, Source, Title).

/*
  sourceAuthorIDs(+Source, -Authors)
*/
sourceAuthorIDs(Source, AuthorIDs) :- arg(3, Source, AuthorIDs).

/*
  sourceAuthors(+Source, -Authors)

  Accepts one argument: Source, a source term;
  and returns a list of author terms in Authors.
*/
sourceAuthors(Source, Authors)
  :- sourceAuthorIDs(Source, AuthorIDs),
     authorIDsAuthors(AuthorIDs, Authors).

/*
  sourceAuthorNames(+Source, -AuthorNames)

  Accepts a source term, Source, and returns
  the names of the source's authors as a list
  of strings in AuthorNames.
*/
sourceAuthorNames(Source, AuthorNames)
  :- sourceAuthors(Source, Authors),
     authorsNames(Authors, AuthorNames).

/*
  isSourceAuthor(+Source, +AuthorID)

  Accepts two arguments:

  * Source, a source term
  * AuthorID, is an integer that identifies an author

  and returns true iff the source has the given author.
*/
isSourceAuthor(Source, AuthorID)
  :- sourceAuthors(Source, Authors),
     member(AuthorID, Authors).

/*
  sourceWithID(+SourceID, -Source)
*/
sourceWithID(SourceID, Source) :- isSource(Source), sourceID(Source, SourceID).

% III. Citations

/*
  citation(+ID, +SourceID, +Location, +Text)

  Accepts four arguments:

  * ID, an integer
  * SourceID, an integer that represents a source ID
  * Location, a string that describes the location (e.g. page number) of the citation in the source
  * Text, a string that represents the cited text.
*/

/*
  isCitation(?Citation)

  Accepts one argument: Citation, a citation
  term and asserts that Citation is a citation.
*/

/*
*/
citationID(Citation, CitationID) :- arg(1, Citation, CitationID).

/*
*/
citationSourceID(Citation, SourceID) :- arg(2, Citation, SourceID).

/*
*/
citationLocation(Citation, Location) :- arg(3, Citation, Location).

/*
*/
citationText(Citation, Text) :- arg(4, Citation, Text).

/*
*/
citationWithID(CitationID, Citation) :- isCitation(Citation), citationID(Citation, CitationID).

/*
  citationWithSourceID(+SourceID, +Citation)
*/
citationWithSourceID(SourceID, Citation)
  :- isCitation(Citation), citationSourceID(Citation, SourceID).

/*
  citationSourceTitle(+Citation, +SourceTitle)
*/
citationSourceTitle(Citation, SourceTitle)
  :- isCitation(Citation),
     citationSourceID(Citation, SourceID),
     sourceWithID(SourceID, Source),
     sourceTitle(Source, SourceTitle).

% IV. Claim

/*
  claim(+ID, +CitationID, +Text, +Confidence, +Rationale)
*/

/*
  isClaim(?Claim)
*/

/*
*/
claimID(Claim, ClaimID) :- arg(1, Claim, ClaimID).

/*
*/
claimCitationID(Claim, CitationID) :- arg(2, Claim, CitationID).

/*
*/
claimText(Claim, Text) :- arg(3, Claim, Text).

/*
*/
claimConfidence(Claim, Confidence) :- arg(4, Claim, Confidence).

/*
*/
claimRationale(Claim, Rationale) :- arg(5, Claim, Rationale).
