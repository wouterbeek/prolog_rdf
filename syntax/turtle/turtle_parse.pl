:- module(
  turtle_parse,
  [
    statement//0,
    test/0,
    turtle/1, % +File:atom
    turtleDoc//0
  ]
).

/** <module> Turtle

@author Wouter Beek
@version 2014/04-2014/05, 2014/08-2014/09
*/

:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(db_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_bracket)).

:- use_module(plRdf(syntax/turtle/turtle_number)).
:- use_module(plRdf(term/rdf_list)).

:- dynamic(base/1).
:- dynamic(triple/1).

:- rdf_meta(verb(r)).



% PREDICATES

turtle(File):-
  setup_call_cleanup(
    open(File, read, Stream),
    turtle_stream(Stream),
    close(Stream)
  ).

turtle_stream(Stream):-
  read_stream_to_codes(Stream, Codes),
  phrase(turtleDoc, Codes).



% TEST

test:-
  forall(
    between(0, 30, N),
    test(N)
  ).

test(N):-
  (    N =< 9
  ->   format(atom(Base), 'test-0~d', [N])
  ;    format(atom(Base), 'test-~d', [N])
  ),
  absolute_file_name(turtle(Base), File, [access(read),extensions([ttl])]),
  turtle(File),
  print_message(information, test_successful(File)).



% GRAMMAR

%! base(?BaseIri:atom)// is det.
% Sets the current Base IRI.
%
% The scope of the base IRI
% starts at the end of the `@base` or `BASE` directive
% and ends at the beginning of the next `@base` or `BASE` diretive
% or at the end of the file.
%
% ```abnf
% base ::= '@base' IRIREF '.'
% ```
%
% @compat Turtle 1.1 [5].

base(Base) -->
  "@base", b,
  'IRIREF'(Base),
  {db_replace_all(base(_), base(Base))},
  b, ".".



%! blankNodePropertyList(-BNode:bnode)// .
% ```ebnf
% blankNodePropertyList ::= '[' predicateObjectList ']'
% ```
%
% @compat Turtle 1.1 [14].

blankNodePropertyList(BNode) -->
  {rdf_bnode(BNode)},
  "[",
    '*'(b, []),
    predicateObjectList(BNode),
    '*'(b, []),
  "]".



%! collection(-RdfList:bnode)// .
% ```ebnf
% collection ::= '(' object* ')'
% ```
%
% @compat Turtle 1.1 [15].

collection(RdfList) -->
  "(",
    '*'(b, []),
    '*'(object, Objects, [separator('+'(b, []))]),
    '*'(b, []),
  ")",
  rdf_list(Objects, RdfList).


%! directive// .
% ```ebnf
% directive ::= prefixID | base | sparqlPrefix | sparqlBase
% ```
%
% @compat Turtle 1.1 [3].

directive --> prefixID.
directive --> base(Base).
directive --> sparqlPrefix.
directive --> sparqlBase.



%! object// .
% ```ebnf
% object ::=   iri
%            | BlankNode
%            | collection
%            | blankNodePropertyList
%            | literal
% ```
%
% @compat Turtle 1.1 [12].

object(Iri) -->
  iri(Iri).
object(BNode) -->
  'BlankNode'(BNode).
object(RdfList) -->
  collection(RdfList).
object(BNode) -->
  blankNodePropertyList(BNode).
object(Literal) -->
  literal(Literal).



%! objectList(+Subject:or([bnode,iri]), +Predicate:iri)// .
% ```ebnf
% objectList ::= object (',' object)*
% ```
%
% @compat Turtle 1.1 [8].

objectList(S, P) -->
  object(O),
  {rdf_assert(S, P, O)},
  (
    comma_separator,
    object(O),
    {rdf_assert(S, P, O)},
    objectList(S, P)
  ;
    ""
  ).



%! predicate(-Predicate:iri)// .
% ```ebnf
% predicate ::= iri
% ```
%
% @compat Turtle 1.1 [11].

predicate(Iri) -->
  iri(Iri).


%! predicateObjectList(+Subject:or([bnode,iri]))// .
% ```ebnf
% predicateObjectList ::= verb objectList (';' (verb objectList)?)*
% ```
%
% @compat Turtle 1.1 [7].

predicateObjectList(S) -->
  verb(P), b,
  objectList(S, P).
predicateObjectList(S) -->
  verb(P), b,
  objectList(S, P),
  semicolon_separator,
  predicateObjectList(S).



%! prefixID// .
% ```ebnf
% prefixID ::= '@prefix' PNAME_NS IRIREF '.'
% ```
%
% @compat Turtle 1.1 [4].

prefixID -->
  "@prefix", b,
  'PNAME_NS'(PrefixLabel), b,
  'IRIREF'(Iri), b, `.`,
  {rdf_register_prefix(PrefixLabel, Iri)}.



%! sparqlBase// .
% ```ebnf
% sparqlBase ::= "BASE" IRIREF
% ```
%
% @compat Turtle 1.1 [5a].

sparqlBase -->
  'BaseDecl'(Base),
  {db_replace(base(_), base(Base))}.



%! sparqlPrefix// .
% ```ebnf
% sparqlPrefix ::= "PREFIX" PNAME_NS IRIREF
% ```
%
% @compat Turtle 1.1 [6s].

sparqlPrefix -->
  'PrefixDecl'(PrefixLabel, Iri),
  {rdf_register_prefix(PrefixLabel, Iri)}.



%! statement// .
% ```ebnf
% statement ::= directive | triples '.'
% ```
%
% @compat Turtle 1.1 [2].

statement -->
  comment.
statement -->
  directive.
statement -->
  triples,
  b, `.`.



%! subject(-Subject:or([bnode,iri,literal]))// .
% ```ebnf
% subject ::= iri | BlankNode | collection
% ```
%
% @compat Turtle 1.1 [10].

subject(Iri) --> iri(Iri).
subject(BNode) --> 'BlankNode'(BNode).
subject(RdfList) --> collection(RdfList).



%! triples// .
% ```ebnf
% triples ::=   subject               predicateObjectList
%             | blankNodePropertyList predicateObjectList?
% ```
%
% @compat Turtle 1.1 [6].

triples -->
  subject(S), b,
  predicateObjectList(S).
triples -->
  blankNodePropertyList(BNode),
  (b, predicateObjectList(BNode); "").



%! turtleDoc(-Triples:list(compound))// .
% ```ebnf
% turtleDoc ::= statement*
% ```
%
% @compat Turtle 1.1 [1].

turtleDoc(Triples) -->
  turtleDoc(_, [], Triples).

turtleDoc(Base1, Triples1, Triples3) -->
  statement(Base1, Triples1, Base2, Triples2), b,
  turtleDoc(Base2, Triples2, Triples3).
turtleDoc(_, Triples, Triples) --> [].



%! verb(?Iri:atom)// .
% ```ebnf
% verb ::= predicate | 'a'
% ```
%
% @compat Turtle 1.1 [9].

verb(Iri) -->
  predicate(Iri).
verb(rdf:type) -->
  "a".



% HELPERS

b --> blanks, comment, !, b.
b --> blanks.

comma_separator -->
  b, ",", b.

comment --> `#`, string(_), newline.

newline --> [10].

:- multifile(prolog:message//1).

prolog:message(test_successful(File)):-
  ['Test successful for file ',File].

