:- module(
  turtle_parse,
  [
    turtle/2, % +File:atom
              % -Triples:ordset(compound)
    turtleDoc//1 % -Triples:ordset(compound)
  ]
).

/** <module> Turtle

@author Wouter Beek
@version 2014/04-2014/05, 2014/08-2014/09, 2014/11
*/

:- use_module(library(dcg/basics)).
:- use_module(library(ordsets)). % Meta-option.
:- use_module(library(readutil)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(db_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_bracket)).

:- use_module(plRdf(syntax/turtle/turtle_char)).
:- use_module(plRdf(syntax/turtle/turtle_iri)).
:- use_module(plRdf(syntax/turtle/turtle_number)).
:- use_module(plRdf(term/rdf_list)).

:- dynamic(base/1).
:- dynamic(triple/1).

:- rdf_meta(verb(r)).



% PREDICATES

turtle(File, Triples):-
  setup_call_cleanup(
    open(File, read, Stream),
    turtle_stream(Stream, Triples),
    close(Stream)
  ).

turtle_stream(Stream, Triples):-
  read_stream_to_codes(Stream, Codes),
  phrase(turtleDoc(Triples), Codes).



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

%! base// .
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

base -->
  "@base",
  b,
  'IRIREF'(Base),
  {db_replace_all(base(_), base(Base))},
  b,
  ".".



%! blankNodePropertyList(-BNode:bnode, -Pairs:list(pair(iri,rdf_term)))// .
% ```ebnf
% blankNodePropertyList ::= '[' predicateObjectList ']'
% ```
%
% @compat Turtle 1.1 [14].

blankNodePropertyList(BNode, P-Os) -->
  {rdf_bnode(BNode)},
  "[",
    b,
    predicateObjectList(P-Os),
    b,
  "]".



%! collection(-RdfList:bnode)// .
% ```ebnf
% collection ::= '(' object* ')'
% ```
%
% @compat Turtle 1.1 [15].

collection(RdfList) -->
  "(",
    b,
    '*'(object, Objects, [separator(b)]),
    b,
  ")",
  rdf_list(Objects, RdfList).



%! directive// .
% ```ebnf
% directive ::= prefixID | base | sparqlPrefix | sparqlBase
% ```
%
% @compat Turtle 1.1 [3].

directive --> prefixID.
directive --> base.
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
  blankNodePropertyList(BNode, P-Os).
object(Literal) -->
  literal(Literal).



%! objectList(?Objects:list(rdf_term))// .
% ```ebnf
% objectList ::= object (',' object)*
% ```
%
% @compat Turtle 1.1 [8].

objectList(Objects) -->
  '+'(object, Objects, [separator(comma)]).



%! predicate(-Predicate:iri)// .
% ```ebnf
% predicate ::= iri
% ```
%
% @compat Turtle 1.1 [11].

predicate(Iri) -->
  iri(Iri).



%! predicateObjectList(?Pairs:pair(iri,list(rdf_term)))// .
% ```ebnf
% predicateObjectList ::= verb objectList (';' (verb objectList)?)*
% ```
%
% @compat Turtle 1.1 [7].

predicateObjectList(Pairs) -->
  '+'(predicateObjectList_content, Pairs, [separator('+'(semi_colon, []))]).

predicateObjectList_content(P-Os) -->
  verb(P),
  b,
  objectList(Os).



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
  "BASE",
  b,
  'IRIREF'(Base),
  {db_replace(base(_), base(Base))}.



%! sparqlPrefix// .
% ```ebnf
% sparqlPrefix ::= "PREFIX" PNAME_NS IRIREF
% ```
%
% @compat Turtle 1.1 [6s].

sparqlPrefix -->
  "PREFIX",
  b,
  'PNAME_NS'(PrefixLabel),
  b,
  'IRIREF'(Iri),
  {rdf_register_prefix(PrefixLabel, Iri)}.



%! statement(-Triples:ordset(compound))// .
% ```ebnf
% statement ::= directive | triples '.'
% ```
%
% @compat Turtle 1.1 [2].

statement([]) -->
  comment.
statement([]) -->
  directive.
statement(Triples) -->
  triples(Triples),
  b,
  ".".



%! subject(-Subject:or([bnode,iri,literal]))// .
% ```ebnf
% subject ::= iri | BlankNode | collection
% ```
%
% @compat Turtle 1.1 [10].

subject(Iri) --> iri(Iri).
subject(BNode) --> 'BlankNode'(BNode).
subject(RdfList) --> collection(RdfList).



%! triples(?Triples:list(compound))// .
% ```ebnf
% triples ::=   subject               predicateObjectList
%             | blankNodePropertyList predicateObjectList?
% ```
%
% @compat Turtle 1.1 [6].

triples(Ts) -->
  subject(S),
  b,
  predicateObjectList(P-Os),
  {maplist(triple_components(S, P), Os, Ts)}.
triples -->
  blankNodePropertyList(BNode),
  (   b, predicateObjectList(P-Os)
  ;   ""
  ).



%! turtleDoc(-Triples:ordset(compound))// .
% ```ebnf
% turtleDoc ::= statement*
% ```
%
% @compat Turtle 1.1 [1].

turtleDoc(Triples) -->
  '*'(statement, Triples, [convert1(ord_union)]).



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



triple_components(S, P, O, rdf(S,P,O)).





% MESSAGES

:- multifile(prolog:message//1).

prolog:message(test_successful(File)):-
  ['Test successful for file ',File].

