:- module(
  turtle_literal,
  [
    literal//1 % ?Literal:compound
  ]
).

/** <module> Turtle literal

DCGs for literal expression defined in Turtle recommendations.

@author Wouter Beek
@version 2014/04-2014/05, 2014/08
*/

:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generic)).

:- use_module(plSparql_parse(sparql_literal)).
:- use_module(turtle(turtle_number)).



%! literal(?Literal:compound)// .
% ~~~{.ebnf}
% literal ::= RDFLiteral | NumericLiteral | BooleanLiteral
% ~~~
%
% @compat Turtle 1.1 [13].

literal(Literal) --> 'RDFLiteral'(Literal).
literal(Literal) --> 'NumericLiteral'(Literal).
literal(Literal) --> 'BooleanLiteral'(Literal).



%! 'NumericLiteral'(?Literal:compound)// .
% ~~~{.ebnf}
% NumericLiteral ::= INTEGER | DECIMAL | DOUBLE
% ~~~
%
% @compat Turtle 1.1 [16].

'NumericLiteral'(literal(type(xsd:decimal,Value))) -->
  'DECIMAL'(Value).
'NumericLiteral'(literal(type(xsd:double,Value))) -->
  'DOUBLE'(Value).
'NumericLiteral'(literal(type(xsd:integer,Value)) -->
  'INTEGER'(Value).



%! 'RDFLiteral'(?Literal:compound)// is det.
% The general syntax for RDF literals.
% The consist of a string (enclosed in either double or single quotes),
% with either an optional language tag (introduced by `@`)
% or an optional datatype IRI or prefixed name (introduced by `^^`).
%
% @arg `Literal` is a literal compound term
%      as specified in the Semweb library:
%        - `literal(<value>)`
%        - `literal(lang(<langtag>,<value>))`
%        - `literal(type(<datatype>,<lexical-expression>))`
%
% ~~~{.ebnf}
% RDFLiteral ::= String ( LANGTAG | ( '^^' iri ) )?
% ~~~
%
% @compat Turtle 1.1 [128s].

% Typed literal.
'RDFLiteral'(literal(type(Datatype,LexicalExpression)) -->
  'String'(LexicalExpression),
  "^^",
  iri(Datatype).
% Language-tagged string.
'RDFLiteral'(literal(lang(Langtag,Value)) -->
  'String'(Value),
  'LANGTAG'(Langtag).
% Simple literal.
'RDFLiteral'(literal(Value)) -->
  'String'(Value),
