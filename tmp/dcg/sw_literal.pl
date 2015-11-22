:- module(
  sw_literal,
  [
    'BooleanLiteral'//1, % ?Value:boolean
    literal//2, % +Language:oneof([manchester,n,turtle])
                % ?Literal:compound
    'NumericLiteral'//2, % ?Language:oneof([sparql,turtle])
                         % ?Literal:compound
    'NumericLiteralNegative'//1, % ?Value:number
    'NumericLiteralPositive'//1, % ?Value:number
    'RDFLiteral'//2 % ?Language:oneof([sparql,turtle])
                    % ?Literal:compound
  ]
).

/** <module> SW grammar: Literals

Grammar rules for literals in Semantic Web standards.

# Examples

Examples of literal syntax in SPARQL include:

  - "chat"
  - 'chat'@fr with language tag "fr"
  - "xyz"^^<http://example.org/ns/userDatatype>
  - "abc"^^appNS:appDataType
  - '''The librarian said, "Perhaps you would enjoy 'War and Peace'."'''
  - 1, which is the same as "1"^^xsd:integer
  - 1.3, which is the same as "1.3"^^xsd:decimal
  - 1.300, which is the same as "1.300"^^xsd:decimal
  - 1.0e6, which is the same as "1.0e6"^^xsd:double
  - true, which is the same as "true"^^xsd:boolean
  - false, which is the same as "false"^^xsd:boolean

---

@author Wouter Beek
@compat N-Quads 1.1
@compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
@compat SPARQL 1.0
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2015/08-2015/09, 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/sw_iri)).
:- use_module(library(dcg/sw_number)).
:- use_module(library(dcg/sw_string)).
:- use_module(library(ltag/rfc5646)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta('NumericLiteral'(+,o,?,?)).
:- rdf_meta('NumericLiteralNegative'(o,?,?)).
:- rdf_meta('NumericLiteralPositive'(o,?,?)).
:- rdf_meta('NumericLiteralUnsigned'(o,?,?)).





%! 'BooleanLiteral'(?Value:boolean)// is det.
% As a convenience, values of type `xsd:boolean`
% can also be written as `true` or `false`
% (without quotation marks and an explicit datatype IRI).
%
% ```ebnf
% BooleanLiteral ::= 'true' | 'false'
% ```
%
% @compat SPARQL 1.0 [65].
% @compat SPARQL 1.1 Query [134].
% @compat Turtle 1.1 [133s].

'BooleanLiteral'(false) --> "false".
'BooleanLiteral'(true)  --> "true".



%! 'LANGTAG'(?LTag:list(atom))// .
% ```ebnf
% LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
% ```
%
% @compat N-Quads 1.1 [144s].
% @compat SPARQL 1.0 [76].
% @compat SPARQL 1.1 Query [145].
% @compat Turtle 1.1 [144s].

'LANGTAG'(LTag) -->
  "@",
  {LTag = [H|T]},
  tag(H),
  (   "-",
      '+'(subtag, T, [separator(hyphen)])
  ;   {T = []}
  ).

tag(Tag) -->
  dcg_atom('+'(ascii_letter, []), Tag).

subtag(Subtag) -->
  dcg_atom('+'(ascii_alpha_numeric, []), Subtag).



%! languageTag(?LanguageTag:atom)// .
% ```BNF
% languageTag := @ (U+40) followed a nonempty sequence of characters matching
%                the langtag production from [BCP 47]
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition).

languageTag(LTag) -->
  "@",
  'Language-Tag'(LTag).



%! lexicalValue(?LexicalForm:atom)// .
% ```bnf
% lexicalValue ::= quotedString
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition).

lexicalValue(Lex) -->
  quotedString(Lex).



%! literal(
%!   ?Language:oneof([manchester,n,turtle]),
%!   ?Literal:compound
%! )// .
% ```ebnf
% [Manchester]          literal ::=   typedLiteral
%                                   | stringLiteralNoLanguage
%                                   | stringLiteralWithLanguage
%                                   | integerLiteral
%                                   | decimalLiteral
%                                   | floatingPointLiteral
% [N-Quads,N-Triples]   literal ::=   STRING_LITERAL_QUOTE
%                                     ('^^' IRIREF | LANGTAG)?
% [Turtle]              literal ::=   RDFLiteral
%                                   | NumericLiteral
%                                   | BooleanLiteral
% ```
%
% @compat N-Quads 1.1 [7].
% @compat N-Triples 1.1 [6].
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition).
% @compat Turtle 1.1 [13].

literal(manchester, Lit) -->
  typedLiteral(Lit).
literal(manchester, Lit) -->
  stringLiteralNoLanguage(Lit).
literal(manchester, Lit) -->
  stringLiteralWithLanguage(Lit).
literal(manchester, Lit) -->
  integerLiteral(Lit).
literal(manchester, Lit) -->
  decimalLiteral(Lit).
literal(manchester, Lit) -->
  floatingPointLiteral(Lit).
literal(n, Lit) -->
  'STRING_LITERAL_QUOTE'(Lex),
  (   "^^",
      'IRIREF'(D)
  ->  {Lit = literal(type(D,Lex))}
  ;   'LANGTAG'(LTag)
  ->  {Lit = literal(lang(LTag,Lex))}
  ;   {Lit = literal(Lex)}
  ).
literal(turtle, Lit) -->
  'RDFLiteral'(turtle, Lit).
literal(turtle, Lit) -->
  'NumericLiteral'(turtle, Lit).
literal(turtle, Lit) -->
  'BooleanLiteral'(Lit).



%! 'NumericLiteral'(?Language:oneof([sparql,turtle]), ?Literal:compound)// .
% ```ebnf
% [SPARQL]   NumericLiteral ::=   NumericLiteralUnsigned
%                               | NumericLiteralPositive
%                               | NumericLiteralNegative
% [Turtle]   NumericLiteral ::= INTEGER | DECIMAL | DOUBLE
% ```
%
% As a convenience, integers / decimal numbers can be written directly
% (without quotation marks and an explicit datatype IRI)
% and are interpreted as typed literals of datatype
% `xsd:integer`/`xsd:decimal`/`xsd:double`.
%
% The following types of numbers are distinguished:
%   - *Integers* have not exponent and no decimal point.
%   - *Decimals* have a decimal point and no exponent.
%   - *Doubles* have an exponent.
%
% @compat SPARQL 1.0 [61].
% @compat SPARQL 1.1 Query [130].
% @compat Turtle 1.1 [16].

'NumericLiteral'(turtle, literal(type(xsd:decimal,Lex))) -->
  'DECIMAL'(turtle, Lex).
'NumericLiteral'(turtle, literal(type(xsd:double,Lex))) -->
  'DOUBLE'(turtle, Lex).
'NumericLiteral'(turtle, literal(type(xsd:integer,Lex))) -->
  'INTEGER'(Lex).
'NumericLiteral'(sparql, V) --> 'NumericLiteralNegative'(V).
'NumericLiteral'(sparql, V) --> 'NumericLiteralPositive'(V).
'NumericLiteral'(sparql, V) --> 'NumericLiteralUnsigned'(V).



%! 'NumericLiteralNegative'(?Value:number)// is det.
% ```ebnf
% NumericLiteralNegative ::= INTEGER_NEGATIVE
%                          | DECIMAL_NEGATIVE
%                          | DOUBLE_NEGATIVE
% ```
%
% @compat SPARQL 1.0 [64].
% @compat SPARQL 1.1 Query [133].

'NumericLiteralNegative'(V) --> 'DECIMAL_NEGATIVE'(V).
'NumericLiteralNegative'(V) --> 'DOUBLE_NEGATIVE'(V).
'NumericLiteralNegative'(V) --> 'INTEGER_NEGATIVE'(V).



%! 'NumericLiteralPositive'(?Value:number)// is det.
% ```ebnf
% NumericLiteralPositive ::= INTEGER_POSITIVE
%                          | DECIMAL_POSITIVE
%                          | DOUBLE_POSITIVE
% ```
%
% @compat SPARQL 1.0 [63].
% @compat SPARQL 1.1 Query [132].

'NumericLiteralPositive'(V) --> 'DECIMAL_POSITIVE'(V).
'NumericLiteralPositive'(V) --> 'DOUBLE_POSITIVE'(V).
'NumericLiteralPositive'(V) --> 'INTEGER_POSITIVE'(V).



%! 'NumericLiteralUnsigned'(?Value:number)// is det.
% ```ebnf
% NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
% ```
%
% @compat SPARQL 1.0 [62].
% @compat SPARQL 1.1 Update [131].

'NumericLiteralUnsigned'(V) --> 'DECIMAL'(sparql, V).
'NumericLiteralUnsigned'(V) --> 'DOUBLE'(sparql, V).
'NumericLiteralUnsigned'(V) --> 'INTEGER'(V).



%! 'RDFLiteral'(?Language:oneof([sparql,turtle]), ?Literal:compound)// is det.
% The general syntax for RDF literals.
% The consist of a string (enclosed in either double or single quotes),
% with either an optional language tag (introduced by `@`)
% or an optional datatype IRI or prefixed name (introduced by `^^`).
%
% @arg `Literal` is a literal compound term
%      as specified in the Semweb library:
%        - `literal(<value>)`
%        - `literal(lang(<langtag>,<value>))`
%        - `literal(type(<datatype>,<lexical-form>))`
%
% ```ebnf
% RDFLiteral ::= String ( LANGTAG | ( '^^' iri ) )?
% ```
%
% @compat SPARQL 1.0 [128].
% @compat SPARQL 1.1 Query [129].
% @compat Turtle 1.1 [128s] is the same, but uses a different `String`.

% Typed literal.
'RDFLiteral'(Lang, literal(type(D,Lex))) -->
  'String'(Lang, Lex),
  "^^",
  iri(D).
% Language-tagged string.
'RDFLiteral'(Lang, literal(lang(LTag,Lex))) -->
  'String'(Lang, Lex),
  'LANGTAG'(LTag0),
  {atomic_list_concat(LTag0, -, LTag)}.
% Simple literal.
'RDFLiteral'(Lang, literal(Lex)) -->
  'String'(Lang, Lex).



%! stringLiteralNoLanguage(?String:atom)// .
% ```bnf
% stringLiteralNoLanguage ::= quotedString
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition).

stringLiteralNoLanguage(S) -->
  quotedString(S).



%! stringLiteralWithLanguage(?String:pair(atom))// .
% ```bnf
% stringLiteralWithLanguage ::= quotedString languageTag
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition).

stringLiteralWithLanguage(Lex-LTag) -->
  quotedString(Lex),
  languageTag(LTag).



%! typedLiteral// .
% ```bnf
% typedLiteral ::= lexicalValue '^^' Datatype
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition).

typedLiteral(literal(type(D,Lex))) -->
  lexicalValue(Lex),
  "^^",
  'Datatype'(D).
