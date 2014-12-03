:- module(
  sw_literal,
  [
    literal//2 % +Language:oneof([manchester,turtle])
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
@compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
@compat SPARQL 1.0
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2014/04-2014/05, 2014/08, 2014/11-2014/12
*/

:- use_module(generics(code_ext)). % Meta-option.

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).

:- use_module(plLangTag(language_tag)).

:- use_module(plRdf(syntax/sw_iri)).
:- use_module(plRdf(syntax/sw_number)).
:- use_module(plRdf(syntax/sw_string)).

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
% @compat SPARQL 1.0 [65]
% @compat SPARQL 1.1 Query [134]
% @compat Turtle 1.1 [133s]

'BooleanLiteral'(false) --> "false".
'BooleanLiteral'(true)  --> "true".



%! 'LANGTAG'(?LangTag:list(atom))// .
% ```ebnf
% LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
% ```
%
% @compat SPARQL 1.0 [76]
% @compat SPARQL 1.1 Query [145]
% @compat Turtle 1.1 [144s]

'LANGTAG'(LangTag) -->
  "@",
  {LangTag = [Tag|Subtags]},
  tag(Tag),
  (   "-",
      '+'(subtag, Subtags, [separator(hyphen)])
  ;   {Subtags = []}
  ).

tag(Tag) -->
  '+'(letter, Tag, [convert1(codes_atom)]).

subtag(Subtag) -->
  '+'(alpha_numeric, Subtag, [separator(codes_atom)]).



%! languageTag(?LangTag:list(atom))// .
% ```BNF
% languageTag := @ (U+40) followed a nonempty sequence of characters matching
%                the langtag production from [BCP 47]
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

languageTag(LangTag) -->
  "@",
  'Language-Tag'(LangTag).



%! lexicalValue(?LexicalForm:atom)// .
% ```bnf
% lexicalValue ::= quotedString
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

lexicalValue(LexicalForm) --> quotedString(LexicalForm).



%! literal(+Language:oneof([manchester,turtle]), ?Literal:compound)// .
% ```ebnf
% [Manchester]   literal ::=   typedLiteral
%                            | stringLiteralNoLanguage
%                            | stringLiteralWithLanguage
%                            | integerLiteral
%                            | decimalLiteral
%                            | floatingPointLiteral
% [Turtle]       literal ::= RDFLiteral | NumericLiteral | BooleanLiteral
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
% @compat Turtle 1.1 [13]

literal(manchester, Literal) --> typedLiteral(Literal).
literal(manchester, Literal) --> stringLiteralNoLanguage(Literal).
literal(manchester, Literal) --> stringLiteralWithLanguage(Literal).
literal(manchester, Literal) --> integerLiteral(Literal).
literal(manchester, Literal) --> decimalLiteral(Literal).
literal(manchester, Literal) --> floatingPointLiteral(Literal).
literal(turtle, Literal) --> 'RDFLiteral'(turtle, Literal).
literal(turtle, Literal) --> 'NumericLiteral'(turtle, Literal).
literal(turtle, Literal) --> 'BooleanLiteral'(Literal).



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
% @compat Turtle 1.1 [16]

'NumericLiteral'(turtle, literal(type(xsd:decimal,Value))) -->
  'DECIMAL'(turtle, Value).
'NumericLiteral'(turtle, literal(type(xsd:double,Value))) -->
  'DOUBLE'(turtle, Value).
'NumericLiteral'(turtle, literal(type(xsd:integer,Value))) -->
  'INTEGER'(turtle, Value).
'NumericLiteral'(sparql, Value) -->
  'NumericLiteralNegative'(Value).
'NumericLiteral'(sparql, Value) -->
  'NumericLiteralPositive'(Value).
'NumericLiteral'(sparql, Value) -->
  'NumericLiteralUnsigned'(Value).



%! 'NumericLiteralNegative'(?Value:number)// is det.
% ```ebnf
% NumericLiteralNegative ::=   INTEGER_NEGATIVE
%                            | DECIMAL_NEGATIVE
%                            | DOUBLE_NEGATIVE
% ```
%
% @compat SPARQL 1.0 [64]
% @compat SPARQL 1.1 Query [133]

'NumericLiteralNegative'(literal(type(xsd:decimal,Value))) -->
  'DECIMAL_NEGATIVE'(Value).
'NumericLiteralNegative'(literal(type(xsd:double,Value))) -->
  'DOUBLE_NEGATIVE'(Value).
'NumericLiteralNegative'(literal(type(xsd:integer,Value))) -->
  'INTEGER_NEGATIVE'(Value).



%! 'NumericLiteralPositive'(?Value:number)// is det.
% ```ebnf
% NumericLiteralPositive ::=   INTEGER_POSITIVE
%                            | DECIMAL_POSITIVE
%                            | DOUBLE_POSITIVE
% ```
%
% @compat SPARQL 1.0 [63]
% @compat SPARQL 1.1 Query [132]

'NumericLiteralPositive'(literal(type(xsd:decimal,Value))) -->
  'DECIMAL_POSITIVE'(Value).
'NumericLiteralPositive'(literal(type(xsd:double,Value))) -->
  'DOUBLE_POSITIVE'(Value).
'NumericLiteralPositive'(literal(type(xsd:integer,Value))) -->
  'INTEGER_POSITIVE'(Value).



%! 'NumericLiteralUnsigned'(?Value:number)// is det.
% ```ebnf
% NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
% ```
%
% @compat SPARQL 1.0 [62]
% @compat SPARQL 1.1 Update [131]

'NumericLiteralUnsigned'(literal(type(xsd:decimal,Value))) -->
  'DECIMAL'(sparql, Value).
'NumericLiteralUnsigned'(literal(type(xsd:double,Value))) -->
  'DOUBLE'(sparql, Value).
'NumericLiteralUnsigned'(literal(type(xsd:integer,Value))) -->
  'INTEGER'(sparql, Value).



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
%        - `literal(type(<datatype>,<lexical-expression>))`
%
% ```ebnf
% RDFLiteral ::= String ( LANGTAG | ( '^^' iri ) )?
% ```
%
% @compat SPARQL 1.0 [128]
% @compat SPARQL 1.1 Query [129]
% @compat Turtle 1.1 [128s] is the same, but uses a different `String`.

% Typed literal.
'RDFLiteral'(Language, literal(type(Datatype,LexicalExpression))) -->
  'String'(Language, LexicalExpression),
  "^^",
  iri(Datatype).
% Language-tagged string.
'RDFLiteral'(Language, literal(lang(LangTag,LexicalExpression))) -->
  'String'(Language, LexicalExpression),
  'LANGTAG'(LangTag0),
  {atomic_list_concat(LangTag0, '-', LangTag)}.
% Simple literal.
'RDFLiteral'(Language, literal(LexicalExpression)) -->
  'String'(Language, LexicalExpression).



%! stringLiteralNoLanguage(?String:atom)// .
% ```bnf
% stringLiteralNoLanguage ::= quotedString
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

stringLiteralNoLanguage(String) --> quotedString(String).



%! stringLiteralWithLanguage(?String:atom)// .
% ```bnf
% stringLiteralWithLanguage ::= quotedString languageTag
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

stringLiteralWithLanguage(LangTag-LexicalForm) -->
  quotedString(LexicalForm),
  languageTag(LangTag).



%! typedLiteral// .
% ```bnf
% typedLiteral ::= lexicalValue '^^' Datatype
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

typedLiteral(literal(type(Datatype,LexicalForm))) -->
  lexicalValue(LexicalForm),
  "^^",
  'Datatype'(Datatype).
