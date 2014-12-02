:- module(
  sparql_literal,
  [
    'BooleanLiteral'//1, % ?Value:boolean
    'NumericLiteral'//1, % ?Value:number
    'NumericLiteralNegative'//1, % ?Value:number
    'NumericLiteralPositive'//1, % ?Value:number
    'RDFLiteral'//1, % ?Literal:compound
    'String'//1, % ?Literal:atom
    'STRING_LITERAL1'//1, % ?Literal:atom
    'STRING_LITERAL2'//1, % ?Literal:atom
    'STRING_LITERAL_LONG1'//1, % ?Literal:atom
    'STRING_LITERAL_LONG2'//1 % ?Literal:atom
  ]
).

/** <module> SPARQL literal

DCGs for literal definition in SPARQL recommendations.

Examples
========

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

--

@author Wouter Beek
@version 2014/04-2014/05, 2014/08-2014/09
*/

:- use_module(library(lists)).

:- use_module(math(radix)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_meta)).

:- use_module(plRdf(syntax/turtle/turtle_char)).

:- meta_predicate('STRING_LITERAL_LONG'(//,-,?,?)).



%! 'BooleanLiteral'(?Value:boolean)// is det.
% As a convenience, values of type `xsd:boolean`
% can also be written as `true` or `false`
% (without quotation marks and an explicit datatype IRI).
%
% ~~~{.ebnf}
% BooleanLiteral ::= 'true' | 'false'
% ~~~
%
% @compat SPARQL 1.0 [133].
% @compat SPARQL 1.1 Query [134].
% @compat Turtle 1.1 [133s].

'BooleanLiteral'(false) --> "false".
'BooleanLiteral'(true)  --> "true".



%! 'LANGTAG'(?LanguageTag:list(atom))// .
% ~~~{.ebnf}
% LANGTAG ::= '@' [a-zA-Z]+ ( '-' [a-zA-Z0-9]+ )*
% ~~~
%
% @compat SPARQL 1.0 [144].
% @compat SPARQL 1.1 Query [145].
% @compat Turtle 1.1 [144s].

'LANGTAG'([Tag|Subtags]) -->
  "@",
  tag(Tag),
  '*'(subtag, Subtags, []).


%! 'NumericLiteral'(?Value:number)// is det.
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
% ~~~{.ebnf}
% NumericLiteral ::=   NumericLiteralUnsigned
%                    | NumericLiteralPositive
%                    | NumericLiteralNegative
% ~~~
%
% @compat SPARQL 1.1 Query [130].

'NumericLiteral'(Value) --> 'NumericLiteralNegative'(Value).
'NumericLiteral'(Value) --> 'NumericLiteralPositive'(Value).
'NumericLiteral'(Value) --> 'NumericLiteralUnsigned'(Value).



%! 'NumericLiteralNegative'(?Value:number)// is det.
% ~~~{.ebnf}
% NumericLiteralNegative ::=   INTEGER_NEGATIVE
%                            | DECIMAL_NEGATIVE
%                            | DOUBLE_NEGATIVE
% ~~~
%
% @compat SPARQL 1.1 Query [132].

'NumericLiteralNegative'(Value) --> 'DECIMAL_NEGATIVE'(Value).
'NumericLiteralNegative'(Value) --> 'DOUBLE_NEGATIVE'(Value).
'NumericLiteralNegative'(Value) --> 'INTEGER_NEGATIVE'(Value).



%! 'NumericLiteralPositive'(?Value:number)// is det.
% ~~~{.ebnf}
% NumericLiteralPositive ::=   INTEGER_POSITIVE
%                            | DECIMAL_POSITIVE
%                            | DOUBLE_POSITIVE
% ~~~
%
% @compat SPARQL 1.1 Query [132].

'NumericLiteralPositive'(Value) --> 'DECIMAL_POSITIVE'(Value).
'NumericLiteralPositive'(Value) --> 'DOUBLE_POSITIVE'(Value).
'NumericLiteralPositive'(Value) --> 'INTEGER_POSITIVE'(Value).



%! 'NumericLiteralUnsigned'(?Value:number)// is det.
% ~~~{.ebnf}
% NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
% ~~~
%
% @compat SPARQL 1.1 Update [131].

'NumericLiteralUnsigned'(Value) --> 'DECIMAL'(Value).
'NumericLiteralUnsigned'(Value) --> 'DOUBLE'(Value).
'NumericLiteralUnsigned'(Value) --> 'INTEGER'(Value).



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
% @compat SPARQL 1.0 [128].
% @compat SPARQL 1.1 Query [129].
% @compat Turtle 1.1 [128s] is the same, but uses a different `String`.

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
  'String'(Value).



%! 'String'(?Literal:atom)// .
% ~~~{.ebnf}
% String ::= STRING_LITERAL1 |
%            STRING_LITERAL2 |
%            STRING_LITERAL_LONG1 |
%            STRING_LITERAL_LONG2
% ~~~
%
% @compat SPARQL 1.1 Query [135].

'String'(Literal) -->
  'STRING_LITERAL1'(Literal).
'String'(Literal) -->
  'STRING_LITERAL2'(Literal).
'String'(Literal) -->
  'STRING_LITERAL_LONG1'(Literal).
'String'(Literal) -->
  'STRING_LITERAL_LONG2'(Literal).



%! 'STRING_LITERAL1'(?Literal:atom)// .
% ~~~{.ebnf}
% STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
% ~~~
%
% @compat SPARQL 1.1 Query [156].
% @compat This differs from Turtle 1.1 [23] in which
%         escape sequences for Unicode characters
%         are allowed to occur.

'STRING_LITERAL1'(Literal) -->
  quoted(single_quote, dcg_atom_codes('STRING_LITERAL1*', Literal)).

'STRING_LITERAL1*'([H|T]) -->
  'ECHAR'(C),
  {
    \+ single_quote(C),
    \+ backslash(C),
    \+ newline(C),
    \+ carriage_return(C)
  },
  'STRING_LITERAL1*'(T).
'STRING_LITERAL1*' --> [].



%! 'STRING_LITERAL2'(?Literal:atom)// .
% ~~~{.ebnf}
% STRING_LITERAL2 ::= '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
% ~~~
%
% @compat SPARQL 1.1 Query [157].
% @compat This differs from Turtle 1.1 [22] in which
%         escape sequences for Unicode characters
%         are allowed to occur.

'STRING_LITERAL2'(Literal) -->
  quoted(double_quote, dcg_atom_codes('STRING_LITERAL2*', Literal)).

'STRING_LITERAL2*'([H|T]) -->
  'ECHAR'(C),
  {
    \+ double_quote(C),
    \+ backslash(C),
    \+ newline(C),
    \+ carriage_return(C)
  },
  'STRING_LITERAL2*'(T).
'STRING_LITERAL2*' --> [].



%! 'STRING_LITERAL_LONG1'(?Literal:atom)// .
% A literal that can contain unescaped single quotes and newlines.
%
% ~~~{.ebnf}
% STRING_LITERAL_LONG1 ::= "'''"
%                          ( ( "'" | "''" )? ( [^'\] | ECHAR ) )*
%                          "'''"
% ~~~
%
% @compat SPARQL 1.1 Update [158].
% @compat This differs from Turtle 1.1 [24] in which
%         escape sequences for Unicode characters
%         are allowed to occur.

'STRING_LITERAL_LONG1'(Literal) -->
  'STRING_LITERAL_LONG'(single_quote, Literal).



%! 'STRING_LITERAL_LONG2'(?Literal:atom)// .
% A literal that can contain unescaped single quotes and newlines.
%
% ~~~{.ebnf}
% STRING_LITERAL_LONG2 ::= '"""'
%                          ( ( '"' | '""' )? ( [^"\] | ECHAR ) )*
%                          '"""'
% ~~~
%
% @compat SPARQL 1.1 Update [159].
% @compat This differs from Turtle 1.1 [25] in which
%         escape sequences for Unicode characters
%         are allowed to occur.

'STRING_LITERAL_LONG2'(Literal) -->
  'STRING_LITERAL_LONG'(double_quote, Literal).



% Helpers

'STRING_LITERAL_LONG'(Quote, Literal) -->
  quoted(
    triple_quote(Quote),
    dcg_atom_codes('STRING_LITERAL_LONG*'(Quote), Literal)
  ).

'STRING_LITERAL_LONG*'(Quote, [H|T]) -->
  'STRING_LITERAL_LONG*_char'(Quote, H),
  'STRING_LITERAL_LONG*'(Quote, T).
'STRING_LITERAL_LONG*'(_, []) --> [].

'STRING_LITERAL_LONG*_char'(Quote, C) -->
  (Quote, Quote ; Quote ; ``),
  'ECHAR'(C),
  {\+ member(C, [39,92])}. % Apostrophe, backslash.


%! subtag(?Subtag:atom)// .

subtag(Subtag) -->
  (   {var(Subtag)}
  ->  "-",
      '+'(ascii_alpha_numeric, SubtagCodes, []),
      {atom_codes(Subtag, SubtagCodes)}
  ;   {atom_codes(Subtag, SubtagCodes)},
      '+'(ascii_alpha_numeric, SubtagCodes, [])
  ).


%! tag(?Tag:atom)// .

tag(Tag) -->
  (   {var(Tag)}
  ->  '+'(ascii_letter, TagCodes, []),
      {atom_codes(Tag, TagCodes)}
  ;   {atom_codes(Tag, TagCodes)},
      '+'(ascii_letter, TagCodes, [])
  ).

