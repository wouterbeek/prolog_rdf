:- module(
  sw_number,
  [
    'DECIMAL'//2, % +Language:oneof([sparql,turtle])
                  % ?Value:float
    'DECIMAL_NEGATIVE'//1, % ?Value:float
    'DECIMAL_POSITIVE'//1, % ?Value:float
    'DOUBLE'//2, % +Language:oneof([sparql,turtle])
                 % ?Value:float
    'DOUBLE_NEGATIVE'//1, % ?Value:float
    'DOUBLE_POSITIVE'//1, % ?Value:float
    'EXPONENT'//1, % ?Value:integer
    'INTEGER'//2, % +Language:oneof([sparql,turtle])
                  % ?Value:integer
    'INTEGER_NEGATIVE'//1, % ?Value:integer
    'INTEGER_POSITIVE'//1 % ?Value:integer
  ]
).

/** <module> SW grammar: Numbers

Grammar rules for numbers as defined by Semantic Web standards.

---

@author Wouter Beek
@compat SPARQL 1.0
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2014/04-2014/05, 2014/11-2014/12
*/

:- use_module(math(math_ext)).
:- use_module(math(radix)). % Meta-option.

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_cardinal)).





%! 'DECIMAL'(+Language:oneof([sparql,turtle]), ?Value:float)// .
% ```ebnf
% [SPARQL]   DECIMAL ::= [0-9]* '.' [0-9]+
% [Turtle]   DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
% ```
%
% @compat SPARQL 1.0 [78]
% @compat SPARQL 1.1 Query [147]
% @compat Turtle 1.1 [20]

'DECIMAL'(sparql, N) --> decimal(N).
'DECIMAL'(turtle, N) --> signed_decimal(N).



%! 'DECIMAL_NEGATIVE'(?Value:float)// .
% ```bnf
% DECIMAL_NEGATIVE ::= '-' DECIMAL
% ```
%
% @compat SPARQL 1.0 [84]
% @compat SPARQL 1.1 Query [153]

'DECIMAL_NEGATIVE'(N) -->
  "-",
  (   {var(N)}
  ->  'DECIMAL'(N0),
      {N is copysign(N0, -1)}
  ;   {N0 is abs(N)},
      'DECIMAL'(N0)
  ).



%! 'DECIMAL_POSITIVE'(?Value:float)// .
% ```bnf
% DECIMAL_POSITIVE ::= '+' INTEGER
% ```
%
% @compat SPARQL 1.0 [81]
% @compat SPARQL 1.1 Query [149]

'DECIMAL_POSITIVE'(N) -->
  "+",
  'DECIMAL'(N).



%! 'DOUBLE'(+Language:oneof([sparql,turtle]), ?Value:float)// .
% ```ebnf
% [SPARQL]   DOUBLE ::=   [0-9]+ '.' [0-9]* EXPONENT
%                       | '.' ([0-9])+ EXPONENT
%                       | ([0-9])+ EXPONENT
% [Turtle]   DOUBLE ::= [+-]?
%                       ( [0-9]+ '.' [0-9]* EXPONENT
%                       | '.' [0-9]+ EXPONENT
%	                      | [0-9]+ EXPONENT
%                       )
% ```
%
% @compat SPARQL 1.0 [79]
% @compat SPARQL 1.1 Query [148]
% @compat Turtle 1.1 [21]
% @tbd How to write the exponent in the generative case?

'DECIMAL'(sparql, N) --> number(N).
'DECIMAL'(turtle, N) --> signed_decimal(N).

'DOUBLE'(sparql, N) --> double(N).
'DOUBLE'(turtle, N) --> signed_double(N).



%! 'DOUBLE_POSITIVE'(?Value:float)// .
% ```bnf
% DOUBLE_POSITIVE ::= '+' DOUBLE
% ```
%
% @compat SPARQL 1.0 [82]
% @compat SPARQL 1.1 Query [151]

'DOUBLE_POSITIVE'(N) -->
  "+",
  'DOUBLE'(N).



%! 'DOUBLE_NEGATIVE'(?Value:float)// .
% ```bnf
% DOUBLE_NEGATIVE ::= '-' DOUBLE
% ```
%
% @compat SPARQL 1.0 [85]
% @compat SPARQL 1.1 Query [154]

'DOUBLE_NEGATIVE'(N) -->
  "-",
  (   {var(N)}
  ->  'DOUBLE'(sparql, N0),
      {N is copysign(N0, -1)}
  ;   {N0 is abs(N)},
      'DOUBLE'(sparql, N0)
  ).



%! 'EXPONENT'(?Value:float)// .
% ```ebnf
% EXPONENT ::= [eE] [+-]? [0-9]+
% ```
%
% @compat SPARQL 1.0 [86]
% @compat SPARQL 1.1 Query [155]
% @compat Turtle 1.1 [154s]

'EXPONENT'(N) --> exponent(N).



%! 'INTEGER'(?Language:oneof([sparql,turtle]), ?Value:integer)// .
% ```ebnf
% [SPARQL]   INTEGER ::= [0-9]+
% [Turtle]   INTEGER ::= [+-]? [0-9]+
% ```
%
% @compat SPARQL 1.0 [77]
% @compat SPARQL 1.1 Query [146]
% @compat Turtle 1.1 [19]

'INTEGER'(sparql, N) --> integer(N).
'INTEGER'(turtle, N) --> signed_integer(N).



%! 'INTEGER_POSITIVE'(?Value:integer)// .
% ```bnf
% INTEGER_POSITIVE ::= '+' INTEGER
% ```
%
% @compat SPARQL 1.0 [80]
% @compat SPARQL 1.1 Query [149]

'INTEGER_POSITIVE'(N) -->
  "+",
  'INTEGER'(sparql, N).



%! 'INTEGER_NEGATIVE'(?Value:integer)// .
% ```bnf
% INTEGER_NEGATIVE ::= '-' INTEGER
% ```
%
% @compat SPARQL 1.0 [83]
% @compat SPARQL 1.1 Query [152]

'INTEGER_NEGATIVE'(N) -->
  "-",
  (   {var(N)}
  ->  'INTEGER'(sparql, N0),
      {N is copysign(N0, -1)}
  ;   {N0 is abs(N)},
      'INTEGER'(sparql, N0)
  ).
