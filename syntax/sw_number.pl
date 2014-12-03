:- module(
  sw_number,
  [
    'DECIMAL'//2, % +Language:oneof([sparql,turtle])
                  % ?Decimal:float
    decimalLiteral//1, % ?Decimal:float
    'DECIMAL_NEGATIVE'//1, % ?Decimal:float
    'DECIMAL_POSITIVE'//1, % ?Decimal:float
    'DOUBLE'//2, % +Language:oneof([sparql,turtle])
                 % ?Double:float
    'DOUBLE_NEGATIVE'//1, % ?Double:float
    'DOUBLE_POSITIVE'//1, % ?Double:float
    'EXPONENT'//1, % ?Value:integer
    floatingPointLiteral//1, % ?Float:float
    'INTEGER'//2, % +Language:oneof([sparql,turtle])
                  % ?Integer:integer
    integerLiteral//1, % ?Integer:integer
    'INTEGER_NEGATIVE'//1, % ?Integer:integer
    'INTEGER_POSITIVE'//1 % ?Integer:integer
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





%! 'DECIMAL'(+Language:oneof([sparql,turtle]), ?Decimal:float)// .
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



%! decimalLiteral(?Decimal:float)// .
% ```bnf
% decimalLiteral ::= ['+' | '-'] digits '.' digits
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
% @compat Different from 'DECIMAL'//2 (SPARQL, Turtle) in that
%         (1) at least one digit must appear before the dot,
%         (2) at least one digit must appear after the dot.

decimalLiteral(N) -->
  {var(N)}, !,
  '?'(sign, Sg, [empty1(1)]),
  digits(L1),
  ".",
  digits(L2),
  {
    weights_decimal(L1, IntegerPart),
    weights_decimal(L2, FractionalPart),
    number_integer_parts(N0, IntegerPart, FractionalPart),
    N is copysign(N0, Sg)
  }.
decimalLiteral(N) -->
  sign(N),
  {
    N0 is abs(N),
    number_integer_parts(N0, IntegerPart, FractionalPart),
    weights_decimal(L1, IntegerPart),
    weights_decimal(L2, FractionalPart)
  },
  digits(L1),
  ".",
  digits(L2).



%! 'DECIMAL_NEGATIVE'(?Decimal:float)// .
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



%! 'DECIMAL_POSITIVE'(?Decimal:float)// .
% ```bnf
% DECIMAL_POSITIVE ::= '+' INTEGER
% ```
%
% @compat SPARQL 1.0 [81]
% @compat SPARQL 1.1 Query [149]

'DECIMAL_POSITIVE'(N) -->
  "+",
  'DECIMAL'(N).



%! digit(?Digit:between(0,9))// .
% ```bnf
% digit ::= zero | nonZero
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

digit(N) --> zero(_, N).
digit(N) --> nonZero(N).



%! digits(?Digits:list(between(0,9)))// .
% ```bnf
% digits ::= digit { digit }
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

digits(L) -->
  '+'(digit, L, []).



%! 'DOUBLE'(+Language:oneof([sparql,turtle]), ?Double:float)// .
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

'DOUBLE'(sparql, N) --> double(N).
'DOUBLE'(turtle, N) --> signed_double(N).



%! 'DOUBLE_POSITIVE'(?Double:float)// .
% ```bnf
% DOUBLE_POSITIVE ::= '+' DOUBLE
% ```
%
% @compat SPARQL 1.0 [82]
% @compat SPARQL 1.1 Query [151]

'DOUBLE_POSITIVE'(N) -->
  "+",
  'DOUBLE'(N).



%! 'DOUBLE_NEGATIVE'(?Double:float)// .
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



%! exponent(?Value:float)// .
% ```bnf
% exponent ::= ('e' | 'E') ['+' | '-'] digits
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)



%! floatingPointLiteral(?Float:float)// .
% ```bnf
% floatingPointLiteral ::= [ '+' | '-']
%                          ( digits ['.'digits] [exponent]
%                          | '.' digits [exponent])
%                          ( 'f' | 'F' )
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
% @compat This is different from 'DOUBLE'//2 (SPARQL, Turtle) where
%         (1) the exponent is required,
%         (2) where the dot is allowed even if it is not followed by
%             any digit.

floatingPointLiteral(N) -->
  signed_float(N),
  f.



%! 'INTEGER'(?Language:oneof([sparql,turtle]), ?Integer:integer)// .
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



%! integerLiteral(?Integer:integer)// .
% ```bnf
% integerLiteral ::= ['+' | '-'] digits
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

integerLiteral(N) -->
  'INTEGER'(N).



%! 'INTEGER_POSITIVE'(?Integer:integer)// .
% ```bnf
% INTEGER_POSITIVE ::= '+' INTEGER
% ```
%
% @compat SPARQL 1.0 [80]
% @compat SPARQL 1.1 Query [149]

'INTEGER_POSITIVE'(N) -->
  "+",
  'INTEGER'(sparql, N).



%! 'INTEGER_NEGATIVE'(?Integer:integer)// .
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



%! nonNegativeInteger(?Integer:nonneg)// .
% ```bnf
% nonNegativeInteger ::= zero | positiveInteger
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

nonNegativeInteger(N) --> zero(_, N).
nonNegativeInteger(N) --> positiveInteger(N).



%! positiveInteger(?Integer:positive_integer)// .
% ```bnf
% positiveInteger ::= nonZero { digit }
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

positiveInteger(N) -->
  {var(N)}, !,
  nonZero(H),
  '*'(digit, T, []),
  {weights_decimal([H|T], N)}.
positiveInteger(N) -->
  {weights_decimal([H|T], N)},
  nonZero(H),
  '*'(digit, T, []).



%! nonZero(?Digit:between(1,9))// .
% ```bnf
% nonZero := '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

nonZero(Weight) --> between_digit(1, 9, Weight).



%! zero(?Code:code, ?Digit:between(0,0))// .
% ```bnf
% zero := '0'
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
