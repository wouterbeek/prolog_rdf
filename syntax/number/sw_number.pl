:- module(
  sw_number,
  [
    'DECIMAL'//1, % ?Value:float
    'DECIMAL_NEGATIVE'//1, % ?Value:float
    'DECIMAL_POSITIVE'//1, % ?Value:float
    'DOUBLE'//1, % ?Value:float
    'EXPONENT'//1, % ?Value:integer
    'INTEGER'//1 % ?Value:integer
  ]
).

/** <module> SW grammar: Numbers

Grammar rules for numbers as defined by Semantic Web standards.

---

@author Wouter Beek
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
% @compat SPARQL 1.1 Query [147]
% @compat Turtle 1.1 [20]

'DECIMAL'(sparql, N) --> number(N).
'DECIMAL'(turtle, N) --> signed_number(N).



%! 'DECIMAL_NEGATIVE'(?Value:float)// .
% ```bnf
% DECIMAL_NEGATIVE ::= '-' DECIMAL
% ```
%
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
% @compat SPARQL 1.1 Query [149]

'DECIMAL_POSITIVE'(N) -->
  "+",
  (   {var(N)}
  -> 'DECIMAL'(N0),
      {N is copysign(N0, 1)}
  ;   {N0 is abs(N)},
      'DECIMAL'(N0)
  ).



%! 'DOUBLE'(+Language:oneof([sparql,turtle]), ?Value:float)// .
% ```ebnf
% DOUBLE ::= [+-]?
%            ( [0-9]+ '.' [0-9]* EXPONENT
%            | '.' [0-9]+ EXPONENT
%	           | [0-9]+ EXPONENT
%            )
% ```
%
% @compat Turtle 1.1 [21]
% @tbd How to write the exponent in the generative case?

'DOUBLE'(N) -->
  {nonvar(N)},
  sign(N),
  {AbsN is abs(N)},
  (   % Integer / no fractional.
      {integer(AbsN)}
  ->  integer(AbsN)
  ;   % Float with zero integer part, i.e. only fractional.
      {float_integer_part(AbsN) =:= 0}
  ->  ".",
      {fractional_integer(AbsN, Frac)},
      integer(Frac)
  ;   % Float with non-zero integer part.
      float(AbsN)
  ).
%  'EXPONENT'(Exp).
'DOUBLE'(N) -->
  {var(N)},
  '?'(sign, Sign, [empty1(1)]),
  (   '+'(decimal_digit, IntegerPart, [convert1(weights_decimal)]),
      ".",
      '*'(decimal_digit, FractionalPart, [convert(weights_decimal)])
  ;   ".",
      '+'(decimal_digit, FractionalPart, [convert(weights_decimal)]),
      {IntegerPart = 0}
  ;   '+'(decimal_digit, IntegerPart, [convert1(weights_decimal)]),
      {FractionalPart = 0}
  ),
  'EXPONENT'(Exp),
  {
    number_integer_parts(AbsN, IntegerPart, FractionalPart),
    N is copysign(AbsN,Sign) * 10 ** Exp
  }.



%! 'EXPONENT'(?Value:float)// .
% ~~~{.ebnf}
% EXPONENT ::= [eE] [+-]? [0-9]+
% ~~~
%
% @compat SPARQL 1.0 [154]
% @compat SPARQL 1.1 Query [155]
% @compat Turtle 1.1 [154s]

'EXPONENT'(N) -->
  exponent_sign,
  (   {nonvar(N)}
  ->  sign(N),
      {AbsN is abs(N)},
      integer(AbsN)
  ;   '?'(sign, Sign, [empty1(1)]),
      integer(AbsN),
      {N is copysign(Sign,AbsN)}
  ).



%! 'INTEGER'(?Value:integer)// .
% ```ebnf
% INTEGER ::= [+-]? [0-9]+
% ```
%
% @compat Turtle 1.1 [19]

'INTEGER'(N) -->
  {nonvar(N)},
  sign(N),
  {AbsN is abs(N)},
  integer(AbsN).
'INTEGER'(N) -->
  {var(N)},
  '?'(sign, Sign, [empty1(1)]),
  '+'(decimal_digit, AbsN, [convert1(weights_decimal)]),
  {N is copysign(AbsN,Sign)}.

