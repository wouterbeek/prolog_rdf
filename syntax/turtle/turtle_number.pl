:- module(
  turtle_number,
  [
    'DECIMAL'//1, % ?Value:float
    'DOUBLE'//1, % ?Value:float
    'EXPONENT'//1, % ?Value:integer
    'INTEGER'//1 % ?Value:integer
  ]
).

/** <module> Turtle numbers

DCGs for numbers defined in Turtle recommendations.

@author Wouter Beek
@version 2014/04-2014/05, 2014/11
*/

:- use_module(math(radix)). % Meta-option.

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_cardinal)).



%! 'DECIMAL'(?Value:float)// .
% ```ebnf
% DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
% ```
%
% @compat Turtle 1.1 [20].

'DECIMAL'(N) -->
  {nonvar(N)},
  sign(N),
  {AbsN is abs(N)},
  {number_integer_parts(AbsN, IntegerPart, FractionalPart)},
  (   {IntegerPart =:= 0}
  ->  ""
  ;   integer(IntegerPart)
  ),
  ".",
  integer(FractionalPart).
'DECIMAL'(N) -->
  {var(N)},
  '?'(sign, Sign, [empty1(1)]),
  (   integer(IntegerPart)
  ;   {IntegerPart = 0}
  ),
  ".",
  integer(FractionalPart),
  {number_integer_parts(AbsN, IntegerPart, FractionalPart)},
  {N is copysign(AbsN,Sign)}.



%! 'DOUBLE'(?Value:float)// .
% ```ebnf
% DOUBLE ::= [+-]?
%            ( [0-9]+ '.' [0-9]* EXPONENT
%            | '.' [0-9]+ EXPONENT
%	           | [0-9]+ EXPONENT
%            )
% ```
%
% @compat Turtle 1.1 [21].
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
% @compat SPARQL 1.0 [154].
% @compat SPARQL 1.1 Query [155].
% @compat Turtle 1.1 [154s].

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
% @compat Turtle 1.1 [19].

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

