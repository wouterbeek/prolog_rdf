:- module(
  turtle_number,
  [
    'DECIMAL'//1, % ?Value:float
    'DOUBLE'//1, % ?Value:float
    'INTEGER'//1 % ?Value:integer
  ]
).

/** <module> Turtle numbers

DCGs for numbers defined in Turtle recommendations.

@author Wouter Beek
@version 2014/04-2014/05
*/

:- use_module(
  sparql(sparql_number),
  [
    'DECIMAL'//1 as sparql_DECIMAL,
    'DOUBLE'//1 as sparql_DOUBLE,
    'INTEGER'//1 as sparql_INTEGER,
  ]
).



%! 'DECIMAL'(?Value:float)// .
% ```{.ebnf}
% DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
% ```
%
% @compat Turtle 1.1 [20].

'DECIMAL'(N) -->
  {nonvar(N)},
  'sign?'(N),
  {N1 is abs(N)},
  sparlq_DECIMAL(N1).
'DECIMAL'(N) -->
  {var(N)},
  'sign?'(Sign),
  sparql_DECIMAL(N1),
  {N is Sign * N1}.



%! 'DOUBLE'(?Value:float)// .
% ```{.ebnf}
% DOUBLE ::= [+-]?
%            ( [0-9]+ '.' [0-9]* EXPONENT |
%              '.' [0-9]+ EXPONENT |
%              [0-9]+ EXPONENT )
% ```
%
% @compat Turtle 1.1 [21].

'DOUBLE'(N) -->
  {nonvar(N)},
  'sign?'(N),
  {N1 is abs(N)},
  sparql_DOUBLE(N1).
'DOUBLE'(N) -->
  {var(N)},
  'sign?'(Sign),
  sparql_DOUBLE(N1),
  {N is Sign * N1}.



%! 'INTEGER'(?Value:integer)// .
% ```{.ebnf}
% INTEGER ::= [+-]? [0-9]+
% ```
%
% @compat Turtle 1.1 [19].

'INTEGER'(N) -->
  {nonvar(N)},
  'sign?'(N),
  {N1 is abs(N)},
  sparql_INTEGER(N1).
'INTEGER'(N) -->
  {var(N)},
  'sign?'(Sign),
  sparql_INTEGER(N1),
  {N is Sign * N1}.

