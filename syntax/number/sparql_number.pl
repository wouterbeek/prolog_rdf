:- module(
  sparql_number,
  [
    'DOUBLE'//1, % ?Value:float
    'DOUBLE_NEGATIVE'//1, % ?Value:float
    'DOUBLE_POSITIVE'//1, % ?Value:float
    'INTEGER'//1, % ?Value:integer
    'INTEGER_POSITIVE'//1, % ?Value:integer
    'INTEGER_NEGATIVE'//1 % ?Value:integer
  ]
).

/** <module> SPARQL number

DCGs for number-denoting expressions defined in SPARQL recommendations.

@author Wouter Beek
@version 2014/04-2014/05, 2014/11
*/

:- use_module(generics(typecheck)).
:- use_module(math(float_ext)).

:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_cardinal)).

:- use_module(plRdf(syntax/turtle/turtle_number)).



%! 'DOUBLE'(?Value:float)// .
% ```bnf
% DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT |
%            '.' ([0-9])+ EXPONENT |
%            ([0-9])+ EXPONENT
% ```
%
% @compat SPARQL 1.1 Query [148].
% @tbd Add (+) case.

'DOUBLE'(N) -->
  {nonvar(N)},
  ('DOUBLE_1'(N1) ; 'DOUBLE_2'(N1) ; 'DOUBLE_3'(N1)),
  'EXPONENT'(N2),
  {N is N1 * N2}.

'DOUBLE_1'(N) -->
  {nonvar(N)},
  {number_integer_parts(N, N1 N2)},
  integer(N1),
  `.`,
  'integer?'(N2).
'DOUBLE_1'(N) -->
  {var(N)},
  integer(N1),
  `.`,
  'integer?'(N2),
  {number_integer_parts(N, N1 N2)}.

'DOUBLE_2'(N) -->
  {nonvar(N)},
  {number_integer_parts(N, 0, N1)},
  `.`,
  integer(N1).
'DOUBLE_2'(N) -->
  {var(N)},
  `.`,
  integer(N1),
  {number_integer_parts(N, 0, N1)}.

'DOUBLE_3'(N) -->
  integer(N).



%! 'DOUBLE_POSITIVE'(?Value:float)// .
% ```bnf
% DOUBLE_POSITIVE ::= '+' DOUBLE
% ```
%
% @compat SPARQL 1.1 Query [151].

'DOUBLE_POSITIVE'(N) -->
  {positive_float(N)},
  `+`,
  'DOUBLE'(N).
'DOUBLE_POSITIVE'(N) -->
  {var(N)},
  `+`,
  'DOUBLE'(N1),
  {positive_float(N1)}.



%! 'DOUBLE_NEGATIVE'(?Value:float)// .
% ```bnf
% DOUBLE_NEGATIVE ::= '-' DOUBLE
% ```
%
% @compat SPARQL 1.1 Query [154].

'DOUBLE_NEGATIVE'(N) -->
  {negative_float(N)},
  `-`,
  {N1 is abs(N)}.
  'DOUBLE'(N1),
'DOUBLE_NEGATIVE'(N) -->
  {var(N)},
  `-`,
  'DOUBLE'(N1),
  {N is -1 * N1}.



%! 'INTEGER'(?Value:integer)// .
% ```bnf
% INTEGER ::= [0-9]+
% ```
%
% @compat SPARQL 1.1 Query [146].

'INTEGER'(N) -->
  integer(N).



%! 'INTEGER_POSITIVE'(?Value:integer)// .
% ```bnf
% INTEGER_POSITIVE ::= '+' INTEGER
% ```
%
% @compat SPARQL 1.1 Query [149].

'INTEGER_POSITIVE'(N) -->
  {positive_integer(N)},
  `+`,
  'INTEGER'(N).
'INTEGER_POSITIVE'(N) -->
  {var(N)},
  `+`,
  'INTEGER'(N1),
  {positive_integer(N1)}.



%! 'INTEGER_NEGATIVE'(?Value:integer)// .
% ```bnf
% INTEGER_NEGATIVE ::= '-' INTEGER
% ```
%
% @compat SPARQL 1.1 Query [152].

'INTEGER_NEGATIVE'(N) -->
  {negative_integer(N)},
  `-`,
  {N1 is abs(N)}.
  'INTEGER'(N1),
'INTEGER_NEGATIVE'(N) -->
  {var(N)},
  `-`,
  'INTEGER'(N1),
  {N is -1 * N1}.
