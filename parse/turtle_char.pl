:- module(
  turtle_char,
  [
    'IRIREF'//1, % ?Value:float
    'UCHAR'//1 % ?Code:code
  ]
).
:- reexport(
  sparql_char,
  [
    'ECHAR'//1,
    'HEX'//2,
    'PERCENT'//1,
    'PLX'//1,
    'PN_CHARS'//1,
    'PN_CHARS_BASE'//1,
    'PN_CHARS_U'//1,
    'PN_LOCAL_ESC'//1,
    'WS'//0
  ]
).

/** <module> Turtle character

DCGs for character definitions in Turtle recommendations.

Turtle characters are a superset of SPARQL characters.

@author Wouter Beek
@version 2014/04-2014/05, 2014/09-2014/10
*/

:- use_module(math(radix)).

:- use_module(plDcg(dcg_bracket)).
:- use_module(plDcg(dcg_content)).



%! 'IRIREF'(?Codes:list(code))// .
% ```{.ebfn}
% IRIREF ::= '<' ( [^#x00-#x20<>"{}|^`\] | UCHAR )* '>'
%                  /* #x00=NULL #01, '1F=control codes #x20=space */
% ```
%
% @compat Turtle 1.1 [18].

'IRIREF'(L) -->
  bracketed(angular,
    'IRIREF_char*'(L)
  ).

'IRIREF_char*'([H|T]) -->
  'IRIREF_char'(H),
  'IRIREF_char*'(T).
'IRIREF_char*'([]) --> [].

'IRIREF_char'(_) --> [Code], {Code =< 32}, !, {fail}.
'IRIREF_char'(_) --> "<", !, {fail}.
'IRIREF_char'(_) --> ">", !, {fail}.
'IRIREF_char'(_) --> "\"", !, {fail}.
'IRIREF_char'(_) --> "{", !, {fail}.
'IRIREF_char'(_) --> "}", !, {fail}.
'IRIREF_char'(_) --> "|", !, {fail}.
'IRIREF_char'(_) --> "^", !, {fail}.
'IRIREF_char'(_) --> "\`", !, {fail}.
'IRIREF_char'(_) --> "\\", !, {fail}.
'IRIREF_char'(Code) --> 'UCHAR'(Code).
'IRIREF_char'(Code) --> [Code].



%! 'UCHAR'// .
%! 'UCHAR'(?Code:nonneg)// .
% ```{.ebnf}
% UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ```
%
% @compat Turtle 1.1 [26].

'UCHAR' -->
  'UCHAR'(_).

'UCHAR'(Code) -->
  "\\u",
  '#'(4, 'HEX', Weights, []),
  {weights_radix(Weights, Code)}.
'UCHAR'(C) -->
  "\\U",
  '#'(8, 'HEX', Weights, []),
  {weights_radix(Weights, Code)}.
