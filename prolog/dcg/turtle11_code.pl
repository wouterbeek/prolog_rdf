:- module(
  turtle11_code,
  [
    'UCHAR'//1 % ?Code:code
  ]
).
:- reexport(library(dcg/sparql10_code), [
     'ECHAR'//1, % ?Code:code
     'PERCENT'//1, % ?Code:code
     'PLX'//1, % ?Code:code
     'WS'//0
   ]).

/** <module> Turtle 1.1: Codes

@author Wouter Beek
@compat Turtle 1.1
@version 2015/11
*/





%! 'UCHAR'(?Code:code)// .
% ```ebnf
% UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ```

'UCHAR'(C) --> "\\u", '#HEX'(4, C).
'UCHAR'(C) --> "\\U", '#HEX'(8, C).
