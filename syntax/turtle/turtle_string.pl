:- module(
  turtle_string,
  [
    'STRING_LITERAL_QUOTE'//1, % ?String:atom
    'STRING_LITERAL_SINGLE_QUOTE'//1, % ?String:atom
    'STRING_LITERAL_LONG_QUOTE'//1, % ?String:atom
    'STRING_LITERAL_LONG_SINGLE_QUOTE'//1 % ?String:atom
  ]
).

/** <module> Turtle syntax: String

Grammar for strings in Turtle.

@author Wouter Beek
@version 2014/04-2014/05, 2014/08, 2014/11
*/

:- use_module(generics(code_ext)). % Meta-option.

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_meta)).
:- use_module(plDcg(dcg_quote)).



%! 'String'(?String:atom)// .
% ```ebnf
% String ::=   STRING_LITERAL_QUOTE
%            | STRING_LITERAL_SINGLE_QUOTE
%            | STRING_LITERAL_LONG_SINGLE_QUOTE
%            | STRING_LITERAL_LONG_QUOTE
% ```
%
% @compat Turtle 1.1 [17].

'String'(String) -->
  'STRING_LITERAL_QUOTE'(String).
'String'(String) -->
  'STRING_LITERAL_SINGLE_QUOTE'(String).
'String'(String) -->
  'STRING_LITERAL_LONG_SINGLE_QUOTE'(String).
'String'(String) -->
  'STRING_LITERAL_LONG_QUOTE'(String).



%! 'STRING_LITERAL_QUOTE'(?String:atom)// .
% ```ebnf
% STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
%                          /* #x22=" #x5C=\ #xA=new line
%                             #xD=carriage return */
% ```
%
% @compat Turtle 1.1 [22].

'STRING_LITERAL_QUOTE'(String) -->
  quoted(
    '*'('STRING_LITERAL_QUOTE_char', String, [convert1(codes_atom)])
  ).

'STRING_LITERAL_QUOTE_char'(Code) --> 'ECHAR'(Code).
'STRING_LITERAL_QUOTE_char'(Code) --> 'UCHAR'(Code).
'STRING_LITERAL_QUOTE_char'(_) --> code_radix(hex('22')), !, {fail}.
'STRING_LITERAL_QUOTE_char'(_) --> code_radix(hex('5C')), !, {fail}.
'STRING_LITERAL_QUOTE_char'(_) --> code_radix(hex('A')), !, {fail}.
'STRING_LITERAL_QUOTE_char'(_) --> code_radix(hex('D')), !, {fail}.
'STRING_LITERAL_QUOTE_char'(Code) --> [Code].



%! 'STRING_LITERAL_SINGLE_QUOTE'(?String:atom)// .
% ```ebnf
% STRING_LITERAL_SINGLE_QUOTE ::= "'"
%                                 ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)*
%                                 "'"
%                                 /* #x27=' #x5C=\ #xA=new line
%                                    #xD=carriage return */
% ```
%
% @compat Turtle 1.1 [23].

'STRING_LITERAL_SINGLE_QUOTE'(String) -->
  quoted(1, single_quote,
    '*'('STRING_LITERAL_SINGLE_QUOTE_char', String, [convert1(codes_atom)])
  ).

'STRING_LITERAL_SINGLE_QUOTE_char'(Code) --> 'ECHAR'(Code).
'STRING_LITERAL_SINGLE_QUOTE_char'(Code) --> 'UCHAR'(Code).
'STRING_LITERAL_SINGLE_QUOTE_char'(_) --> hex('27'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char'(_) --> hex('5C'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char'(_) --> hex('A'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char'(_) --> hex('D'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char'(C) --> [C].



%! 'STRING_LITERAL_LONG_SINGLE_QUOTE'(?String:atom)// .
% ```ebnf
% STRING_LITERAL_LONG_SINGLE_QUOTE ::= "'''"
%                                      (
%                                        ("'" | "''")?
%                                        ([^'\] | ECHAR | UCHAR)
%                                      )*
%                                      "'''"
% ```
%
% @compat Turtle 1.1 [24].

'STRING_LITERAL_LONG_SINGLE_QUOTE'(String) -->
  quoted(3, single_quote,
    ("" ; "'" ; "''"),
    '*'('STRING_LITERAL_LONG_SINGLE_QUOTE_char', String, [convert1(codes_atom)])
  )).

'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'(Code) --> 'ECHAR'(Code).
'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'(Code) --> 'UCHAR'(Code).
'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'(_) --> "\\", !, {fail}.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'(_) --> "'''", !, {fail}.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(Code) --> [Code].



%! 'STRING_LITERAL_LONG_QUOTE'(?String:atom)// .
% ```ebnf
% STRING_LITERAL_LONG_QUOTE ::= '"""'
%                               (
%                                 ('"' | '""')?
%                                 ([^"\] | ECHAR | UCHAR)
%                               )*
%                               '"""'
% ```
%
% @compat Turtle 1.1 [25].

'STRING_LITERAL_LONG_QUOTE'(String) -->
  quoted(3, double_quote,
    '*'('STRING_LITERAL_LONG_QUOTE_char', String, [convert1(codes_atom)])
  ).

'STRING_LITERAL_LONG_QUOTE_char'(Code) --> 'ECHAR'(Code).
'STRING_LITERAL_LONG_QUOTE_char'(Code) --> 'UCHAR'(Code).
'STRING_LITERAL_LONG_QUOTE_char'(_) --> "\"\"\"", !, {fail}.
'STRING_LITERAL_LONG_QUOTE_char'(_) --> "\\", !, {fail}.
'STRING_LITERAL_LONG_QUOTE_char'(Code) --> [Code].

