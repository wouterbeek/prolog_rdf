:- module(
  turtle_string,
  [
    'STRING_LITERAL_QUOTE'//1, % ?Codes:list(code)
    'STRING_LITERAL_SINGLE_QUOTE'//1, % ?Codes:list(code)
    'STRING_LITERAL_LONG_QUOTE'//1, % ?Codes:list(code)
    'STRING_LITERAL_LONG_SINGLE_QUOTE'//1 % ?Codes:list(code)
  ]
).

/** <module> Turtle String

Grammar for strings in Turtle.

@author Wouter Beek
@version 2014/04-2014/05, 2014/08
*/

:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).



%! 'String'(-Value:atom)// .
% ```ebnf
% String ::= STRING_LITERAL_QUOTE |
%            STRING_LITERAL_SINGLE_QUOTE |
%            STRING_LITERAL_LONG_SINGLE_QUOTE |
%            STRING_LITERAL_LONG_QUOTE
% ```
%
% @compat Turtle 1.1 [17].

'String'(Value) -->
  dcg_atom_codes('STRING_LITERAL_QUOTE', Value).
'String'(Value) -->
  dcg_atom_codes('STRING_LITERAL_SINGLE_QUOTE', Value).
'String'(Value) -->
  dcg_atom_codes('STRING_LITERAL_LONG_SINGLE_QUOTE', Value).
'String'(Value) -->
  dcg_atom_codes('STRING_LITERAL_LONG_QUOTE', Value).



%! 'STRING_LITERAL_QUOTE'(?Codes:list(code))// .
% ```ebnf
% STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
%                          /* #x22=" #x5C=\ #xA=new line
%                             #xD=carriage return */
% ```
%
% @compat Turtle 1.1 [22].

'STRING_LITERAL_QUOTE'(L) -->
  quoted(double_quote,
    'STRING_LITERAL_QUOTE_char*'(L)
  ).

'STRING_LITERAL_QUOTE_char*'([H|T]) -->
  'STRING_LITERAL_QUOTE_char'(H),
  'STRING_LITERAL_QUOTE_char*'(T).
'STRING_LITERAL_QUOTE_char*'([]) --> [].

'STRING_LITERAL_QUOTE_char'(C) --> 'ECHAR'(C).
'STRING_LITERAL_QUOTE_char'(C) --> 'UCHAR'(C).
'STRING_LITERAL_QUOTE_char'(_) --> hex('22'), !, {fail}.
'STRING_LITERAL_QUOTE_char'(_) --> hex('5C'), !, {fail}.
'STRING_LITERAL_QUOTE_char'(_) --> hex('A'), !, {fail}.
'STRING_LITERAL_QUOTE_char'(_) --> hex('D'), !, {fail}.
'STRING_LITERAL_QUOTE_char'(C) --> [C].



%! 'STRING_LITERAL_SINGLE_QUOTE'(-Codes:list(code))// .
% ```ebnf
% STRING_LITERAL_SINGLE_QUOTE ::= "'"
%                                 ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)*
%                                 "'"
%                                 /* #x27=' #x5C=\ #xA=new line
%                                    #xD=carriage return */
% ```
%
% @compat Turtle 1.1 [23].

'STRING_LITERAL_SINGLE_QUOTE'(L) -->
  quoted(single_quote,
    'STRING_LITERAL_SINGLE_QUOTE_char*'(L),
  ).

'STRING_LITERAL_SINGLE_QUOTE_char*'([H|T]) -->
  'STRING_LITERAL_SINGLE_QUOTE_char'(H),
  'STRING_LITERAL_SINGLE_QUOTE_char*'(T).
'STRING_LITERAL_SINGLE_QUOTE_char*'([]) --> [].

'STRING_LITERAL_SINGLE_QUOTE_char'(C) --> 'ECHAR'(C).
'STRING_LITERAL_SINGLE_QUOTE_char'(C) --> 'UCHAR'(C).
'STRING_LITERAL_SINGLE_QUOTE_char'(_) --> hex('27'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char'(_) --> hex('5C'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char'(_) --> hex('A'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char'(_) --> hex('D'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char'(C) --> [C].



%! 'STRING_LITERAL_LONG_SINGLE_QUOTE'(-Codes:list(code))// .
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

'STRING_LITERAL_LONG_SINGLE_QUOTE'(L) -->
  quoted(triple_quote(single_quote), (
    ("" ; "'" ; "''"),
    'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'(L)
  )).

'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'(L) -->
  (
    single_quote(H1),
    single_quote(H2),
    'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(H3)
  ->
    {L = [H1,H2,H3|T]}
  ;
    single_quote(H1),
    'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(H2)
  ->
    {L = [H1,H2|T]}
  ;
    'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(H)
  ->
    {L = [H|T]}
  ),
  'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'(T).
'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'([]) --> [].

'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(C) --> 'ECHAR'(C).
'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(C) --> 'UCHAR'(C).
'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(_) --> "'", !, {fail}.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(_) --> "\\", !, {fail}.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char'(C) --> [C].



%! 'STRING_LITERAL_LONG_QUOTE'(-Codes:list(code))// .
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

'STRING_LITERAL_LONG_QUOTE'(L) -->
  quoted(triple_quote(double_quote), (
    'STRING_LITERAL_LONG_QUOTE_char*'(L),
  )).

'STRING_LITERAL_LONG_QUOTE_char*'(L) -->
  (
    double_quote(H1),
    double_quote(H2),
    'STRING_LITERAL_LONG_QUOTE_char'(H3)
  ->
    {L = [H1,H2,H3|T]}
  ;
    double_quote(H1),
    'STRING_LITERAL_LONG_QUOTE_char'(H2)
  ->
    {L = [H1,H2|T]}
  ;
    'STRING_LITERAL_LONG_QUOTE_char'(H)
  ->
    {L = [H|T]}
  ),
  'STRING_LITERAL_LONG_QUOTE_char*'(T).
'STRING_LITERAL_LONG_QUOTE_char*'([]) --> [].

'STRING_LITERAL_LONG_QUOTE_char'(C) --> 'ECHAR'(C).
'STRING_LITERAL_LONG_QUOTE_char'(C) --> 'UCHAR'(C).
'STRING_LITERAL_LONG_QUOTE_char'(_) --> "\"", !, {fail}.
'STRING_LITERAL_LONG_QUOTE_char'(_) --> "\\", !, {fail}.
'STRING_LITERAL_LONG_QUOTE_char'(C) --> [C].

