:- module(
  turtle_literal,
  [
    'STRING_LITERAL_QUOTE'//1, % ?Codes:list(code)
  ]
).

/** <module> Turtle literal

DCGs for literal expression defined in Turtle recommendations.

@author Wouter Beek
@version 2014/04-2014/05
*/

:- use_module(dcg(dcg_content)).



%! 'STRING_LITERAL_QUOTE'(?Codes:list(code))// .
% ~~~{.ebnf}
% STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
%                          /* #x22=" #x5C=\ #xA=new line
%                             #xD=carriage return */
% ~~~
%
% @compat Turtle 1.1 [22].

'STRING_LITERAL_QUOTE'(L) -->
  quoted('STRING_LITERAL_QUOTE_char*'(L)).

'STRING_LITERAL_QUOTE_char*'([H|T]) -->
  'STRING_LITERAL_QUOTE_char'(H),
  'STRING_LITERAL_QUOTE_char*'(T).
'STRING_LITERAL_QUOTE_char*'([]) --> [].

'STRING_LITERAL_QUOTE_char'(C) --> 'ECHAR'(C).
'STRING_LITERAL_QUOTE_char'(C) --> 'UCHAR'(C).
'STRING_LITERAL_QUOTE_char'(C) --> hex('22'), !, {fail}.
'STRING_LITERAL_QUOTE_char'(C) --> hex('5C'), !, {fail}.
'STRING_LITERAL_QUOTE_char'(C) --> hex('A'), !, {fail}.
'STRING_LITERAL_QUOTE_char'(C) --> hex('D'), !, {fail}.
'STRING_LITERAL_QUOTE_char'(C) --> [C].


%! 'STRING_LITERAL_SINGLE_QUOTE'// .
% ~~~{.ebnf}
% [23]   STRING_LITERAL_SINGLE_QUOTE ::= "'"
%                                        ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)*
%                                        "'"
%                                        /* #x27=' #x5C=\ #xA=new line
%                                           #xD=carriage return */
% ~~~

'STRING_LITERAL_SINGLE_QUOTE' --> `'`, 'STRING_LITERAL_SINGLE_QUOTE_char*', `'`.

'STRING_LITERAL_SINGLE_QUOTE_char*' --> 'STRING_LITERAL_SINGLE_QUOTE_char', 'STRING_LITERAL_SINGLE_QUOTE_char*'.
'STRING_LITERAL_SINGLE_QUOTE_char*' --> [].

'STRING_LITERAL_SINGLE_QUOTE_char' --> 'ECHAR'.
'STRING_LITERAL_SINGLE_QUOTE_char' --> 'UCHAR'.
'STRING_LITERAL_SINGLE_QUOTE_char' --> hex('27'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char' --> hex('5C'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char' --> hex('A'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char' --> hex('D'), !, {fail}.
'STRING_LITERAL_SINGLE_QUOTE_char' --> [_].


%! 'STRING_LITERAL_LONG_SINGLE_QUOTE'// .
% ~~~{.ebnf}
% [24]   STRING_LITERAL_LONG_SINGLE_QUOTE ::= "'''"
%                                             (("'" | "''")?
%                                              ([^'\] | ECHAR | UCHAR)
%                                             )*
%                                             "'''"
% ~~~

'STRING_LITERAL_LONG_SINGLE_QUOTE' -->
  `'''`,
  (`` ; `'` ; `''`),
  'STRING_LITERAL_LONG_SINGLE_QUOTE_char*',
  `'''`.

'STRING_LITERAL_LONG_SINGLE_QUOTE_char*' --> 'STRING_LITERAL_LONG_SINGLE_QUOTE_char', 'STRING_LITERAL_LONG_SINGLE_QUOTE_char*'.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char*' --> [].

'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> 'ECHAR'.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> 'UCHAR'.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> `'`, !, {fail}.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> `\\`, !, {fail}.
'STRING_LITERAL_LONG_SINGLE_QUOTE_char' --> [_].


%! 'STRING_LITERAL_LONG_QUOTE'// .
% ~~~{.ebnf}
% [25]   STRING_LITERAL_LONG_QUOTE ::= '"""'
%                                      (('"' | '""')? ([^"\] | ECHAR | UCHAR))*
%                                      '"""'
% ~~~

'STRING_LITERAL_LONG_QUOTE' -->
  `"""`,
  (`` ; `"` ; `""`),
  'STRING_LITERAL_LONG_QUOTE_char*',
  `"""`.

'STRING_LITERAL_LONG_QUOTE_char*' --> 'STRING_LITERAL_LONG_QUOTE_char', 'STRING_LITERAL_LONG_QUOTE_char*'.
'STRING_LITERAL_LONG_QUOTE_char*' --> [].

'STRING_LITERAL_LONG_QUOTE_char' --> 'ECHAR'.
'STRING_LITERAL_LONG_QUOTE_char' --> 'UCHAR'.
'STRING_LITERAL_LONG_QUOTE_char' --> `"`, !, {fail}.
'STRING_LITERAL_LONG_QUOTE_char' --> `\\`, !, {fail}.
'STRING_LITERAL_LONG_QUOTE_char' --> [_].

