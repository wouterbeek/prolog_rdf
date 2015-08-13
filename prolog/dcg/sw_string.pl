:- module(
  sw_string,
  [
    quotedString//1, % ?String:atom
    'String'//2, % +Language:oneof([sparql,turtle])
                 % ?String:atom
    'STRING_LITERAL_QUOTE'//1 % ?String:atom
  ]
).

/** <module> SW grammar: Strings

Grammar rules for strings in Semantic Web standards.

---

@author Wouter Beek
@compat SPARQL 1.0
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2015/08
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_quoted)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/sw_char)).

:- meta_predicate('STRING_LITERAL'(+,//,?,?,?)).
:- meta_predicate('STRING_LITERAL_char'(+,//,?,?,?)).
:- meta_predicate('STRING_LITERAL_LONG'(+,//,?,?,?)).
:- meta_predicate('STRING_LITERAL_LONG0'(+,//,?,?,?)).





%! quotedString(?String:atom)// .
% ```bnf
% quotedString := a finite sequence of characters
%                 in which " (U+22) and \ (U+5C) occur only in pairs
%                 of the form \" (U+5C, U+22) and \\ (U+5C, U+5C),
%                 enclosed in a pair of " (U+22) characters
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

quotedString(S) -->
  bracketed(dcg_atom(quotedString0, S)).

quotedString0([H1,H2|T]) -->
  backslash(H1),
  double_quote(H2), !,
  quotedString0(T).
quotedString0([H1,H2|T]) -->
  backslash(H1),
  backslash(H2), !,
  quotedString0(T).
quotedString0(_) -->
  "\\", !,
  {fail}.
quotedString0(_) -->
  "\"", !,
  {fail}.
quotedString0([H|T]) -->
  [H],
  quotedString0(T).



%! 'String'(?Language:oneof([sparql,turtle]), ?String:atom)// .
% ```ebnf
% [SPARQL]   String ::=   STRING_LITERAL1
%                       | STRING_LITERAL2
%                       | STRING_LITERAL_LONG1
%                       | STRING_LITERAL_LONG2
% [Turtle]   String ::=   STRING_LITERAL_QUOTE
%                       | STRING_LITERAL_SINGLE_QUOTE
%                       | STRING_LITERAL_LONG_SINGLE_QUOTE
%                       | STRING_LITERAL_LONG_QUOTE
% ```
%
% @compat SPARQL 1.0 [66].
% @compat SPARQL 1.1 Query [135].
% @compat Turtle 1.1 [17].

'String'(sparql, Literal) -->
  'STRING_LITERAL1'(Literal).
'String'(sparql, Literal) -->
  'STRING_LITERAL2'(Literal).
'String'(sparql, Literal) -->
  'STRING_LITERAL_LONG1'(Literal).
'String'(sparql, Literal) -->
  'STRING_LITERAL_LONG2'(Literal).
'String'(turtle, S) -->
  'STRING_LITERAL_QUOTE'(S).
'String'(turtle, S) -->
  'STRING_LITERAL_SINGLE_QUOTE'(S).
'String'(turtle, S) -->
  'STRING_LITERAL_LONG_SINGLE_QUOTE'(S).
'String'(turtle, S) -->
  'STRING_LITERAL_LONG_QUOTE'(S).



%! 'STRING_LITERAL_LONG1'(?String:atom)// .
% A literal that can contain unescaped single quotes and newlines.
%
% ```ebnf
% STRING_LITERAL_LONG1 ::= "'''"
%                          ( ( "'" | "''" )? ( [^'\] | ECHAR ) )*
%                          "'''"
% ```
%
% @compat SPARQL 1.0 [89]
% @compat SPARQL 1.1 Update [158]
% @compat This differs from Turtle 1.1 [24] in which
%         escape sequences for Unicode characters
%         are allowed to occur.

'STRING_LITERAL_LONG1'(S) -->
  'STRING_LITERAL_LONG'(sparql, single_quote, S).



%! 'STRING_LITERAL_LONG2'(?String:atom)// .
% A literal that can contain unescaped single quotes and newlines.
%
% ```ebnf
% STRING_LITERAL_LONG2 ::= '"""'
%                          ( ( '"' | '""' )? ( [^"\] | ECHAR ) )*
%                          '"""'
% ```
%
% @compat SPARQL 1.0 [90]
% @compat SPARQL 1.1 Update [159]
% @compat This differs from Turtle 1.1 [25] in which
%         escape sequences for Unicode characters
%         are allowed to occur.

'STRING_LITERAL_LONG2'(S) -->
  'STRING_LITERAL_LONG'(sparql, double_quote, S).



%! 'STRING_LITERAL_QUOTE'(?String:atom)// .
% ```ebnf
% STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
%                          /* #x22=" #x5C=\ #xA=new line
%                             #xD=carriage return */
% ```
% @compat N-Quads 1.1 [11].
% @compat N-Triples 1.1 [6].
% @compat Turtle 1.1 [22]
% @compat This different from SPARQL 1.0 [88] and SPARQL 1.1 [157]
%         in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_QUOTE'(S) -->
  'STRING_LITERAL'(turtle, double_quote, S).



%! 'STRING_LITERAL_SINGLE_QUOTE'(?String:atom)// .
% ```ebnf
% STRING_LITERAL_SINGLE_QUOTE ::= "'"
%                                 ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)*
%                                 "'"
%                                 /* #x27=' #x5C=\ #xA=new line
%                                    #xD=carriage return */
% ```
%
% @compat Turtle 1.1 [23]
% @compat This different from SPARQL 1.0 [87] and SPARQL 1.1 [156]
%         in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_SINGLE_QUOTE'(S) -->
  'STRING_LITERAL'(turtle, single_quote, S).



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
% @compat Turtle 1.1 [24]
% @compat This different from SPARQL 1.0 [89] and SPARQL 1.1 [158]
%         in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_LONG_SINGLE_QUOTE'(S) -->
  'STRING_LITERAL_LONG'(turtle, single_quote, S).



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
% @compat Turtle 1.1 [25]
% @compat This different from SPARQL 1.0 [90] and SPARQL 1.1 [159]
%         in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_LONG_QUOTE'(S) -->
  'STRING_LITERAL_LONG'(turtle, double_quote, S).



%! 'STRING_LITERAL1'(?String:atom)// .
% ```ebnf
% STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
% ```
%
% @compat SPARQL 1.0 [87]
% @compat SPARQL 1.1 Query [156]
% @compat This differs from Turtle 1.1 [23] in which
%         escape sequences for Unicode characters
%         are allowed to occur.

'STRING_LITERAL1'(S) -->
  'STRING_LITERAL'(sparql, single_quote, S).



%! 'STRING_LITERAL2'(?String:atom)// .
% ```ebnf
% STRING_LITERAL2 ::= '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
% ```
%
% @compat SPARQL 1.0 [88]
% @compat SPARQL 1.1 Query [157]
% @compat This differs from Turtle 1.1 [22] in which
%         escape sequences for Unicode characters
%         are allowed to occur.

'STRING_LITERAL2'(S) -->
  'STRING_LITERAL'(sparql, double_quote, S).





% HELPERS %

%! 'STRING_LITERAL'(
%!   +Language:oneof([sparql,turtle]),
%!   +Quote:oneof([double_quote,single_quote]),
%!   ?String:atom
%! )// .

'STRING_LITERAL'(Lang, Quote, S) -->
  quoted(Quote, dcg_atom('*'('STRING_LITERAL0'(Lang, Quote), []), S)).

%! 'STRING_LITERAL0'(+Language:oneof([sparql,turtle]), :Esc, ?Code:code)// .

'STRING_LITERAL0'(_, Esc, _) --> Esc, !, {fail}.
'STRING_LITERAL0'(_, _, C) --> 'ECHAR'(C).
'STRING_LITERAL0'(_, _, _) --> code_radix(hex('5C')), !, {fail}.
'STRING_LITERAL0'(_, _, _) --> code_radix(hex('A')), !, {fail}.
'STRING_LITERAL0'(_, _, _) --> code_radix(hex('D')), !, {fail}.
'STRING_LITERAL0'(turtle, _, C) --> 'UCHAR'(C).
'STRING_LITERAL0'(_, _, C) --> [C].



%! 'STRING_LITERAL_LONG0'(
%!   +Language:oneof([sparql,turtle]),
%!   +Quote:oneof([double_quote,single_quote]),
%!   ?String:atom
%! )// .

'STRING_LITERAL_LONG'(Lang, Quote, S) -->
  quoted(3, Quote, dcg_atom('*'('STRING_LITERAL_LONG0'(Lang, Quote), []), S)).



%! 'STRING_LITERAL_LONG0'(
%!   +Language:oneof([sparql,turtle]),
%!   :Esc,
%!   ?Code:code
%! )// .

'STRING_LITERAL_LONG0'(_, _, _) --> "\\", !, {fail}.
'STRING_LITERAL_LONG0'(_, Esc, _) --> Esc, Esc, Esc, !, {fail}.
'STRING_LITERAL_LONG0'(_, _, C) --> 'ECHAR'(C).
'STRING_LITERAL_LONG0'(turtle, _, C) --> 'UCHAR'(C).

