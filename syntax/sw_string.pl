:- module(
  sw_string,
  [
    'String'//2 % +Language:oneof([sparql,turtle])
                % ?String:atom
  ]
).

/** <module> SW grammar: Strings

Grammar rules for strings in Semantic Web standards.

---

@author Wouter Beek
@compat SPARQL 1.0
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2014/04-2014/05, 2014/08, 2014/11-2014/12
*/

:- use_module(generics(code_ext)). % Meta-option.

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_meta)).
:- use_module(plDcg(dcg_quote)).

:- use_module(plRdf(syntax/sw_char)).





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

'STRING_LITERAL_LONG1'(String) -->
  'STRING_LITERAL_LONG'(sparql, single_quote, String).



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

'STRING_LITERAL_LONG2'(String) -->
  'STRING_LITERAL_LONG'(sparql, double_quote, String).



%! 'STRING_LITERAL_QUOTE'(?String:atom)// .
% ```ebnf
% STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
%                          /* #x22=" #x5C=\ #xA=new line
%                             #xD=carriage return */
% ```
%
% @compat Turtle 1.1 [22]
% @compat This different from SPARQL 1.0 [88] and SPARQL 1.1 [157]
%         in that escape sequences for Unicode characters are allowed here.

'STRING_LITERAL_QUOTE'(String) -->
  'STRING_LITERAL'(turtle, double_quote, String).



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

'STRING_LITERAL_SINGLE_QUOTE'(String) -->
  'STRING_LITERAL'(turtle, single_quote, String).



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

'STRING_LITERAL_LONG_SINGLE_QUOTE'(String) -->
  'STRING_LITERAL_LONG'(turtle, single_quote, String).



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

'STRING_LITERAL_LONG_QUOTE'(String) -->
  'STRING_LITERAL_LONG'(turtle, double_quote, String).



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

'STRING_LITERAL1'(String) -->
  'STRING_LITERAL'(sparql, single_quote, String).



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

'STRING_LITERAL2'(String) -->
  'STRING_LITERAL'(sparql, double_quote, String).



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
% @compat SPARQL 1.0 [66]
% @compat SPARQL 1.1 Query [135]
% @compat Turtle 1.1 [17]

'String'(sparql, Literal) -->
  'STRING_LITERAL1'(Literal).
'String'(sparql, Literal) -->
  'STRING_LITERAL2'(Literal).
'String'(sparql, Literal) -->
  'STRING_LITERAL_LONG1'(Literal).
'String'(sparql, Literal) -->
  'STRING_LITERAL_LONG2'(Literal).
'String'(turtle, String) -->
  'STRING_LITERAL_QUOTE'(String).
'String'(turtle, String) -->
  'STRING_LITERAL_SINGLE_QUOTE'(String).
'String'(turtle, String) -->
  'STRING_LITERAL_LONG_SINGLE_QUOTE'(String).
'String'(turtle, String) -->
  'STRING_LITERAL_LONG_QUOTE'(String).





% HELPERS

%! 'STRING_LITERAL'(
%!   +Language:oneof([sparql,turtle]),
%!   +Quote:oneof([double_quote,single_quote]),
%!   ?String:atom
%! )// .

'STRING_LITERAL'(Language, Quote, String) -->
  quoted(Quote,
    '*'(
      'STRING_LITERAL_char'(Language, Quote),
      String,
      [convert1(codes_atom)]
    )
  ).



%! 'STRING_LITERAL_LONG_char'(
%!   +Language:oneof([sparql,turtle]),
%!   +Quote:oneof([double_quote,single_quote]),
%!   ?String:atom
%! )// .

'STRING_LITERAL_LONG'(Language, Quote, String):-
  quoted(3, Quote,
    '*'(
      'STRING_LITERAL_LONG_char'(Language, Quote),
      String,
      [convert1(codes_atom)]
    )
  ).



%! 'STRING_LITERAL_LONG_char'(
%!   +Language:oneof([sparql,turtle]),
%!   :Exclude,
%!   ?Code:code
%! )// .

'STRING_LITERAL_LONG_char'(_, _, Code) --> "\\", !, {fail}.
'STRING_LITERAL_LONG_char'(_, Exclude, _) -->
  Exclude, Exclude, Exclude, !, {fail}.
'STRING_LITERAL_LONG_char'(_, _, Code) --> 'ECHAR'(Code).
'STRING_LITERAL_LONG_char'(turtle, _, Code) --> 'UCHAR'(Code).


%! 'STRING_LITERAL_char'(
%!   +Language:oneof([sparql,turtle]),
%!   :Exclude,
%!   ?Code:code
%! )// .

'STRING_LITERAL_char'(_, Exclude, _) --> Exclude, !, {fail}.
'STRING_LITERAL_char'(_, _, Code) --> 'ECHAR'(Code).
'STRING_LITERAL_char'(_, _, _) --> code_radix(hex('5C')), !, {fail}.
'STRING_LITERAL_char'(_, _, _) --> code_radix(hex('A')), !, {fail}.
'STRING_LITERAL_char'(_, _, _) --> code_radix(hex('D')), !, {fail}.
'STRING_LITERAL_char'(turtle, _, Code) --> 'UCHAR'(Code).
