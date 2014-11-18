:- module(
  turtle_char,
  [
    'ECHAR'//1, % ?Code:code
    'IRIREF'//1, % ?Value:float
    'PERCENT'//1, % ?Code:code
    'PLX'//1, % ?Code:code
    'PN_CHARS'//1, % ?Code:code
    'PN_CHARS_BASE'//1, % ?Code:code
    'PN_CHARS_U'//1, % ?Code:code
    'PN_LOCAL_ESC'//1, % ?Code:code
    'UCHAR'//1, % ?Code:code
    'WS'//0 % ?Code:code
  ]
).

/** <module> Turtle syntax: Character

DCGs for character definitions in Turtle recommendations.

Turtle characters are a superset of SPARQL characters.

@author Wouter Beek
@version 2014/04-2014/05, 2014/09-2014/11
*/

:- use_module(math(radix)).

:- use_module(plDcg(abnf_core_rules)).
:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_bracket)).
:- use_module(plDcg(dcg_content)).



%! 'ECHAR'(?Code:code)// .
% ~~~{.ebnf}
% ECHAR ::= '\' [tbnrf"'\]
% ~~~
%
% @compat SPARQL 1.0 [159].
% @compat SPARQL 1.1 Query [160].
% @compat Turtle 1.1 [159s].

'ECHAR'(Code) -->
  "\\",
  'ECHAR_char'(Code).

'ECHAR_char'(Code) --> "t", {horizontal_tab(Code, _, _)}.
'ECHAR_char'(Code) --> "b", {bell(Code, _, _)}.
'ECHAR_char'(Code) --> "n", {line_feed(Code, _, _)}.
'ECHAR_char'(Code) --> "r", {carriage_return(Code, _, _)}.
'ECHAR_char'(Code) --> "f", {form_feed(Code, _, _)}.
'ECHAR_char'(Code) --> double_quote(Code).
'ECHAR_char'(Code) --> apostrophe(Code).
'ECHAR_char'(Code) --> backslash(Code).



%! 'IRIREF'(?Rdf:atom)// .
% ```ebfn
% IRIREF ::= '<' ( [^#x00-#x20<>"{}|^`\] | UCHAR )* '>'
%            /* #x00=NULL #01, '1F=control codes #x20=space */
% ```
%
% @compat Turtle 1.1 [18].

'IRIREF'(Ref) -->
  bracketed(angular,
    '*'('IRIREF_char', Ref, [convert1(codes_atom)])
  ).

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



%! 'PERCENT'(?Weight:between(0,256))// .
% ~~~{.ebnf}
% PERCENT ::= '%' HEX HEX
% ~~~
%
% @compat SPARQL 1.0 [170].
% @compat SPARQL Query 1.1 [171].
% @compat Turtle 1.1 [170s].

'PERCENT'(Weight) -->
  {between(0, 256, Weight)},
  {Weight1 is Weight // 16},
  {Weight2 is Weight rem 16},
  'PERCENT'(Weight1, Weight2).
'PERCENT'(Weight) -->
  {var(Weight)},
  'PERCENT'(Weight1, Weight2),
  {Weight is Weight1 * 16 + Weight2}.

'PERCENT'(Weight1, Weight2) -->
  "%",
  'HEX'(Weight1),
  'HEX'(Weight2).



%! 'PLX'(?Code:code)// .
% ~~~{.ebnf}
% PLX ::= PERCENT | PN_LOCAL_ESC
% ~~~
%
% @compat SPARQL 1.0 [169].
% @compat SPARQL 1.1 Query [170].
% @compat Turtle 1.1 [169s].

'PLX'(Code) --> 'PERCENT'(Code).
'PLX'(Code) --> 'PN_LOCAL_ESC'(Code).



%! 'PN_CHARS'(?Code:code)// .
% ~~~{.ebnf}
% PN_CHARS ::= PN_CHARS_U |
%              '-' |
%              [0-9] |
%              #xB7 |
%              [#x300-#x36F] |
%              [#x203F-#x2040]
% ~~~
%
% @compat SPARQL 1.0 [166].
% @compat SPARQL 1.1 Query [167].
% @compat Turtle 1.1 [166s].

'PN_CHARS'(Code) --> 'PN_CHARS_U'(Code).
'PN_CHARS'(Code) --> hyphen_minus(Code).
'PN_CHARS'(Code) --> decimal_digit(Code).
'PN_CHARS'(Code) --> hex_code('00B7', Code).
'PN_CHARS'(Code) --> between_hex('0300', '036F', Code).
'PN_CHARS'(Code) --> between_hex('203F', '2040', Code).



%! 'PN_CHARS_BASE'(?Code:code)// .
% ~~~{.ebnf}
% PN_CHARS_BASE ::=   [A-Z]
%                   | [a-z]
%                   | [#xC0-#xD6]
%                   | [#xD8-#xF6]
%                   | [#xF8-#x2FF]
%                   | [#x370-#x37D]
%                   | [#x37F-#x1FFF]
%                   | [#x200C-#x200D]
%                   | [#x2070-#x218F]
%                   | [#x2C00-#x2FEF]
%                   | [#x3001-#xD7FF]
%                   | [#xF900-#xFDCF]
%                   | [#xFDF0-#xFFFD]
%                   | [#x10000-#xEFFFF]
% ~~~
%
% @compat SPARQL 1.0 [163].
% @compat SPARQL 1.1 Query [164].
% @compat Turtle 1.1 [163s].
% @see Almost the same as XML 1.0.5 and 1.1.2,
%      but without colon and underscore.

% [A-Z] and [a-z]
'PN_CHARS_BASE'(Code) --> ascii_letter(Code).
% #xC0-#xD6
'PN_CHARS_BASE'(Code) --> between_hex('C0', 'D6', Code).
% #xD8-#xF6
'PN_CHARS_BASE'(Code) --> between_hex('D8', 'F6', Code).
% #xF8-#x2FF
'PN_CHARS_BASE'(Code) --> between_hex('F8', '2FF', Code).
% #x370-#x37D
'PN_CHARS_BASE'(Code) --> between_hex('370', '37D', Code).
% #x37F-#x1FFF
'PN_CHARS_BASE'(Code) --> between_hex('37F', '1FFF', Code).
% #x200C-#x200D
'PN_CHARS_BASE'(Code) --> zero_width_non_joiner(Code).
'PN_CHARS_BASE'(Code) --> zero_width_joiner(Code).
% #x2070-#x218F
'PN_CHARS_BASE'(Code) --> between_hex('2070', '218F', Code).
% #x2C00-#x2FEF
'PN_CHARS_BASE'(Code) --> between_hex('2C00', '2FEF', Code).
% #x3001-#xD7FF
'PN_CHARS_BASE'(Code) --> between_hex('3001', 'D7FF', Code).
% #xF900-#xFDCF
'PN_CHARS_BASE'(Code) --> between_hex('F900', 'FDCF', Code).
% #xFDF0-#xFFFD
'PN_CHARS_BASE'(Code) --> between_hex('FDF0', 'FFFD', Code).
% #x10000-#xEFFFF
'PN_CHARS_BASE'(Code) --> between_hex('10000', 'EFFFF', Code).



%! 'PN_CHARS_U'(?Code:code)// .
% ~~~{.ebnf}
% PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ~~~
%
% @compat SPARQL 1.0 [164].
% @compat SPARQL 1.1 Query [165].
% @compat Turtle 1.1 [164s].

'PN_CHARS_U'(Code) --> 'PN_CHARS_BASE'(Code).
'PN_CHARS_U'(Code) --> underscore(Code).



%! 'PN_LOCAL_ESC'(?Code:code)// .
% ~~~{.ebnf}
% PN_LOCAL_ESC ::= '\'
%                  ( '_' | '~' | '.' | '-' | '!' | '$' | '&' |
%                    "'" | '(' | ')' | '*' | '+' | ',' | ';' |
%                    '=' | '/' | '?' | '#' | '@' | '%'
%                  )
% ~~~
%
% @compat SPARQL 1.0 [172].
% @compat SPARQL 1.1 Query [173].
% @compat Turtle 1.1 [172s].

'PN_LOCAL_ESC'(Code) -->
  "\\",
  'PN_LOCAL_ESC_char'(Code).

'PN_LOCAL_ESC_char'(Code) --> underscore(Code).
'PN_LOCAL_ESC_char'(Code) --> tilde(Code).
'PN_LOCAL_ESC_char'(Code) --> dot(Code).
'PN_LOCAL_ESC_char'(Code) --> hyphen_minus(Code).
'PN_LOCAL_ESC_char'(Code) --> exclamation_mark(Code).
'PN_LOCAL_ESC_char'(Code) --> dollar_sign(Code).
'PN_LOCAL_ESC_char'(Code) --> ampersand(Code).
'PN_LOCAL_ESC_char'(Code) --> apostrophe(Code).
'PN_LOCAL_ESC_char'(Code) --> opening_round_bracket(Code).
'PN_LOCAL_ESC_char'(Code) --> closing_round_bracket(Code).
'PN_LOCAL_ESC_char'(Code) --> asterisk(Code).
'PN_LOCAL_ESC_char'(Code) --> plus_sign(Code).
'PN_LOCAL_ESC_char'(Code) --> comma(Code).
'PN_LOCAL_ESC_char'(Code) --> semi_colon(Code).
'PN_LOCAL_ESC_char'(Code) --> equals_sign(Code).
'PN_LOCAL_ESC_char'(Code) --> slash(Code).
'PN_LOCAL_ESC_char'(Code) --> question_mark(Code).
'PN_LOCAL_ESC_char'(Code) --> number_sign(Code).
'PN_LOCAL_ESC_char'(Code) --> at_sign(Code).
'PN_LOCAL_ESC_char'(Code) --> percent_sign(Code).



%! 'UCHAR'// .
%! 'UCHAR'(?Code:nonneg)// .
% ```ebnf
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
'UCHAR'(Code) -->
  "\\U",
  '#'(8, 'HEX', Weights, []),
  {weights_radix(Weights, Code)}.



%! 'WS'// .
% ~~~{.ebnf}
% WS ::= #x20 | #x9 | #xD | #xA
%        /* #x20=space #x9=character tabulation
%           #xD=carriage return #xA=new line */
% ~~~
%
% @compat SPARQL 1.0 [161].
% @compat SPARQL 1.1 Query [162].
% @compat Turtle 1.1 [161s].

'WS' --> 'WSP'.
'WS' --> carriage_return.
'WS' --> line_feed.

