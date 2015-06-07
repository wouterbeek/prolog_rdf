:- module(
  sw_char,
  [
    'ECHAR'//1, % ?Code:code
    'EOL'//0,
    'PERCENT'//1, % ?Code:code
    'PLX'//1, % ?Code:code
    'PN_CHARS'//2, % ?Language:oneof([nquads,ntriples,sparql,turtle])
                   % ?Code:code
    'PN_CHARS_BASE'//1, % ?Code:code
    'PN_CHARS_U'//2, % ?Language:oneof([nquads,ntriples,sparql,turtle])
                     % ?Code:code
    'PN_LOCAL_ESC'//1, % ?Code:code
    'UCHAR'//1, % ?Code:code
    white_space//1, % ?Language:oneof([manchester,n])
    'WS'//0,
    'WS*'//0
  ]
).

/** <module> SW Grammar: Characters

Grammar rules for characters in SW standards.

Turtle characters are a superset of SPARQL characters.

---

@author Wouter Beek
@compat N-Quads 1.1
@compat N-Triples 1.1
@compat SPARQL 1.0
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2014/04-2014/05, 2014/09-2015/01
*/

:- use_module(library(dif)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_abnf_rules)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_bracket)).
:- use_module(plc(dcg/dcg_code)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_unicode), [
  zero_width_joiner//1,
  zero_width_non_joiner//1
]).
:- use_module(plc(math/radix)).





% comment// .
% Comments are maximal sequences of Unicode characters starting with a `#`
%  and not containing a line feed or a carriage return.
% Note that comments are only recognized where white space is allowed,
%  and thus not inside the above non-terminals.
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

comment -->
  "#",
  dcg_until(end_of_comment, _, [end_mode(inclusive)]).

end_of_comment --> carriage_return.
end_of_comment --> line_feed.



%! 'ECHAR'(?Code:code)// .
% ```bnf
% ECHAR ::= '\' [tbnrf"'\]
% ```
%
% @compat SPARQL 1.0 [91].
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



%! 'EOL'// .
% ```abnf
% EOL ::= [#xD#xA]+
% ```
%
% @compat N-Quads [8].
% @compat N-Triples [7].

'EOL' -->
  '+'('EOL0', []).

'EOL0' --> carriage_return.
'EOL0' --> line_feed.



%! 'PERCENT'(?Code:between(0,256))// .
% ```bnf
% PERCENT ::= '%' HEX HEX
% ```
%
% @compat SPARQL Query 1.1 [171].
% @compat Turtle 1.1 [170s].

'PERCENT'(W) -->
  {between(0, 256, W)},
  {W1 is W // 16},
  {W2 is W rem 16},
  'PERCENT'(W1, W2).
'PERCENT'(W) -->
  {var(W)},
  'PERCENT'(W1, W2),
  {W is W1 * 16 + W2}.

'PERCENT'(W1, W2) -->
  "%",
  'HEX'(W1),
  'HEX'(W2).



%! 'PLX'(?Code:code)// .
% ```bnf
% PLX ::= PERCENT | PN_LOCAL_ESC
% ```
%
% @compat SPARQL 1.1 Query [170].
% @compat Turtle 1.1 [169s].

'PLX'(Code) --> 'PERCENT'(Code).
'PLX'(Code) --> 'PN_LOCAL_ESC'(Code).



%! 'PN_CHARS'(?Language:oneof([nquads,ntriple,sparql,turtle]), ?Code:code)// .
% ```bnf
% PN_CHARS ::=   PN_CHARS_U
%              | '-'
%              | [0-9]
%              | #xB7
%              | [#x300-#x36F]
%              | [#x203F-#x2040]
% ```
%
% @compat N-Quads 1.1 [160s].
% @compat SPARQL 1.0 [98].
% @compat SPARQL 1.1 Query [167].
% @compat Turtle 1.1 [166s].

'PN_CHARS'(Lang, Code) -->
  'PN_CHARS_U'(Lang, Code).
'PN_CHARS'(_, Code) --> hyphen_minus(Code).
'PN_CHARS'(_, Code) --> decimal_digit(_, Code).
'PN_CHARS'(_, Code) --> code_radix(hex('00B7'), Code).
'PN_CHARS'(_, Code) --> between_code_radix(hex('0300'), hex('036F'), Code).
'PN_CHARS'(_, Code) --> between_code_radix(hex('203F'), hex('2040'), Code).



%! 'PN_CHARS_BASE'(?Code:code)// .
% ```bnf
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
% ```
%
% @compat N-Quads 1.1 [157s].
% @compat SPARQL 1.0 [95].
% @compat SPARQL 1.1 Query [164].
% @compat Turtle 1.1 [163s].
% @see Almost the same as XML 1.0.5 and 1.1.2,
%      but without colon and underscore.

% [A-Z] and [a-z]
'PN_CHARS_BASE'(Code) -->
  ascii_letter(Code).
% #xC0-#xD6
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('C0'), hex('D6'), Code).
% #xD8-#xF6
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('D8'), hex('F6'), Code).
% #xF8-#x2FF
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('F8'), hex('2FF'), Code).
% #x370-#x37D
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('370'), hex('37D'), Code).
% #x37F-#x1FFF
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('37F'), hex('1FFF'), Code).
% #x200C-#x200D
'PN_CHARS_BASE'(Code) -->
  zero_width_non_joiner(Code).
'PN_CHARS_BASE'(Code) -->
  zero_width_joiner(Code).
% #x2070-#x218F
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('2070'), hex('218F'), Code).
% #x2C00-#x2FEF
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('2C00'), hex('2FEF'), Code).
% #x3001-#xD7FF
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('3001'), hex('D7FF'), Code).
% #xF900-#xFDCF
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('F900'), hex('FDCF'), Code).
% #xFDF0-#xFFFD
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('FDF0'), hex('FFFD'), Code).
% #x10000-#xEFFFF
'PN_CHARS_BASE'(Code) -->
  between_code_radix(hex('10000'), hex('EFFFF'), Code).



%! 'PN_CHARS_U'(?Language:oneof([nquads,ntriples,sparql,turtle]), ?Code:code)// .
% ```bnf
% [N-Quads,N-Triples]   PN_CHARS_U ::= PN_CHARS_BASE | '_' | ':'
% [Turtle,SPARQL]       PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ```
%
% @compat N-Quads [158s].
% @compat N-Triples [158s].
% @compat SPARQL 1.0 [96].
% @compat SPARQL 1.1 Query [165].
% @compat Turtle 1.1 [164s].

'PN_CHARS_U'(_, Code) -->
  'PN_CHARS_BASE'(Code).
'PN_CHARS_U'(_, Code) -->
  underscore(Code).
'PN_CHARS_U'(Lang, Code) -->
  {member(Lang, [nquads,ntriples])},
  colon(Code).



%! 'PN_LOCAL_ESC'(?Code:code)// .
% ```bnf
% PN_LOCAL_ESC ::= '\'
%                  ( '_' | '~' | '.' | '-' | '!' | '$' | '&' |
%                    "'" | '(' | ')' | '*' | '+' | ',' | ';' |
%                    '=' | '/' | '?' | '#' | '@' | '%'
%                  )
% ```
%
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



%! 'UCHAR'(?Code:nonneg)// .
% ```ebnf
% UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ```
%
% @compat N-Quads 1.1 [12].
% @compat N-Triples 1.1 [10].
% @compat Turtle 1.1 [26].

'UCHAR'(Code) -->
  "\\u",
  '#'(4, 'HEX', Weights, []),
  {weights_radix(Weights, Code)}.
'UCHAR'(Code) -->
  "\\U",
  '#'(8, 'HEX', Weights, []),
  {weights_radix(Weights, Code)}.



%! white_space(?Language:oneof([manchester,n]))// .
% White space is a sequence of:
%   - N-Quads 1.1, N-Triples 1.1
%   - OWL 2 Web Ontology Language Manchester Syntax (Second Edition):
%     - blanks (U+20)
%     - tabs (U+9)
%     - line feeds (U+A)
%     - carriage returns (U+D)
%     - comments
%
% @compat N-Triples 1.1
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

white_space(manchester) --> 'WS'.
white_space(manchester) --> comment.
white_space(n) --> horizontal_tab.
white_space(n) --> space.



%! 'WS'// .
% ```bnf
% WS ::= #x20 | #x9 | #xD | #xA
%        /* #x20=space #x9=character tabulation
%           #xD=carriage return #xA=new line */
% ```
%
% @compat SPARQL 1.0 [93].
% @compat SPARQL 1.1 Query [162].
% @compat Turtle 1.1 [161s].

'WS' --> space.
'WS' --> horizontal_tab.
'WS' --> carriage_return.
'WS' --> line_feed.



'WS*' -->
  'WS',
  'WS*'.
'WS*' --> [].
