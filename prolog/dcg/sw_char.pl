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
    'WS'//0
  ]
).

/** <module> Character definitions in Semantic Web grammars

Grammar rules for characters in SW standards.

Turtle characters are a superset of SPARQL characters.

---

@author Wouter Beek
@compat N-Quads 1.1
@compat N-Triples 1.1
@compat SPARQL 1.0
@compat SPARQL 1.1 Query
@compat Turtle 1.1
@version 2015/08
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_abnf_rules)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(lists)).
:- use_module(library(math/positional)).





% comment// .
% Comments are maximal sequences of Unicode characters starting with a `#`
%  and not containing a line feed or a carriage return.
% Note that comments are only recognized where white space is allowed,
%  and thus not inside the above non-terminals.
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)

comment --> "#", string(_), 'EOL0'.



%! 'ECHAR'(?Code:code)// .
% ```bnf
% ECHAR ::= '\' [tbnrf"'\]
% ```
%
% @compat SPARQL 1.0 [91].
% @compat SPARQL 1.1 Query [160].
% @compat Turtle 1.1 [159s].

'ECHAR'(C) --> "\\", 'ECHAR0'(C).

'ECHAR0'(C) --> "t", {horizontal_tab(C, _, _)}.
'ECHAR0'(C) --> "b", {bell(C, _, _)}.
'ECHAR0'(C) --> "n", {line_feed(C, _, _)}.
'ECHAR0'(C) --> "r", {carriage_return(C, _, _)}.
'ECHAR0'(C) --> "f", {form_feed(C, _, _)}.
'ECHAR0'(C) --> double_quote(C).
'ECHAR0'(C) --> apostrophe(C).
'ECHAR0'(C) --> backslash(C).



%! 'EOL'// .
% ```abnf
% EOL ::= [#xD#xA]+
% ```
%
% @compat N-Quads [8].
% @compat N-Triples [7].

'EOL' --> '+'('EOL0', []).

'EOL0' --> carriage_return.
'EOL0' --> line_feed.



%! 'PERCENT'(?Code:code)// .
% ```bnf
% PERCENT ::= '%' HEX HEX
% ```
%
% @compat SPARQL Query 1.1 [171].
% @compat Turtle 1.1 [170s].

'PERCENT'(C) -->
  {var(C)}, !,
  'PERCENT'(C1, C2),
  {C is C1 * 16 + C2}.
'PERCENT'(C) -->
  {between(0, 256, C)},
  {C1 is C // 16},
  {C2 is C rem 16},
  'PERCENT'(C1, C2).

'PERCENT'(C1, C2) --> "%", 'HEX'(C1), 'HEX'(C2).



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

'PN_CHARS'(Lang, C) --> 'PN_CHARS_U'(Lang, C).
'PN_CHARS'(_,    C) --> hyphen_minus(C).
'PN_CHARS'(_,    C) --> decimal_digit(_, C).
'PN_CHARS'(_,    C) --> code_radix(hex('00B7'), C).
'PN_CHARS'(_,    C) --> between_code_radix(hex('0300'), hex('036F'), C).
'PN_CHARS'(_,    C) --> between_code_radix(hex('203F'), hex('2040'), C).



%! 'PN_CHARS_BASE'(?Code:code)// .
% ```bnf
% PN_CHARS_BASE ::= [A-Z]
%                 | [a-z]
%                 | [#xC0-#xD6]
%                 | [#xD8-#xF6]
%                 | [#xF8-#x2FF]
%                 | [#x370-#x37D]
%                 | [#x37F-#x1FFF]
%                 | [#x200C-#x200D]
%                 | [#x2070-#x218F]
%                 | [#x2C00-#x2FEF]
%                 | [#x3001-#xD7FF]
%                 | [#xF900-#xFDCF]
%                 | [#xFDF0-#xFFFD]
%                 | [#x10000-#xEFFFF]
% ```
%
% @compat N-Quads 1.1 [157s].
% @compat SPARQL 1.0 [95].
% @compat SPARQL 1.1 Query [164].
% @compat Turtle 1.1 [163s].
% @see Almost the same as XML 1.0.5 and 1.1.2,
%      but without colon and underscore.

% [A-Z] and [a-z]
'PN_CHARS_BASE'(C) --> ascii_letter(C).
% #xC0-#xD6
'PN_CHARS_BASE'(C) --> between_code_radix(hex('C0'), hex('D6'), C).
% #xD8-#xF6
'PN_CHARS_BASE'(C) --> between_code_radix(hex('D8'), hex('F6'), C).
% #xF8-#x2FF
'PN_CHARS_BASE'(C) --> between_code_radix(hex('F8'), hex('2FF'), C).
% #x370-#x37D
'PN_CHARS_BASE'(C) --> between_code_radix(hex('370'), hex('37D'), C).
% #x37F-#x1FFF
'PN_CHARS_BASE'(C) --> between_code_radix(hex('37F'), hex('1FFF'), C).
% #x200C-#x200D
'PN_CHARS_BASE'(C) --> zero_width_non_joiner(C).
'PN_CHARS_BASE'(C) --> zero_width_joiner(C).
% #x2070-#x218F
'PN_CHARS_BASE'(C) --> between_code_radix(hex('2070'), hex('218F'), C).
% #x2C00-#x2FEF
'PN_CHARS_BASE'(C) --> between_code_radix(hex('2C00'), hex('2FEF'), C).
% #x3001-#xD7FF
'PN_CHARS_BASE'(C) --> between_code_radix(hex('3001'), hex('D7FF'), C).
% #xF900-#xFDCF
'PN_CHARS_BASE'(C) --> between_code_radix(hex('F900'), hex('FDCF'), C).
% #xFDF0-#xFFFD
'PN_CHARS_BASE'(C) --> between_code_radix(hex('FDF0'), hex('FFFD'), C).
% #x10000-#xEFFFF
'PN_CHARS_BASE'(C) --> between_code_radix(hex('10000'), hex('EFFFF'), C).



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

'PN_CHARS_U'(_, C) --> 'PN_CHARS_BASE'(C).
'PN_CHARS_U'(_, C) --> underscore(C).
'PN_CHARS_U'(Lang, C) --> {member(Lang, [nquads,ntriples])}, colon(C).



%! 'PN_LOCAL_ESC'(?Code:code)// .
% ```bnf
% PN_LOCAL_ESC ::= '\'
%                  ( '_' | '~' | '.' | '-' | '!' | '$' | '&' |
%                    "'" | '(' | ')' | * | '+' | ',' | ';' |
%                    '=' | '/' | '?' | '#' | '@' | '%'
%                  )
% ```
%
% @compat SPARQL 1.1 Query [173].
% @compat Turtle 1.1 [172s].

'PN_LOCAL_ESC'(C) --> "\\", 'PN_LOCAL_ESC_char'(C).

'PN_LOCAL_ESC_char'(C) --> underscore(C).
'PN_LOCAL_ESC_char'(C) --> tilde(C).
'PN_LOCAL_ESC_char'(C) --> dot(C).
'PN_LOCAL_ESC_char'(C) --> hyphen_minus(C).
'PN_LOCAL_ESC_char'(C) --> exclamation_mark(C).
'PN_LOCAL_ESC_char'(C) --> dollar_sign(C).
'PN_LOCAL_ESC_char'(C) --> ampersand(C).
'PN_LOCAL_ESC_char'(C) --> apostrophe(C).
'PN_LOCAL_ESC_char'(C) --> opening_round_bracket(C).
'PN_LOCAL_ESC_char'(C) --> closing_round_bracket(C).
'PN_LOCAL_ESC_char'(C) --> asterisk(C).
'PN_LOCAL_ESC_char'(C) --> plus_sign(C).
'PN_LOCAL_ESC_char'(C) --> comma(C).
'PN_LOCAL_ESC_char'(C) --> semi_colon(C).
'PN_LOCAL_ESC_char'(C) --> equals_sign(C).
'PN_LOCAL_ESC_char'(C) --> slash(C).
'PN_LOCAL_ESC_char'(C) --> question_mark(C).
'PN_LOCAL_ESC_char'(C) --> number_sign(C).
'PN_LOCAL_ESC_char'(C) --> at_sign(C).
'PN_LOCAL_ESC_char'(C) --> percent_sign(C).



%! 'UCHAR'(?Code:code)// .
% ```ebnf
% UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ```
%
% @compat N-Quads 1.1 [12].
% @compat N-Triples 1.1 [10].
% @compat Turtle 1.1 [26].

'UCHAR'(C) --> "\\u", '#'(4, 'HEX', Ws, []), {positional(C, Ws)}.
'UCHAR'(C) --> "\\U", '#'(8, 'HEX', Ws, []), {positional(C, Ws)}.



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
