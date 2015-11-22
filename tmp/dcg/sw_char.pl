:- module(
  sw_char,
  [
    'ECHAR'//1, % ?Code:code
    'EOL'//0,
    'PERCENT'//1, % ?Code:code
    'PLX'//1, % ?Code:code
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
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(lists)).
:- use_module(library(math/positional)).
:- use_module(library(url/rfc1738_code), [escape//1 as 'PERCENT']).





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

'ECHAR0'(0'\t) --> "\t".
'ECHAR0'(0'\b) --> "\b".
'ECHAR0'(0'\n) --> "\n".
'ECHAR0'(0'\r) --> "\r".
'ECHAR0'(0'\f) --> "\f".
'ECHAR0'(0'") --> "\"".
'ECHAR0'(0'') --> "'".
'ECHAR0'(0'\\) --> "\\".



%! 'EOL'// .
%! 'EOL'(?Codes:list(code))// .
% ```abnf
% EOL ::= [#xD#xA]+
% ```
%
% @compat N-Quads [8].
% @compat N-Triples [7].

'EOL' --> 'EOL'(_).
'EOL'(L) --> +(eol_code, L, []).
eol_code(C) --> 'CR'(C).
eol_code(C) --> 'LF'(C).



%! 'PERCENT'(?Code:code)// .
% RFC 1738 (URL) defines the same thing under the name escape//1.
%
% ```bnf
% PERCENT ::= '%' HEX HEX
% ```
%
% @compat SPARQL Query 1.1 [171].
% @compat Turtle 1.1 [170s].



%! 'PLX'(?Code:code)// .
% ```bnf
% PLX ::= PERCENT | PN_LOCAL_ESC
% ```
%
% @compat SPARQL 1.1 Query [170].
% @compat Turtle 1.1 [169s].

'PLX'(C) --> 'PERCENT'(C).
'PLX'(C) --> 'PN_LOCAL_ESC'(C).



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

'PN_LOCAL_ESC'(C) --> "\\", pn_local_esc_code(C).
pn_local_esc_code(0'_) --> "_".
pn_local_esc_code(0'~) --> "~".
pn_local_esc_code(0'.) --> ".".
pn_local_esc_code(0'-) --> "-".
pn_local_esc_code(0'!) --> "!".
pn_local_esc_code(0'$) --> "$".
pn_local_esc_code(0'&) --> "&".
pn_local_esc_code(0'') --> "'".
pn_local_esc_code(0'() --> "(".
pn_local_esc_code(0')) --> ")".
pn_local_esc_code(0'*) --> "*".
pn_local_esc_code(0'+) --> "+".
pn_local_esc_code(0',) --> ",".
pn_local_esc_code(0';) --> ";".
pn_local_esc_code(0'=) --> "=".
pn_local_esc_code(0'/) --> "/".
pn_local_esc_code(0'?) --> "?".
pn_local_esc_code(0'#) --> "#".
pn_local_esc_code(0'@) --> "@".
pn_local_esc_code(0'%) --> "%".



%! 'UCHAR'(?Code:code)// .
% ```ebnf
% UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
% ```
%
% @compat N-Quads 1.1 [12].
% @compat N-Triples 1.1 [10].
% @compat Turtle 1.1 [26].

'UCHAR'(C) --> "\\u", '#'(4, 'HEX', C, [convert1(positional)]).
'UCHAR'(C) --> "\\U", '#'(8, 'HEX', C, [convert1(positional)]).



%! white_space// .
%! white_space(?Codes:list(code))// .
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

white_space --> white_space(_).
white_space([C]) --> 'WS'(C).
white_space(Cs) --> comment(Cs).
white_space([0'\t]) --> "\t".
white_space([0' ]) --> " ".



%! 'WS'// .
%! 'WS'(?Code:code)// .
% ```bnf
% WS ::= #x20 | #x9 | #xD | #xA
%        /* #x20=space #x9=character tabulation
%           #xD=carriage return #xA=new line */
% ```
%
% @compat SPARQL 1.0 [93].
% @compat SPARQL 1.1 Query [162].
% @compat Turtle 1.1 [161s].

'WS' --> 'WS'(_).
'WS'(C) --> code_radix(hex('20'), C).
'WS'(C) --> code_radix(hex('9'), C).
'WS'(C) --> code_radix(hex('D'), C).
'WS'(C) --> code_radix(hex('A'), C).
