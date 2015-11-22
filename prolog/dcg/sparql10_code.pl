:- module(
  sparql10_code,
  [
    'ECHAR'//1, % ?Code:code
    'PN_CHARS'//1, % ?Code:code
    'PN_CHARS_BASE'//1, % ?Code:code
    'PN_CHARS_U'//1 % ?Code:code
  ]
).

/** <module> SPARQL Query Language for RDF

@author Wouter Beek
@compat SPARQL 1.1 Query
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(lists)).





%! 'ECHAR'(?Code:code)// .
% ```bnf
% ECHAR ::= '\' [tbnrf"'\]
% ```

'ECHAR'(C) --> "\\", echar_code(C).
echar_code(0'\t) --> "\t".
echar_code(0'\b) --> "\b".
echar_code(0'\n) --> "\n".
echar_code(0'\r) --> "\r".
echar_code(0'\f) --> "\f".
echar_code(0'")  --> "\"".   %"
echar_code(0'')  --> "'".
echar_code(0'\\) --> "\\".



%! 'PN_CHARS'(?Code:code)// .
% ```
% PN_CHARS ::= PN_CHARS_U
%            | '-'
%            | [0-9]
%            | #x00B7
%            | [#x300-#x36F]
%            | [#x203F-#x2040]
% ```

'PN_CHARS'(C)      --> 'PN_CHARS_U'(C).
'PN_CHARS'(0'-)    --> "-".
'PN_CHARS'(C)      --> digit(C).
'PN_CHARS'(0x00BF) --> [0x00B7].
'PN_CHARS'(C)      --> [C], {between(0x0300, 0x036F, C)}.
'PN_CHARS'(C)      --> [C], {between(0x203F, 0x2040, C)}.



%! 'PN_CHARS_BASE'(?Code:code)// .
% ```
% PN_CHARS_BASE ::= [A-Z]
%                 | [a-z]
%                 | [#x00C0-#x00D6]
%                 | [#x00D8-#x00F6]
%                 | [#x00F8-#x02FF]
%                 | [#x0370-#x037D]
%                 | [#x037F-#x1FFF]
%                 | [#x200C-#x200D]
%                 | [#x2070-#x218F]
%                 | [#x2C00-#x2FEF]
%                 | [#x3001-#xD7FF]
%                 | [#xF900-#xFDCF]
%                 | [#xFDF0-#xFFFD]
%                 | [#x10000-#xEFFFF]
% ```
%
% @see Almost the same as XML 1.0.5 and 1.1.2,
%      but without colon and underscore.

'PN_CHARS_BASE'(C) --> ascii_alpha(C).
'PN_CHARS_BASE'(C) --> [C], {between(0x00C0,  0x00D6,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x00D8,  0x00F6,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x00F8,  0x02FF,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x0370,  0x037D,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x037F,  0x1FFF,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x200C,  0x200D,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x2070,  0x218F,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x2C00,  0x2FEF,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x3001,  0xD7FF,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0xF900,  0xFDCF,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0xFDF0,  0xFFFD,  C).
'PN_CHARS_BASE'(C) --> [C], {between(0x10000, 0xEFFFF, C).



%! 'PN_CHARS_U'(?Code:code)// .
% ```
% PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ```
%
% Different from N-Quads and N-Triples where the colon is included as well.

'PN_CHARS_U'(C)   --> 'PN_CHARS_BASE'(C).
'PN_CHARS_U'(0'_) --> "_".



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

'WS' --> [0x20].
'WS' --> [0x9].
'WS' --> [0xD].
'WS' --> [0xA].
